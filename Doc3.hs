{-# LANGUAGE OverloadedStrings #-}

module Doc3 where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified TextEntry          as T
import qualified Doc                as D
import qualified Network.WebSockets as WS

data DocP f b d = DocP (US (f b, d))

data Doc a b d = Doc (DocP (ReadMessage (Focus a)) b d)

mapDocP :: (d -> d') -> DocP f b d -> DocP f b d'
mapDocP f (DocP u) = DocP (L.over (L.mapped.L._2) f u)

mapResponseP :: Functor f => (b -> b') -> DocP f b d -> DocP f b' d
mapResponseP f (DocP u) = DocP (L.over (L.mapped.L._1.L.mapped) f u)

pairP :: A.Applicative f => DocP f b d -> DocP f b' d' -> DocP f (b, b') (d, d')
pairP (DocP u) (DocP u') = DocP (A.liftA2 pair' u u')
  where pair' (fb, d) (fb', d') = (A.liftA2 (,) fb fb', (d, d'))

pair :: Doc a b d -> Doc a b' d' -> Doc a (b, b') (d, d')
pair (Doc dp) (Doc dp') = Doc (pairP dp dp')

mapDoc :: (d -> d') -> Doc a b d -> Doc a b d'
mapDoc f (Doc dp) = Doc (mapDocP f dp)

data ReadMessage f a = ReadMessage (D.Message -> f a)

instance Functor f => Functor (ReadMessage f) where
  fmap f (ReadMessage fr) = ReadMessage ((fmap.fmap) f fr)

instance A.Applicative f => A.Applicative (ReadMessage f) where
  pure = ReadMessage . A.pure . A.pure
  ReadMessage ff <*> ReadMessage fx = ReadMessage (A.liftA2 (A.<*>) ff fx)

data Focus a b = NeedFocus a b
               | Don'tNeedFocus a b
               | WantFocus b b
               | Don'tWantFocus b

instance Functor (Focus a) where
  fmap f (NeedFocus a b) = NeedFocus a (f b)
  fmap f (Don'tNeedFocus a b) = Don'tNeedFocus a (f b)
  fmap f (WantFocus b b') = WantFocus (f b) (f b')
  fmap f (Don'tWantFocus b) = Don'tWantFocus (f b)

instance A.Applicative (Focus a) where
  pure = Don'tWantFocus
  -- These are probably errors
  NeedFocus a f <*> NeedFocus _ x = NeedFocus a (f x)
  NeedFocus a f <*> Don'tNeedFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> NeedFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> Don'tNeedFocus _ x = Don'tNeedFocus a (f x)
  WantFocus f f' <*> WantFocus _ x = WantFocus (f x) (f' x)
  -- These are fine
  NeedFocus a f <*> WantFocus _ x = NeedFocus a (f x)
  NeedFocus a f <*> Don'tWantFocus x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> WantFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> Don'tWantFocus x = Don'tNeedFocus a (f x)

  WantFocus _ f <*> NeedFocus a x = NeedFocus a (f x)
  WantFocus f _ <*> Don'tNeedFocus a x = NeedFocus a (f x)
  WantFocus f f' <*> Don'tWantFocus x = WantFocus (f x) (f' x)

  Don'tWantFocus f <*> NeedFocus a x = NeedFocus a (f x)
  Don'tWantFocus f <*> Don'tNeedFocus a x = Don'tNeedFocus a (f x)
  Don'tWantFocus f <*> WantFocus x x' = WantFocus (f x) (f x')
  Don'tWantFocus f <*> Don'tWantFocus x = Don'tWantFocus (f x)

textEntryC :: T.TextEntry -> Doc T.TextEntryEvent T.TextEntry [D.Element]
textEntryC te = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view T.tFocused te
                                                      then WantFocus te
                                                           (L.set T.tFocused False te)
                                                      else Don'tWantFocus te
                                           Just teev -> NeedFocus
                                                        teev
                                                        (T.textEntryHandle' True
                                                         teev te)
                                       ), d))
                 . D.unDoc
                 . T.textEntry) te

two :: (T.TextEntry, T.TextEntry)
    -> Doc T.TextEntryEvent (T.TextEntry, T.TextEntry) [D.Element]
two (t1, t2) = mapDoc (uncurry (++))
               (pair (textEntryC t1) (textEntryC t2))

run' :: (d -> IO D.Message) -> (s -> Doc a s d) -> s -> IO s
run' f fd s = do
  let Doc (DocP u) = fd s
      (ReadMessage mab, d) = D.runUS u
  message <- f d
  return $ mostFocused (mab message)

mostFocused :: Focus a b -> b
mostFocused x = case x of NeedFocus _ s'      -> s'
                          Don'tNeedFocus _ s' -> s'
                          WantFocus s' _      -> s'
                          Don'tWantFocus s'   -> s'

run :: (d -> IO D.Message) -> (s -> Doc e s d) -> s -> IO void
run f fd s = do
  s' <- run' f fd s
  run f fd s'

runServer :: WS.PendingConnection -> IO ()
runServer pc = do
  conn <- WS.acceptRequest pc

  let initialGui = (T.textEntryMake "foo", T.textEntryMake "bar")

      gui = two

      handler d = do
        WS.sendTextData conn (D.renderElements' d)
        msg <- WS.receiveData conn
        print msg
        return msg

  run handler gui initialGui

main :: IO ()
main = WS.runServer "0.0.0.0" 9998 runServer
