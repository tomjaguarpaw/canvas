{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Doc3 where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified TextEntry          as T
import qualified Select             as S
import qualified Doc                as D
import qualified Network.WebSockets as WS
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import qualified Filter             as F
import           Focus              (Focus(NeedFocus, Don'tNeedFocus,
                                           WantFocus, Don'tWantFocus))
import qualified Focus              as Focus

data DocP f b d = DocP (US (f b, d))

data Doc a b d = Doc (DocP (ReadMessage (Focus a)) b d)

mapDocP :: (d -> d') -> DocP f b d -> DocP f b d'
mapDocP f (DocP u) = DocP (L.over (L.mapped.L._2) f u)

mapResponseP :: Functor f => (b -> b') -> DocP f b d -> DocP f b' d
mapResponseP f (DocP u) = DocP (L.over (L.mapped.L._1.L.mapped) f u)

mapEvent :: (e -> e') -> Doc e b d -> Doc e' b d
mapEvent f (Doc (DocP us)) = Doc (DocP (L.over l f us))
  where l = (L.mapped.L._1.answer.Focus.attached)

mapBehaviour :: (b -> b') -> Doc e b d -> Doc e b' d
mapBehaviour f (Doc (DocP us)) = Doc (DocP (L.over (L.mapped.L._1.L.mapped) f us))

pairP :: A.Applicative f => DocP f b d -> DocP f b' d' -> DocP f (b, b') (d, d')
pairP (DocP u) (DocP u') = DocP (A.liftA2 pair' u u')
  where pair' (fb, d) (fb', d') = (A.liftA2 (,) fb fb', (d, d'))

pair :: Doc a b d -> Doc a b' d' -> Doc a (b, b') (d, d')
pair (Doc dp) (Doc dp') = Doc (pairP dp dp')

pairC :: (b -> Doc a b d) -> (b' -> Doc a b' d')
      -> ((b, b') -> Doc a (b, b') (d, d'))
pairC w1 w2 (b1, b2) = w1 b1 `pair` w2 b2

pairE :: (b -> Doc a b d) -> (b' -> Doc a' b' d')
      -> ((b, b') -> Doc (Either a a') (b, b') (d, d'))
pairE w1 w2 (b1, b2) = mapEvent Left (w1 b1) `pair` mapEvent Right (w2 b2)

mapDoc :: (d -> d') -> Doc a b d -> Doc a b d'
mapDoc f (Doc dp) = Doc (mapDocP f dp)

static :: b -> Doc F.Void b ()
static b = Doc (DocP (A.pure (ReadMessage (A.pure (Don'tWantFocus b)), ())))

handleEvent :: (e -> b -> b) -> Doc e b d -> Doc e b d
handleEvent h (Doc (DocP us)) = Doc (DocP (f us))
  where f = L.over (L.mapped.L._1.answer) (handleEventFocus h)

handleEventFocus :: (e -> b -> b) -> Focus e b -> Focus e b
handleEventFocus h = \case NeedFocus e b      -> NeedFocus e (h e b)
                           Don'tNeedFocus e b -> Don'tNeedFocus e (h e b)
                           WantFocus b b'     -> WantFocus b b'
                           Don'tWantFocus b   -> Don'tWantFocus b

data ReadMessage f a = ReadMessage (D.Message -> f a)

answer :: L.Setter (ReadMessage f a) (ReadMessage g b) (f a) (g b)
answer = L.sets (\f (ReadMessage g) -> ReadMessage (f . g))

instance Functor f => Functor (ReadMessage f) where
  fmap f (ReadMessage fr) = ReadMessage ((fmap.fmap) f fr)

instance A.Applicative f => A.Applicative (ReadMessage f) where
  pure = ReadMessage . A.pure . A.pure
  ReadMessage ff <*> ReadMessage fx = ReadMessage (A.liftA2 (A.<*>) ff fx)

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


selectC :: S.Select a -> Doc (S.SelectEvent a) (S.Select a) [D.Element]
selectC se = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view S.sFocused se
                                                      then WantFocus se
                                                           (L.set S.sFocused False se)
                                                      else Don'tWantFocus se
                                           Just seev -> NeedFocus
                                                        seev
                                                        (S.selectHandle
                                                         seev se)
                                       ), d))
                 . D.unDoc
                 . S.select) se

-- Should be `Getting (First a) s a`
handle :: L.Fold s a -> (a -> b -> b) -> Doc s b d -> Doc s b d
handle l f = handleEvent (\e b -> case e L.^? l of
                             Just m  -> f m b
                             Nothing -> b)

textSelectC :: (T.TextEntry, S.Select a)
            -> Doc (Either (T.TextEntryEvent) (S.SelectEvent a))
                   (T.TextEntry, S.Select a)
                   [D.Element]
textSelectC = handle L._Left (\_ b -> L.set (L._2.S.sRadio.R.chosen.L._1)
                                            (L.view (L._1.T.tText) b)
                                            b)
              . handle L._Right (\_ b -> let newText =
                                               L.view (L._2.S.sRadio.R.chosen.L._1) b
                                             newLength =
                                               (fromIntegral . DT.length) newText
                                         in (L.set (L._1.T.tText) newText
                                             . L.set (L._1.T.tPosition) newLength)
                                            b)
              . mapDoc (uncurry (++))
              . (textEntryC `pairE` selectC)


filterC :: F.Filter -> Doc F.FilterEvent F.Filter [D.Element]
filterC = handle F._EditorEvent
          (\_ a -> L.set (F.fAv.F.aAv.R.chosen)
                         (L.view (F.fEd.T.tText) a) a)
          . handle F._FilterEvent
          (\_ a -> L.set F.fSe
                         (F.selectFromAvailable (L.view F.fFi a)
                                                (L.view F.fAv a)) a)
          . handle (F._SelectEvent.S.cEv)
          (\i a -> L.over (F.fAv.F.aAv) (R.chooseIndex i) a)

          . filterA

filterA :: F.Filter -> Doc F.FilterEvent F.Filter [D.Element]
filterA = mapBehaviour (\((a, t), (tt, s)) -> F.Filter a t tt s)
          . mapEvent (either (either F.absurd F.FilterEvent)
                           (either F.EditorEvent F.SelectEvent))
          . mapDoc (\(((), d1), d2) -> d1 ++ d2)
          . (static
           `pairE` textEntryC
           `pairE` textSelectC)
          . (\(F.Filter a t tt s) -> ((a, t), (tt, s)))


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

  let initialGui = F.filterMake

      gui = filterC

      handler d = do
        WS.sendTextData conn (D.renderElements' d)
        msg <- WS.receiveData conn
        print msg
        return msg

  run handler gui initialGui

main :: IO ()
main = WS.runServer "0.0.0.0" 9998 runServer
