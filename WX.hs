{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.UI.WX hiding (Layout, Button, button, widget, textEntry)
import qualified Graphics.UI.WX as WX
import Data.IORef
import qualified Doc                as D
import           Doc3               (DocR, contains, also, emitting)
import qualified Doc3               as D3
import           Control.Monad      (guard)
import qualified Graphics.UI.WXCore.WxcClassesMZ as MZ
import qualified Data.Dynamic       as Dyn
import qualified Control.Lens       as L
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import qualified Focus              as Focus

data Button = Button { _bText    :: String
                     , _bFocused :: Bool }
$(L.makeLenses ''Button)

data TextEntry = TextEntry { _teText     :: String
                           , _teFocused  :: Bool
                           , _tePosition :: Int }
$(L.makeLenses ''TextEntry)

buttonD :: Button -> D.DocF D3.Message () Layout
buttonD (Button t f) = D.Doc $ do
  n <- D.uniqueInt
  return (ButtonL t f n, parseMessage n)
  where parseMessage n message = do
          let (mn, _) = message
          guard (n == mn)

mkButton :: String -> Button
mkButton = flip Button False

mkTextEntry :: String -> TextEntry
mkTextEntry s = TextEntry s False 0

button :: Button -> DocR () Button Layout
button = D3.makeDoc (\b -> \case Nothing -> if L.view bFocused b
                                            then WantFocus b
                                                 (L.set bFocused False b)
                                            else Don'tWantFocus b
                                 Just t  -> NeedFocus
                                            t
                                            (L.set bFocused True b)
                                 )
                    buttonD

textEntryD :: TextEntry -> D.DocF D3.Message (String, Int) Layout
textEntryD (TextEntry t f p) = D.Doc $ do
  n <- D.uniqueInt
  return (TextEntryL t f p n, parseMessage n)
  where parseMessage n message = do
          let (mn, d) = message
          guard (n == mn)
          Dyn.fromDynamic d

textEntry :: TextEntry -> DocR (String, Int) TextEntry Layout
textEntry = D3.makeDoc (\te -> \case Nothing -> if L.view teFocused te
                                                then WantFocus te
                                                     (L.set teFocused False te)
                                                else Don'tWantFocus te
                                     Just s -> NeedFocus
                                               s
                                               ((L.set teFocused True
                                                . L.set teText (fst s)
                                                . L.set tePosition (snd s)) te)
                                     )
                    textEntryD

-- Just like sequence :: Applicative f => [f a] -> f [a]
sequence2 :: (forall a b a' b'. f a a' -> f b b' -> f (a, b) (a', b'))
          -> (forall a b a' b'. (a -> b) -> (a' -> b') -> f a a' -> f b b')
          -> (forall a b. a -> b -> f a b)
          -> [f c d]
          -> f [c] [d]
sequence2 _     _     bipure []     = bipure [] []
sequence2 (***) bimap bipure (x:xs) = bimap (uncurry (:)) (uncurry (:))
                                            (x *** sequence2 (***) bimap bipure xs)

list :: (s -> DocR e s l) -> [s]
     -> DocR e [s] [l]
list w = sequence2 D3.pair D3.mapBD D3.bipure . fmap w

main :: IO ()
main = start hello

runWX :: (state -> DocR ev state Layout) -> state -> IO ()
runWX widget initial = do
  f <- frame [text := "Hello!"]
  handler <- newIORef (\_ -> close f)

  let loop state = do
        let D3.Doc (D3.DocP u) = widget state
            (D3.ReadMessage mab, d) = D.runUS u

        (wipe, l) <- renderLayout handler f d
        set f [layout := l]
        writeIORef handler (\x -> do
                               let state' = Focus.mostFocused (mab x)
                               wipe
                               loop state'
                               )
  loop initial

hello :: IO ()
hello = let widget = \(x1, y1) ->
             ((,), \x y -> Column 1 (map (Row 1) [x, y]))
             `contains` (list button x1)
             `also` (list textEntry y1 `emitting` const ())

            initial = ([mkButton "Hello", mkButton "Something", mkButton "Else"],
                       [mkTextEntry "Foo"])

            in runWX widget initial

data Layout = ButtonL String Bool Int
            | TextEntryL String Bool Int Int
            | Row Int [Layout]
            | Column Int [Layout]

renderLayout :: IORef (D3.Message -> IO ()) -> Frame () -> Layout -> IO (IO (), WX.Layout)
renderLayout handlerRef f = \case
  ButtonL t focused ht -> do
    b <- WX.button f [text := t, on command := do
                         handler <- readIORef handlerRef
                         handler (ht, Dyn.toDyn ())
                     ]
    when focused (MZ.windowSetFocus b)
    return (set b [visible := False], WX.widget b)

  TextEntryL t focused p ht ->  do
    b <- WX.textEntry f [text := t]
    set b [on anyKey := \k -> do
              oldText <- get b text
              oldPosition <- MZ.textCtrlGetInsertionPoint b
              let newText = case k of KeyChar c -> oldText ++ [c]
                                      _         -> oldText
              let newPosition = oldPosition + 1
              print newPosition
              
              handler <- readIORef handlerRef
              handler (ht, Dyn.toDyn (newText, newPosition))
          ]
    when focused (MZ.windowSetFocus b)
    MZ.textCtrlSetInsertionPoint b p
    return (set b [visible := False], WX.widget b)

  Row i l -> do
    ls <- mapM (renderLayout handlerRef f) l
    return ((mapM_ fst) ls, row i (map snd ls))

  Column i l -> do
    ls <- mapM (renderLayout handlerRef f) l
    return ((mapM_ fst) ls, column i (map snd ls))
