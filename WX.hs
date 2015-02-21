module Main where

import Graphics.UI.WX hiding (Layout, Button, button, widget, textEntry)
import qualified Graphics.UI.WX as WX
import Data.IORef
import qualified Doc                as D
import           Doc3               (DocF(Doc), DocP(DocP), DocR,
                                     ReadMessage(ReadMessage), contains, also)
import qualified Doc3               as D3
import           Control.Monad      (guard)
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Monoid        as DM
import qualified Data.List.NonEmpty as NEL
import qualified Radio              as R
import qualified Data.Text.Lazy     as T

data Button = Button String

data TextEntry = TextEntry String

buttonD :: Button -> D.Doc Layout ()
buttonD (Button t) = D.Doc $ do
  n <- D.unique
  return (ButtonL t n, parseMessage n)
  where parseMessage n message = guard (message == n)

button :: Button -> DocR () Button Layout
button b = (Doc
            . DocP
            . fmap (\(d, m) -> (ReadMessage (\message -> case m message of
                                                Nothing -> return b
                                                Just () -> do
                                                  W.tell (DM.First (Just ()))
                                                  return b), d))
            . D.unDoc
            . buttonD) b

textEntryD :: TextEntry -> D.Doc Layout ()
textEntryD (TextEntry t) = D.Doc $ do
  n <- D.unique
  return (TextEntryL t n, parseMessage n)
  where parseMessage n message = guard (message == n)

textEntry :: TextEntry -> DocR () TextEntry Layout
textEntry te = (Doc . DocP
                . fmap (\(d, m) -> (ReadMessage (\message -> case m message of
                                                    Nothing -> return te
                                                    Just () -> do
                                                      W.tell (DM.First (Just ()))
                                                      return te), d))
            . D.unDoc
            . textEntryD) te

list :: (s -> DocR e s l) -> NEL.NonEmpty s
     -> DocR e (NEL.NonEmpty s) (NEL.NonEmpty l)
list w = R.sequenceNEL2 D3.pair D3.mapBD . fmap w

main :: IO ()
main = start hello

runWX :: (t -> DocR t1 t Layout) -> t -> IO ()
runWX widget initial = do
  f <- frame [text := "Hello!"]
  handler <- newIORef (\_ -> close f)

  let loop state = do
        let D3.Doc (D3.DocP u) = widget state
            (D3.ReadMessage mab, d) = D.runUS u

        (wipe, l) <- renderLayout handler f d
        set f [layout := l]
        writeIORef handler (\x -> do
                               let (state', _) = W.runWriter (mab x)
                               wipe
                               loop state'
                               )
  loop initial

hello :: IO ()
hello = let widget = \(x1, y1) ->
             ((,), \x y -> Column 1 [x, y])
             `contains` (fmap (D3.mapDoc (Row 1 . NEL.toList)) (list button) x1)
             `also` (fmap (D3.mapDoc (Row 1 . NEL.toList)) (list textEntry) y1)

            initial = (Button "Hello" NEL.:| [Button "Something", Button "Else"],
                      TextEntry "Foo" NEL.:| [])

            in runWX widget initial

data Layout = ButtonL String T.Text
            | TextEntryL String T.Text
            | Row Int [Layout]
            | Column Int [Layout]

renderLayout :: IORef (D.Message -> IO ()) -> Frame () -> Layout -> IO (IO (), WX.Layout)
renderLayout handlerRef f (ButtonL t ht) = do
  b <- WX.button f [text := t, on command := do
                       handler <- readIORef handlerRef
                       handler ht
                   ]
  return (set b [visible := False], WX.widget b)

renderLayout handlerRef f (TextEntryL t ht) = do
  b <- WX.textEntry f [text := t, on command := do
                          handler <- readIORef handlerRef
                          handler ht
                      ]
  return (set b [visible := False], WX.widget b)

renderLayout handlerRef f (Row i l) = do
  ls <- mapM (renderLayout handlerRef f) l
  return ((mapM_ fst) ls, row i (map snd ls))

renderLayout handlerRef f (Column i l) = do
  ls <- mapM (renderLayout handlerRef f) l
  return ((mapM_ fst) ls, column i (map snd ls))
