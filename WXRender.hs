{-# LANGUAGE LambdaCase #-}

module WXRender where

import qualified Doc                as D
import qualified Doc3               as D3
import           Doc3               (DocR)
import           Graphics.UI.WX hiding (Layout, Button, button, widget,
                                        textEntry)
import qualified Graphics.UI.WX as WX
import           Data.IORef
import qualified Focus              as Focus
import qualified Data.Dynamic       as Dyn
import qualified Graphics.UI.WXCore.WxcClassesMZ as MZ

runWX :: (state -> DocR ev state Layout) -> state -> IO ()
runWX widget initial = WX.start $ do
  f <- frame [text := "Hello!"]
  handler <- newIORef (\_ -> return ())

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
              let (newText, newPosition) = case k of
                    KeyChar c -> (oldText ++ [c], oldPosition + 1)
                    KeySpace  -> (oldText ++ " ", oldPosition + 1)
                    KeyBack   -> if oldPosition == 0
                                 then (oldText, oldPosition)
                                 else (let (a, b) = splitAt oldPosition oldText
                                       in init a ++ b, oldPosition - 1)
                    _         -> (oldText, oldPosition)
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
