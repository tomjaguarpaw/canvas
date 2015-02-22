{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Doc                as D
import           Doc3               (DocR, emitting, ($$$), (***))
import qualified Doc3               as D3
import           Control.Monad      (guard)
import qualified Data.Dynamic       as Dyn
import qualified Control.Lens       as L
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import           WXRender           (Layout, runWX, Layout(Column, Row))
import qualified WXRender           as WXR
import qualified Data.Biapplicative as Bi
import qualified Bisequence         as BiS

-- State for the GUI elements

data Button = Button { _bText    :: String
                     , _bFocused :: Bool }
$(L.makeLenses ''Button)

data TextEntry = TextEntry { _teText     :: String
                           , _teFocused  :: Bool
                           , _tePosition :: Int }
$(L.makeLenses ''TextEntry)

-- Make GUI elements

mkButton :: String -> Button
mkButton = flip Button False

mkTextEntry :: String -> TextEntry
mkTextEntry s = TextEntry s False 0

-- Core event handling for GUI elements

buttonD :: Button -> D.DocF D3.Message () Layout
buttonD (Button t f) = D.Doc $ do
  n <- D.uniqueInt
  return (WXR.ButtonL t f n, parseMessage n)
  where parseMessage n message = do
          let (mn, _) = message
          guard (n == mn)

textEntryD :: TextEntry -> D.DocF D3.Message (String, Int) Layout
textEntryD (TextEntry t f p) = D.Doc $ do
  n <- D.uniqueInt
  return (WXR.TextEntryL t f p n, parseMessage n)
  where parseMessage n message = do
          let (mn, d) = message
          guard (n == mn)
          Dyn.fromDynamic d

-- State processing for GUI elements

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

-- Example of how to combine GUI elements.  Here is a list.

traverseList :: Bi.Biapplicative f
             => (a -> f b c)
             -> [a] -> f [b] [c]
traverseList w = BiS.biSequence . fmap w

list :: (state -> DocR event state' gui) -> [state]
        -> DocR event [state'] [gui]
list = traverseList

-- Run an example GUI

exampleGUI :: ([Button], [TextEntry]) -> DocR () ([Button], [TextEntry]) Layout
exampleGUI = ((,), \x y -> Column 1 (map (Row 1) [x, y]))
             $$$ (list button . fst)
             *** ((list textEntry . snd) `emitting` const ())

main :: IO ()
main = runWX gui initialState where
  gui          = exampleGUI
  initialState =  ([mkButton "Hello", mkButton "Something", mkButton "Else"],
                   [mkTextEntry "Foo"])
