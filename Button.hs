{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Button where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import qualified Html               as H
import           Control.Monad      (guard)

data ButtonEvent = MouseClick
data Button = Button { _bText :: T.Text } deriving Show
$(L.makeLenses ''Button)

parseButtonEvent :: T.Text -> Maybe ButtonEvent
parseButtonEvent = \case "click" -> Just MouseClick
                         _       -> Nothing

buttonMake :: T.Text -> Button
buttonMake t = Button { _bText = t }

buttonHandle :: ButtonEvent -> Button -> Button
buttonHandle _ = id

guiButton :: T.Text -> Button -> H.GUIButton
guiButton n b = H.GUIButton n (_bText b)

-- TODO: duplication with circle
button :: Button -> D.Doc [H.Element] ButtonEvent
button b = D.Doc $ do
  n <- D.unique
  return ([H.Button (guiButton n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent] -> do
                                      guard (theName == n)
                                      parseButtonEvent theEvent
                                    _ -> Nothing

buttonC :: Button -> D.Doc [H.Element] (ButtonEvent, Button)
buttonC = D.widgetHandler buttonHandle button
