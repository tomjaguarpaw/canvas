{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Button where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D

data ButtonEvent = MouseClick
data Button = Button { _bText :: T.Text } -- delete this field
$(L.makeLenses ''Button)

parseButtonEvent :: T.Text -> Maybe ButtonEvent
parseButtonEvent = \case "click" -> Just MouseClick
                         _       -> Nothing

buttonMake :: T.Text -> Button
buttonMake t = Button { _bText = t }

buttonHandle :: ButtonEvent -> Button -> Button
buttonHandle _ = id

guiButton :: T.Text -> Button -> D.GUIButton
guiButton n b = D.GUIButton n (_bText b)

-- TODO: duplication with circle
button :: Button -> D.Doc [D.Element] ButtonEvent
button b = D.Doc $ do
  n <- D.unique
  return ([D.Button (guiButton n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent] ->
                                      if theName == n
                                      then parseButtonEvent theEvent
                                      else Nothing
                                    _ -> Nothing

buttonC :: Button -> D.Doc [D.Element] (ButtonEvent, Button)
buttonC = D.widgetHandler buttonHandle button
