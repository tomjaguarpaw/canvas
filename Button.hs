{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Button where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D

data ButtonEvent = MouseClick
data Button = Button { _bName :: T.Text
                     , _bText :: T.Text }
$(L.makeLenses ''Button)

parseButtonEvent :: T.Text -> Maybe ButtonEvent
parseButtonEvent = \case "click" -> Just MouseClick
                         _       -> Nothing

buttonMake :: T.Text -> T.Text -> Button
buttonMake t n = Button { _bName = n, _bText = t }

buttonHandle :: ButtonEvent -> Button -> Button
buttonHandle _ = id

guiButton :: Button -> D.GUIButton
guiButton b = D.GUIButton (_bName b) (_bText b)

-- TODO: duplication with circle
button :: Button -> D.Doc [D.Element] ButtonEvent
button b = D.Doc [D.Button (guiButton b)] parseMessage
  where parseMessage message = case T.split (== ',') message
                               of [theName, theEvent] ->
                                    if theName == (_bName b)
                                    then parseButtonEvent theEvent
                                    else Nothing
                                  _ -> Nothing

buttonC :: Button -> D.Doc [D.Element] (ButtonEvent, Button)
buttonC b = fmap (\ev -> (ev, buttonHandle ev b)) (button b)
