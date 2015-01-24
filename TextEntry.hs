{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module TextEntry where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D

data TextEntryEvent = Input T.Text
data TextEntry = TextEntry { _tName :: T.Text
                           , _tText :: T.Text }
$(L.makeLenses ''TextEntry)

parseTextEntryEvent :: T.Text -> Maybe (T.Text -> TextEntryEvent)
parseTextEntryEvent = \case "input" -> Just Input
                            _       -> Nothing

textEntryMake :: T.Text -> T.Text -> TextEntry
textEntryMake t n = TextEntry { _tName = n, _tText = t }

textEntryHandle :: TextEntryEvent -> TextEntry -> TextEntry
textEntryHandle (Input n) = L.set tText n

guiTextEntry :: TextEntry -> D.GUITextEntry
guiTextEntry b = D.GUITextEntry (_tName b) (_tText b)

-- FIXME: duplication with circle, textEntry
textEntry :: TextEntry -> D.Doc [D.Element] TextEntryEvent
textEntry b = D.Doc [D.TextEntry (guiTextEntry b)] parseMessage
  where parseMessage message = case T.split (== ',') message
                               of [theName, theEvent, theValue] ->
                                    if theName == (_tName b)
                                    then parseTextEntryEvent theEvent <$$> theValue
                                    else Nothing
                                  _ -> Nothing
        f <$$> x = fmap ($ x) f

textEntryC :: TextEntry -> D.Doc [D.Element] (TextEntryEvent, TextEntry)
textEntryC = D.widgetHandler textEntryHandle textEntry
