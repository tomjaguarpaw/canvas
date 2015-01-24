{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module TextEntry where

-- TextEntrys don't keep focus when the new page state is loaded

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D

data TextEntryEvent = Input T.Text
data TextEntry = TextEntry { _tText :: T.Text }
$(L.makeLenses ''TextEntry)

parseTextEntryEvent :: T.Text -> Maybe (T.Text -> TextEntryEvent)
parseTextEntryEvent = \case "input" -> Just Input
                            _       -> Nothing

textEntryMake :: T.Text -> TextEntry
textEntryMake t = TextEntry { _tText = t }

textEntryHandle :: TextEntryEvent -> TextEntry -> TextEntry
textEntryHandle (Input n) = L.set tText n

guiTextEntry :: T.Text -> TextEntry -> D.GUITextEntry
guiTextEntry n b = D.GUITextEntry n (_tText b)

-- FIXME: duplication with circle, textEntry
textEntry :: TextEntry -> D.Doc [D.Element] TextEntryEvent
textEntry b = D.Doc $ do
  n <- D.unique
  return ([D.TextEntry (guiTextEntry n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent, theValue] ->
                                      if theName == n
                                      then parseTextEntryEvent theEvent <$$> theValue
                                      else Nothing
                                    _ -> Nothing
        f <$$> x = fmap ($ x) f

textEntryC :: TextEntry -> D.Doc [D.Element] (TextEntryEvent, TextEntry)
textEntryC = D.widgetHandler textEntryHandle textEntry
