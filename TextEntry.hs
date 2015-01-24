{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module TextEntry where

-- TextEntrys don't keep focus when the new page state is loaded

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import           Text.Read          (readMaybe)
import           Control.Applicative ((<*>), pure)

data TextEntryEvent = Input T.Text Int
data TextEntry = TextEntry { _tText     :: T.Text
                           , _tFocused  :: Bool
                           , _tPosition :: Int }
$(L.makeLenses ''TextEntry)

parseTextEntryEvent :: T.Text -> Maybe (T.Text -> Int -> TextEntryEvent)
parseTextEntryEvent = \case "input" -> Just Input
                            _       -> Nothing

textEntryMake :: T.Text -> TextEntry
textEntryMake t = TextEntry { _tText = t, _tFocused = False, _tPosition = 0 }

textEntryHandle :: TextEntryEvent -> TextEntry -> TextEntry
textEntryHandle (Input n p) _ = TextEntry { _tText    = n
                                          , _tFocused = True
                                          , _tPosition = p }

guiTextEntry :: T.Text -> TextEntry -> D.GUITextEntry
guiTextEntry n b = D.GUITextEntry { D.gtName     = n
                                  , D.gtText     = _tText b
                                  , D.gtFocused  = _tFocused b
                                  , D.gtPosition = _tPosition b }

-- FIXME: duplication with circle, textEntry
textEntry :: TextEntry -> D.Doc [D.Element] TextEntryEvent
textEntry b = D.Doc $ do
  n <- D.unique
  return ([D.TextEntry (guiTextEntry n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent, theValue, pos] ->
                                      if theName == n
                                      then parseTextEntryEvent theEvent
                                           <*> pure theValue
                                           <*> (readMaybe (T.unpack pos) :: Maybe Int)
                                      else Nothing
                                    _ -> Nothing

textEntryC :: TextEntry -> D.Doc [D.Element] (TextEntryEvent, TextEntry)
textEntryC = D.widgetHandler textEntryHandle textEntry
