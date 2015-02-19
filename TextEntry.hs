{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module TextEntry where

-- TextEntrys don't keep focus when the new page state is loaded

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import           Doc3               (DocF(Doc), DocP(DocP), Doc,
                                     ReadMessage(ReadMessage))
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import qualified Html               as H
import           Text.Read          (readMaybe)
import           Control.Applicative ((<*>), pure)
import           Control.Monad      (guard)

data TextEntryEvent = Input T.Text Int
data TextEntry = TextEntry { _tText     :: T.Text
                           , _tFocused  :: Bool
                           , _tPosition :: Int } deriving Show
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

guiTextEntry :: T.Text -> TextEntry -> H.GUITextEntry
guiTextEntry n b = H.GUITextEntry { H.gtName     = n
                                  , H.gtText     = _tText b
                                  , H.gtFocused  = _tFocused b
                                  , H.gtPosition = _tPosition b }

-- FIXME: duplication with circle, textEntry
textEntry :: TextEntry -> D.Doc [H.Element] TextEntryEvent
textEntry b = D.Doc $ do
  n <- D.unique
  return ([H.TextEntry (guiTextEntry n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent, theValue, pos] -> do
                                      guard (theName == n)
                                      parseTextEntryEvent theEvent
                                           <*> pure theValue
                                           <*> (readMaybe (T.unpack pos) :: Maybe Int)
                                    _ -> Nothing

textEntryC :: TextEntry -> Doc TextEntryEvent TextEntry [H.Element]
textEntryC te = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view tFocused te
                                                      then WantFocus te
                                                           (L.set tFocused False te)
                                                      else Don'tWantFocus te
                                           Just teev -> NeedFocus
                                                        teev
                                                        (textEntryHandle
                                                         teev te)
                                       ), d))
                 . D.unDoc
                 . textEntry) te
