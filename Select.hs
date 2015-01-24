{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Select where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import qualified Radio              as R
import           Control.Monad      (guard)
import           Control.Applicative ((<*>))
import qualified Data.List.NonEmpty as NEL
import           Text.Read          (readMaybe)

data SelectEvent = Choice Int
data Select = Select { _bRadio :: R.Radio T.Text T.Text }
$(L.makeLenses ''Select)

parseSelectEvent :: T.Text -> Maybe (Int -> SelectEvent)
parseSelectEvent = \case "choose" -> Just Choice
                         _        -> Nothing

selectMake :: NEL.NonEmpty T.Text -> Select
selectMake l = Select { _bRadio = R.chooseFirstNEL l }

selectHandle :: SelectEvent -> Select -> Select
selectHandle (Choice n) = L.over bRadio (R.chooseIndex n)

guiSelect :: T.Text -> Select -> D.GUISelect
guiSelect n s = D.GUISelect n (_bRadio s)

-- TODO: duplication with circle
select :: Select -> D.Doc [D.Element] SelectEvent
select b = D.Doc $ do
  n <- D.unique
  return ([D.Select (guiSelect n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent, theValue] -> do
                                      guard (theName == n)
                                      parseSelectEvent theEvent
                                          <*> (readMaybe (T.unpack theValue)
                                               :: Maybe Int)
                                    _ -> Nothing

selectC :: Select -> D.Doc [D.Element] (SelectEvent, Select)
selectC = D.widgetHandler selectHandle select
