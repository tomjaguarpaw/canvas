{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Select where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import qualified Radio              as R
import           Control.Monad      (guard)
import qualified Data.List.NonEmpty as NEL
import           Text.Read          (readMaybe)

data SelectEvent a = Choice Int a
data Select a = Select { _sRadio :: R.Radio (T.Text, a) (T.Text, a) }
$(L.makeLenses ''Select)

parseSelectEvent :: T.Text -> Maybe (Int -> a -> SelectEvent a)
parseSelectEvent = \case "choose" -> Just Choice
                         _        -> Nothing

selectMake :: NEL.NonEmpty T.Text -> Select ()
selectMake l = Select { _sRadio = R.fmapRadio u u (R.chooseFirstNEL l) }
  where u x = (x, ())

selectHandle :: SelectEvent a -> Select a -> Select a
selectHandle (Choice n _) = L.over sRadio (R.chooseIndex n)

guiSelect :: T.Text -> Select a -> D.GUISelect
guiSelect n s = D.GUISelect n (R.fmapRadio fst fst (_sRadio s))

-- TODO: duplication with circle
select :: Select a -> D.Doc [D.Element] (SelectEvent a)
select b = D.Doc $ do
  n <- D.unique
  return ([D.Select (guiSelect n b)], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent, theValue] -> do
                                      guard (theName == n)
                                      selectedInt <- readMaybe
                                                (T.unpack theValue) :: Maybe Int
                                      let (_, selectedA) = R.radioToNEL (_sRadio b)
                                                           NEL.!! selectedInt
                                      p <- parseSelectEvent theEvent
                                      return (p selectedInt selectedA)

                                    _ -> Nothing

selectC :: Select a -> D.Doc [D.Element] (SelectEvent a, Select a)
selectC = D.widgetHandler selectHandle select
