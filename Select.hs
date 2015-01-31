{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Select where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import qualified Doc                as D
import qualified Html               as H
import qualified Radio              as R
import           Control.Monad      (guard)
import qualified Data.List.NonEmpty as NEL
import           Text.Read          (readMaybe)
import           Doc3               (Doc(Doc), DocP(DocP),
                                     ReadMessage(ReadMessage))
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))

data SelectEvent a = Choice { _cId :: Int, _cEv :: a }
data Select a = Select { _sRadio   :: R.Radio (T.Text, a) (T.Text, a)
                       , _sFocused :: Bool } deriving Show
$(L.makeLenses ''SelectEvent)
$(L.makeLenses ''Select)

parseSelectEvent :: T.Text -> Maybe (Int -> a -> SelectEvent a)
parseSelectEvent = \case "choose" -> Just Choice
                         _        -> Nothing

selectMakeA :: NEL.NonEmpty (T.Text, a) -> Select a
selectMakeA l = Select { _sRadio = R.chooseFirstNEL l
                       , _sFocused = False }

selectMake :: NEL.NonEmpty T.Text -> Select ()
selectMake = selectMakeA . fmap u
  where u x = (x, ())

selectHandle :: SelectEvent a -> Select a -> Select a
selectHandle (Choice n _) = L.over sRadio (R.chooseIndex n)
                            . L.set sFocused True

guiSelect :: T.Text -> Select a -> H.GUISelect
guiSelect n s = H.GUISelect { H.gsName = n
                            , H.gsRadio = R.fmapRadio fst fst (_sRadio s)
                            , H.gsFocused = _sFocused s }

-- TODO: duplication with circle
select :: Select a -> D.Doc [H.Element] (SelectEvent a)
select b = D.Doc $ do
  n <- D.unique
  return ([H.Select (guiSelect n b)], parseMessage n)
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

selectC :: Select a -> Doc (SelectEvent a) (Select a) [H.Element]
selectC se = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view sFocused se
                                                      then WantFocus se
                                                           (L.set sFocused False se)
                                                      else Don'tWantFocus se
                                           Just seev -> NeedFocus
                                                        seev
                                                        (selectHandle
                                                         seev se)
                                       ), d))
                 . D.unDoc
                 . select) se
