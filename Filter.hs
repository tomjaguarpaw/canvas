{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter where

import qualified TextEntry          as T
import qualified TextSelect         as TS
import qualified Select             as S
import qualified Data.Text.Lazy     as DT
import qualified Radio              as R
import qualified Control.Lens       as L
import qualified Html               as H
import           Doc3               (Doc, emitting, contains, also,
                                     handle, absurd, static)

data Filter = Filter { _fAvailable  :: R.Radio DT.Text DT.Text
                     , _fFilter     :: T.TextEntry
                     , _fTextSelect :: (T.TextEntry, S.Select Int) }
              deriving Show
$(L.makeLenses ''Filter)

data FilterEvent = FilterEvent T.TextEntryEvent
                 | EditorEvent T.TextEntryEvent
                 | SelectEvent (S.SelectEvent Int)
$(L.makePrisms ''FilterEvent)

selectFromAvailable :: T.TextEntry -> R.Radio DT.Text DT.Text -> S.Select Int
selectFromAvailable t = flip S.Select False
                        . R.filterRadio (\(x, _) ->
                                          L.view T.tText t `DT.isPrefixOf` x)
                        . R.getEnumerate
                        . R.traverseRadio R.enumerate R.enumerate

filterMake :: Filter
filterMake = Filter available
                    textEntry
                    (T.textEntryMake (L.view (S.sRadio.R.chosen.L._1) select),
                    select)
  where select = selectFromAvailable textEntry available
        available = R.Chosen "tom 1" ["tom 2", "tim 1", "tim 2", "bob 1", "bob 2"]
        textEntry = (T.textEntryMake "")


filterC :: Filter -> Doc FilterEvent Filter [H.Element]
filterC = handle _EditorEvent
          (\_ -> do
              c <- L.use (fTextSelect.L._1.T.tText)
              fAvailable.R.chosen L..= c)
          . handle _FilterEvent
          (\_ -> do
              f <- L.use fFilter
              a <- L.use fAvailable
              fTextSelect.L._2 L..= selectFromAvailable f a)
          . handle (_SelectEvent.S.cEv)
          (\i -> fAvailable L.%= R.chooseIndex i)

          . filterA

filterA :: Filter -> Doc FilterEvent Filter [H.Element]
filterA (Filter available textFilter textSelect) =
  ((Filter, concatElements)
   `contains` (static available `emitting` absurd)
   `also` (T.textEntryC textFilter `emitting` FilterEvent)
   `also` (TS.textSelectC textSelect
               `emitting` either EditorEvent SelectEvent))
  where concatElements _ filterE textSelectE = filterE ++ textSelectE

