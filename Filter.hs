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
import           Doc3               (Doc, mapEvent,
                                     handle, absurd, static)
import qualified Doc3               as D3

data Available = Available { _aAv :: R.Radio DT.Text DT.Text } deriving Show
$(L.makeLenses ''Available)

data Filter = Filter { _fAv :: Available
                     , _fFi :: T.TextEntry
                     , _fEd :: T.TextEntry
                     , _fSe :: (S.Select Int) }
              deriving Show
$(L.makeLenses ''Filter)

data FilterEvent = FilterEvent T.TextEntryEvent
                 | EditorEvent T.TextEntryEvent
                 | SelectEvent (S.SelectEvent Int)
$(L.makePrisms ''FilterEvent)

selectFromAvailable :: T.TextEntry -> Available -> S.Select Int
selectFromAvailable t = flip S.Select False
                        . R.filterRadio (\(x, _) ->
                                          L.view T.tText t `DT.isPrefixOf` x)
                        . R.getEnumerate
                        . R.traverseRadio R.enumerate R.enumerate
                        . L.view aAv

filterMake :: Filter
filterMake = Filter available
                    textEntry
                    (T.textEntryMake (L.view (S.sRadio.R.chosen.L._1) select))
                    select
  where select = selectFromAvailable textEntry available
        available = Available (R.Chosen "tom 1" ["tom 2", "tim 1", "tim 2",
                                                 "bob 1", "bob 2"])
        textEntry = (T.textEntryMake "")


filterC :: Filter -> Doc FilterEvent Filter [H.Element]
filterC = handle _EditorEvent
          (\_ a -> L.set (fAv.aAv.R.chosen)
                         (L.view (fEd.T.tText) a) a)
          . handle _FilterEvent
          (\_ a -> L.set fSe
                         (selectFromAvailable (L.view fFi a)
                                                (L.view fAv a)) a)
          . handle (_SelectEvent.S.cEv)
          (\i a -> L.over (fAv.aAv) (R.chooseIndex i) a)

          . filterA

filterA :: Filter -> Doc FilterEvent Filter [H.Element]
filterA (Filter a t tt s) = (D3.mapBD boller' boller (mapEvent absurd (static a))
                             `D3.pairF` (mapEvent FilterEvent (T.textEntryC t))
                             `D3.pairF` (mapEvent (either EditorEvent SelectEvent) (TS.textSelectC (tt, s))))
  where boller = const (++)
        boller' a' = uncurry . Filter a'
