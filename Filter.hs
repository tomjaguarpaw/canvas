{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter where

import qualified TextEntry          as T
import qualified Select             as S
import qualified TextSelect         as TS
import qualified Data.Text.Lazy     as DT
import qualified Radio              as R
import qualified Doc                as D
import qualified Widget             as W
import qualified Control.Lens       as L

data Void = Void !Void

absurd :: Void -> a
absurd (Void v) = absurd v

data Available = Available { _aAv :: R.Radio DT.Text DT.Text }
$(L.makeLenses ''Available)

data Filter = Filter { _fAv :: Available
                     , _fFi :: T.TextEntry
                     , _fEd :: T.TextEntry
                     , _fSe :: (S.Select Int) }
$(L.makeLenses ''Filter)


data FilterEvent = FilterEvent T.TextEntryEvent
                 | EditorEvent T.TextEntryEvent
                 | SelectEvent (S.SelectEvent Int)
$(L.makePrisms ''FilterEvent)

filterB :: Filter -> D.DocF (FilterEvent, Filter) [D.Element]
filterB = eventMatches' _EditorEvent
      (\_ a -> L.set (fAv.aAv.R.chosen) (L.view (fEd.T.tText) a) a)
          . eventMatches' _FilterEvent
      (\_ a -> L.set fSe (selectFromAvailable (L.view fFi a) (L.view fAv a)) a)
          . eventMatches' (_SelectEvent.S.cEv)
      (\i a -> L.over (fAv.aAv) (R.chooseIndex i) a)
          . filterA

eventMatches' l f = eventMatches l (\i (ev, a) -> (ev, f i a))

eventMatches l f = D.fmapResponse (perhaps (\(ev, a) ->
  ev L.^? l L.<&> (\ev' -> f ev' (ev, a))))

perhaps :: (a -> Maybe a) -> a -> a
perhaps f a = case f a of Nothing -> a
                          Just a' -> a'

selectFromAvailable :: T.TextEntry -> Available -> S.Select Int
selectFromAvailable t = S.Select
                        . R.filterRadio (\(x, _) ->
                                          L.view T.tText t `DT.isPrefixOf` x)
                        . R.getEnumerate
                        . R.traverseRadio' R.enumerate R.enumerate
                        . L.view aAv

filterA :: Filter -> D.DocF (FilterEvent, Filter) [D.Element]
filterA (Filter a t tt s) = D.fmapResponse (L.over L._1
                                 (either (either absurd FilterEvent)
                                         (either EditorEvent SelectEvent ))) $
                            D.fmapResponse (L.over L._2 (\((a', t'), (tt', s')) ->
                                                          Filter a' t' tt' s')) $
                            fmap (\(((), e1), e2) -> e1 ++ e2) $
                            (D.static
                             `W.pair` T.textEntryC
                             `W.pair` TS.textSelect)
                            ((a, t), (tt, s))

filterMake :: Filter
filterMake = Filter available
                    textEntry
                    (T.textEntryMake (L.view (S.sRadio.R.chosen.L._1) select))
                    select
  where select = selectFromAvailable textEntry available
        available = Available (R.Chosen "tom 1" ["tom 2", "tim 1", "tim 2",
                                                 "bob 1", "bob 2"])
        textEntry = (T.textEntryMake "")
