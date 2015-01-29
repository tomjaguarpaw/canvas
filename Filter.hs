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
import qualified Focused            as F
import qualified Control.Applicative as A

data Void = Void !Void

absurd :: Void -> a
absurd (Void v) = absurd v

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

filterB :: F.Focused Filter -> D.DocF (FilterEvent, F.Focused Filter) [D.Element]
filterB = eventMatches' _EditorEvent
      (\_ a -> L.set (F.contentsS.fAv.aAv.R.chosen)
                     (L.view (F.contents.fEd.T.tText) a) a)
          . eventMatches' _FilterEvent
      (\_ a -> L.set (F.contentsS.fSe)
                   (selectFromAvailable (L.view (F.contents.fFi) a)
                                        (L.view (F.contents.fAv) a)) a)
          . eventMatches' (_SelectEvent.S.cEv)
      (\i a -> L.over (F.contentsS.fAv.aAv) (R.chooseIndex i) a)
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

filterA :: F.Focused Filter -> D.DocF (FilterEvent, F.Focused Filter) [D.Element]
filterA ff = D.fmapResponse (L.over L._1
                                 (either (either absurd FilterEvent)
                                         (either EditorEvent SelectEvent ))) $
                            D.fmapNewState (fmap (\((a', t'), (tt', s')) ->
                                                          Filter a' t' tt' s')) $
                            fmap (\(((), e1), e2) -> e1 ++ e2) $
                            (pureF D.static
                             `pairF` T.textEntryC
                             `pairF` TS.textSelectF)
                            (fmap (\(Filter a t tt s) -> ((a, t), (tt, s))) ff)

pureF :: W.Widget d ev x -> W.Widget d ev (F.Focused x)
pureF f = D.fmapNewState A.pure . f . F.mostFocused

pairF :: W.Widget d1 ev1 (F.Focused x1)
      -> W.Widget d2 ev2 (F.Focused x2)
      -> W.Widget (d1, d2) (Either ev1 ev2) (F.Focused (x1, x2))
pairF w1 w2 t = (D.fmapResponse (\(ev, (fl, fr)) ->
                               case ev of
                                 Left _ -> (ev, A.liftA2 (,) fl fr)
                                 Right _ -> (ev, A.liftA2 (flip (,)) fr fl)))
                (W.pair w1 w2 (fmap fst t, fmap snd t))

filterMake :: Filter
filterMake = Filter available
                    textEntry
                    (T.textEntryMake (L.view (S.sRadio.R.chosen.L._1) select))
                    select
  where select = selectFromAvailable textEntry available
        available = Available (R.Chosen "tom 1" ["tom 2", "tim 1", "tim 2",
                                                 "bob 1", "bob 2"])
        textEntry = (T.textEntryMake "")
