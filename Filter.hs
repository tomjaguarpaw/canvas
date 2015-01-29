{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter where

import qualified TextEntry          as T
import qualified Select             as S
import qualified TextSelect         as TS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Lazy     as DT
import qualified Radio              as R
import qualified Doc                as D
import qualified Widget             as W
import qualified Control.Lens       as L

data Void = Void !Void

absurd :: Void -> a
absurd (Void v) = absurd v

data Available = Available (R.Radio DT.Text DT.Text)

data Filter = Filter { _fAv :: Available
                     , _fFi :: T.TextEntry
                     , _fEd :: T.TextEntry
                     , _fSe :: (S.Select Int) }
$(L.makeLenses ''Filter)

filter :: Filter -> D.DocF (Either T.TextEntryEvent
                          (Either T.TextEntryEvent (S.SelectEvent Int)), Filter) [D.Element]
filter (Filter a t tt s) = D.fmapResponse (L.over (L._1.L._Left) (either absurd id)) $
                           D.fmapResponse (L.over L._2 (\((a', t'), (tt', s')) ->
                                                         Filter a' t' tt' s')) $
                           fmap (\(((), e1), e2) -> e1 ++ e2) $
                           (D.static
                            `W.pair` T.textEntryC
                            `W.pair` TS.textSelect)
                           ((a, t), (tt, s))

filterMake :: Filter
filterMake = Filter (Available (R.Chosen "ignored" []))
                    (T.textEntryMake "filterer")
                    (T.textEntryMake (L.view (S.sRadio.R.chosen.L._1) select))
                    select
  where select = S.selectMakeA (("quux", 1) NEL.:| [("bar", 2), ("baz", 3)])
