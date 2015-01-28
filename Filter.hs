{-# LANGUAGE OverloadedStrings #-}

module Filter where

import qualified TextEntry          as T
import qualified Select             as S
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Lazy     as DT
import qualified Radio              as R
import qualified Doc                as D
import qualified Widget             as W
import qualified Control.Lens       as L
import qualified TextSelect         as TS

data Available = Available (R.Radio DT.Text DT.Text)

data Filter = Filter Available T.TextEntry (T.TextEntry, S.Select ())

filter :: Filter -> D.DocF ((), Filter) [D.Element]
filter (Filter a t p) = D.fmapResponse (L.set L._1 ()) $
                        D.fmapResponse (L.over L._2 (\((a', t'), p') ->
                                                      Filter a' t' p')) $
                        fmap (\(((), e1), e2) -> e1 ++ e2) $
                        (D.static `W.pair` T.textEntryC `W.pair` TS.textSelect)
                            ((a, t), p)

filterMake :: Filter
filterMake = Filter (Available (R.Chosen "ignored" []))
                    (T.textEntryMake "filterer")
                    (T.textEntryMake "editor",
                     S.selectMake ("foo" NEL.:| ["bar", "baz"]))
