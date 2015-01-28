{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter where

import qualified TextEntry          as T
import qualified Select             as S
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Lazy     as DT
import qualified Radio              as R
import qualified Doc                as D
import qualified Widget             as W
import qualified Control.Lens       as L

data Available = Available (R.Radio DT.Text DT.Text)

data Filter = Filter { fAv :: Available
                     , fFi :: T.TextEntry
                     , fEd :: T.TextEntry
                     , fSe :: (S.Select Int) }
$(L.makeLenses ''Filter)

filter :: Filter -> D.DocF ((), Filter) [D.Element]
filter (Filter a t tt s) = D.fmapResponse (L.set L._1 ()) $
                           D.fmapResponse (L.over L._2 (\(((a', t'), tt'), s') ->
                                                         Filter a' t' tt' s')) $
                           fmap (\((((), e1), e2), e3) -> e1 ++ e2 ++ e3) $
                           D.fmapResponse (\(ev, a') ->
                             case ev of (Left (Left (Right (T.Input i _)))) ->
                                          (ev, L.set (L._2.S.sRadio.R.chosen.L._1) i a')
                                        ev' -> (ev', a')
                             ) $
                           (D.static
                            `W.pair` T.textEntryC
                            `W.pair` T.textEntryC
                            `W.pair` S.selectC)
                           (((a, t), tt), s)

filterMake :: Filter
filterMake = Filter (Available (R.Chosen "ignored" []))
                    (T.textEntryMake "filterer")
                    (T.textEntryMake "editor")
                    (S.selectMakeA (("foo", 1) NEL.:| [("bar", 2), ("baz", 3)]))
