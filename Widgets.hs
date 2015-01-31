module Widgets where

import qualified Control.Lens       as L
import qualified TextEntry          as T
import qualified Select             as S
import qualified Doc                as D
import qualified Html               as H
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import qualified Filter             as F
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import           Doc3               (Doc(Doc), handle, pair, pairE, mapDoc,
                                     static, mapBehaviour, mapEvent,
                                     ReadMessage(ReadMessage), DocP(DocP))

textEntryC :: T.TextEntry -> Doc T.TextEntryEvent T.TextEntry [H.Element]
textEntryC te = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view T.tFocused te
                                                      then WantFocus te
                                                           (L.set T.tFocused False te)
                                                      else Don'tWantFocus te
                                           Just teev -> NeedFocus
                                                        teev
                                                        (T.textEntryHandle
                                                         teev te)
                                       ), d))
                 . D.unDoc
                 . T.textEntry) te


selectC :: S.Select a -> Doc (S.SelectEvent a) (S.Select a) [H.Element]
selectC se = (Doc
                 . DocP
                 . fmap (\(d, m) ->
                          (ReadMessage (\message -> case m message of
                                           Nothing -> if L.view S.sFocused se
                                                      then WantFocus se
                                                           (L.set S.sFocused False se)
                                                      else Don'tWantFocus se
                                           Just seev -> NeedFocus
                                                        seev
                                                        (S.selectHandle
                                                         seev se)
                                       ), d))
                 . D.unDoc
                 . S.select) se

textSelectC :: (T.TextEntry, S.Select a)
            -> Doc (Either (T.TextEntryEvent) (S.SelectEvent a))
                   (T.TextEntry, S.Select a)
                   [H.Element]
textSelectC = handle L._Left (\_ b -> L.set (L._2.S.sRadio.R.chosen.L._1)
                                            (L.view (L._1.T.tText) b)
                                            b)
              . handle L._Right (\_ b -> let newText =
                                               L.view (L._2.S.sRadio.R.chosen.L._1) b
                                             newLength =
                                               (fromIntegral . DT.length) newText
                                         in (L.set (L._1.T.tText) newText
                                             . L.set (L._1.T.tPosition) newLength)
                                            b)
              . mapDoc (uncurry (++))
              . (textEntryC `pairE` selectC)


filterC :: F.Filter -> Doc F.FilterEvent F.Filter [H.Element]
filterC = handle F._EditorEvent
          (\_ a -> L.set (F.fAv.F.aAv.R.chosen)
                         (L.view (F.fEd.T.tText) a) a)
          . handle F._FilterEvent
          (\_ a -> L.set F.fSe
                         (F.selectFromAvailable (L.view F.fFi a)
                                                (L.view F.fAv a)) a)
          . handle (F._SelectEvent.S.cEv)
          (\i a -> L.over (F.fAv.F.aAv) (R.chooseIndex i) a)

          . filterA

filterA :: F.Filter -> Doc F.FilterEvent F.Filter [H.Element]
filterA = mapBehaviour (\((a, t), (tt, s)) -> F.Filter a t tt s)
          . mapEvent (either (either F.absurd F.FilterEvent)
                           (either F.EditorEvent F.SelectEvent))
          . mapDoc (\(((), d1), d2) -> d1 ++ d2)
          . (static
           `pairE` textEntryC
           `pairE` textSelectC)
          . (\(F.Filter a t tt s) -> ((a, t), (tt, s)))


two :: (T.TextEntry, T.TextEntry)
    -> Doc T.TextEntryEvent (T.TextEntry, T.TextEntry) [H.Element]
two (t1, t2) = mapDoc (uncurry (++))
               (pair (textEntryC t1) (textEntryC t2))
