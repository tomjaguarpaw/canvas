module Widgets where

import qualified Control.Lens       as L
import qualified TextEntry          as T
import qualified Select             as S
import qualified Doc                as D
import qualified Html               as H
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import           Focus              (Focus(NeedFocus,
                                           WantFocus, Don'tWantFocus))
import           Doc3               (Doc(Doc), handle, pairE, mapDoc,
                                     ReadMessage(ReadMessage), DocP(DocP))

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
              . (T.textEntryC `pairE` selectC)
