module TextSelect where

import qualified Control.Lens       as L
import qualified TextEntry          as T
import qualified Select             as S
import qualified Html               as H
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import           Doc3               (Doc, handle, pairE, mapDoc)

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
              . (T.textEntryC `pairE` S.selectC)
