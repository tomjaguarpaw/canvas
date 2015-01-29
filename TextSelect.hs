module TextSelect where

import qualified Doc                as D
import qualified TextEntry          as T
import qualified Select             as S
import qualified Control.Lens       as L
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import qualified Focused            as F
import           Widget             (Widget, Behaviours(..), Response(..),
                                     Component(..), vertW')

handleSelectionChange :: T.TextEntryEvent
                      -> (F.Focused T.TextEntry, S.Select a)
                      -> (F.Focused T.TextEntry, S.Select a)
handleSelectionChange ev t = let T.Input i _ = ev
                             in L.set (L._2.S.sRadio.R.chosen.L._1) i t

handleTextChange :: (F.Focused T.TextEntry, S.Select a)
                 -> (F.Focused T.TextEntry, S.Select a)
handleTextChange t = let newW = t
                         newText = L.view (L._2.S.sRadio.R.chosen.L._1) newW
                     in (L.set (L._1.F.contentsS.T.tText) newText
                         . L.set (L._1.F.contentsS.T.tPosition)
                         ((fromIntegral . DT.length) newText)) newW

textSelect :: Widget [D.Element] (Either T.TextEntryEvent (S.SelectEvent a))
                                 (F.Focused T.TextEntry, S.Select a)
textSelect = D.mapWidgetDoc (uncurry (++)) $ vertW'
  Component { widget  = T.textEntryC
            , handler = \b ->
                Response { responseEvent = Left (event b)
                         , responseWhole = handleSelectionChange
                                                  (event b) (newWhole b)} }
  Component { widget  = S.selectC
            , handler = \b ->
                Response { responseEvent = Right (event b)
                         , responseWhole = handleTextChange (newWhole b) } }

textSelectF :: Widget [D.Element] (Either T.TextEntryEvent (S.SelectEvent a))
                                 (F.Focused (T.TextEntry, S.Select a))
textSelectF t = D.fmapNewState (\(fte', sel)
                                          -> fmap (\x -> (x, sel)) fte')
                          (textSelect (fte, sel))
  where (_, sel) = F.mostFocused t
        fte = fmap fst t
