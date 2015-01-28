module TextSelect where

import qualified Doc                as D
import qualified TextEntry          as T
import qualified Select             as S
import qualified Control.Lens       as L
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT

import           Widget             (Widget, Behaviours(..), Response(..),
                                     Component(..), vertW')

handleSelectionChange :: T.TextEntryEvent
                      -> (T.TextEntry, S.Select ())
                      -> (T.TextEntry, S.Select ())
handleSelectionChange ev t = let T.Input i _ = ev
                             in L.set (L._2.S.sRadio.R.chosen.L._1) i t

handleTextChange :: (T.TextEntry, S.Select a) -> (T.TextEntry, S.Select a)
handleTextChange t = let newW = t
                         newText = L.view (L._2.S.sRadio.R.chosen.L._1) newW
                     in (L.set (L._1.T.tText) newText
                         . L.set (L._1.T.tPosition)
                         ((fromIntegral . DT.length) newText)) newW

textSelect :: Widget [D.Element] () (T.TextEntry, S.Select ())
textSelect = D.mapWidgetDoc (uncurry (++)) $ vertW'
  Component { widget  = T.textEntryC
            , handler = \b ->
                Response { responseEvent = ()
                         , responseWhole = handleSelectionChange
                                                  (event b) (newWhole b)} }
  Component { widget  = S.selectC
            , handler = \b ->
                Response { responseEvent = ()
                         , responseWhole = handleTextChange (newWhole b) } }
