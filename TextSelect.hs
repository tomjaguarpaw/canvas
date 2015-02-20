{-# LANGUAGE TemplateHaskell #-}

module TextSelect where

import qualified Control.Lens       as L
import qualified TextEntry          as T
import qualified Select             as S
import qualified Html               as H
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import           Doc3               (Doc, handle', pairE, mapDoc, emitting,
                                     contains, also)

data TextSelect a = TextSelect { _tsText   :: T.TextEntry
                               , _tsSelect :: S.Select a }
                  deriving Show
$(L.makeLenses ''TextSelect)

textSelectC :: TextSelect a
            -> Doc (Either T.TextEntryEvent (S.SelectEvent a))
                   (TextSelect a)
                   [H.Element]
textSelectC t = (handle' L._Left (\_ b -> L.set (tsSelect.S.sRadio.R.chosen.L._1)
                                            (L.view (tsText.T.tText) b)
                                            b)
              . handle' L._Right (\_ b -> let newText =
                                               L.view (tsSelect.S.sRadio.R.chosen.L._1) b
                                              newLength =
                                               (fromIntegral . DT.length) newText
                                         in (L.set (tsText.T.tText) newText
                                             . L.set (tsText.T.tPosition) newLength)
                                            b))
                ((TextSelect, (++))
                 `contains` (T.textEntryC (_tsText t) `emitting` Left)
                 `also` (S.selectC (_tsSelect t) `emitting` Right))
