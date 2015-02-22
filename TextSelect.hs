{-# LANGUAGE TemplateHaskell #-}

module TextSelect where

import qualified Control.Lens       as L
import           Control.Lens       ((.=))
import qualified TextEntry          as T
import qualified Select             as S
import qualified Html               as H
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import           Doc3               (Doc, emitting, contains, also, handle)

data TextSelect a = TextSelect { _tsText   :: T.TextEntry
                               , _tsSelect :: S.Select a }
                  deriving Show
$(L.makeLenses ''TextSelect)

textSelectC :: TextSelect a
            -> Doc (Either T.TextEntryEvent (S.SelectEvent a))
                   (TextSelect a)
                   [H.Element]
textSelectC t = (handle L._Left (\_ -> do
                                    text <- L.use (tsText.T.tText)
                                    tsSelect.S.sRadio.R.chosen.L._1 .= text)
              . handle L._Right (\_ -> do
                                    newText <- L.use (tsSelect.S.sRadio.R.chosen.L._1)
                                    let newLength = (fromIntegral . DT.length) newText
                                    tsText.T.tText .= newText
                                    tsText.T.tPosition .= newLength))
                ((TextSelect, (++))
                 `contains` ((T.textEntryC `emitting` Left) (_tsText t))
                 `also` ((S.selectC `emitting` Right)) (_tsSelect t))
