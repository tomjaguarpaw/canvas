{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Doc where

import qualified Data.Text.Lazy     as T
import           Data.Monoid        ((<>))
import qualified Text.Blaze.Svg11   as S
import           Control.Applicative (liftA2, Applicative, pure, (<*>))
import qualified Control.Monad.Trans.State as St
import qualified Control.Lens       as L

type Message = T.Text

data DocF a d = Doc { unDoc :: US (d, Message -> Maybe a) }

type Doc d a = DocF a d

type US = St.State Int

runUS :: US a -> a
runUS = flip St.evalState 0

unique :: US T.Text
unique = do
  i <- St.get
  St.modify (+1)
  (return . T.pack . show) i

instance Functor (DocF a) where
  fmap = mapDoc

instance Applicative (DocF a) where
  pure x = Doc (pure (x, const Nothing))
  Doc tf <*> Doc tt = Doc (liftA2 (<**>) tf tt)
    where (df, ff) <**> (dx, fx) = (df dx, liftA2 firstJust ff fx)

mapDoc :: (a -> b) -> DocF e a -> DocF e b
mapDoc f (Doc t) = Doc (L.over (L.mapped.L._1) f t)

mapWidgetDoc :: (a -> b) -> (r -> DocF e a) -> (r -> DocF e b)
mapWidgetDoc = fmap . mapDoc

fmapResponse :: (a -> b) -> Doc d a -> Doc d b
fmapResponse f (Doc t) = Doc (L.over (L.mapped.L._2.L.mapped.L.mapped) f t)

fmapNewState :: (a -> b) -> Doc d (e, a) -> Doc d (e, b)
fmapNewState = fmapResponse . L.over L._2

static :: a -> DocF e ()
static = const (pure ())

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing (Just b) = Just b
firstJust Nothing Nothing = Nothing

handleMessage :: Doc d a -> Message -> Maybe a
handleMessage (Doc t) = snd (runUS t)

handler :: S.AttributeValue -> S.AttributeValue -> S.AttributeValue
handler h n = h <> "('" <> n <> "')"

widgetHandler :: (ev -> b -> a) -> (b -> Doc d ev) -> b -> Doc d (ev, a)
widgetHandler f w x = fmapResponse (\ev -> (ev, f ev x)) (w x)
