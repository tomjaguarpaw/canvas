{-# LANGUAGE LambdaCase #-}

module Doc3 where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified Doc                as D
import qualified Filter             as F
import           Focus              (Focus(NeedFocus, Don'tNeedFocus,
                                           WantFocus, Don'tWantFocus))
import qualified Focus              as Focus
import qualified Data.Monoid        as DM

data DocP f b d = DocP (US (f b, d))

data Doc a b d = Doc (DocP (ReadMessage (Focus a)) b d)

mapDocP :: (d -> d') -> DocP f b d -> DocP f b d'
mapDocP f (DocP u) = DocP (L.over (L.mapped.L._2) f u)

mapResponseP :: Functor f => (b -> b') -> DocP f b d -> DocP f b' d
mapResponseP f (DocP u) = DocP (L.over (L.mapped.L._1.L.mapped) f u)

mapEvent :: (e -> e') -> Doc e b d -> Doc e' b d
mapEvent f (Doc (DocP us)) = Doc (DocP (L.over l f us))
  where l = (L.mapped.L._1.answer.Focus.attached)

mapBehaviour :: (b -> b') -> Doc e b d -> Doc e b' d
mapBehaviour f (Doc (DocP us)) = Doc (DocP (L.over (L.mapped.L._1.L.mapped) f us))

pairP :: A.Applicative f => DocP f b d -> DocP f b' d' -> DocP f (b, b') (d, d')
pairP (DocP u) (DocP u') = DocP (A.liftA2 pair' u u')
  where pair' (fb, d) (fb', d') = (A.liftA2 (,) fb fb', (d, d'))

pair :: Doc a b d -> Doc a b' d' -> Doc a (b, b') (d, d')
pair (Doc dp) (Doc dp') = Doc (pairP dp dp')

pairC :: (b -> Doc a b d) -> (b' -> Doc a b' d')
      -> ((b, b') -> Doc a (b, b') (d, d'))
pairC w1 w2 (b1, b2) = w1 b1 `pair` w2 b2

pairE :: (b -> Doc a b d) -> (b' -> Doc a' b' d')
      -> ((b, b') -> Doc (Either a a') (b, b') (d, d'))
pairE w1 w2 (b1, b2) = mapEvent Left (w1 b1) `pair` mapEvent Right (w2 b2)

mapDoc :: (d -> d') -> Doc a b d -> Doc a b d'
mapDoc f (Doc dp) = Doc (mapDocP f dp)

static :: b -> Doc F.Void b ()
static b = Doc (DocP (A.pure (ReadMessage (A.pure (Don'tWantFocus b)), ())))

handleEvent :: (e -> b -> b) -> Doc e b d -> Doc e b d
handleEvent h (Doc (DocP us)) = Doc (DocP (f us))
  where f = L.over (L.mapped.L._1.answer) (handleEventFocus h)

handleEventFocus :: (e -> b -> b) -> Focus e b -> Focus e b
handleEventFocus h = \case NeedFocus e b      -> NeedFocus e (h e b)
                           Don'tNeedFocus e b -> Don'tNeedFocus e (h e b)
                           WantFocus b b'     -> WantFocus b b'
                           Don'tWantFocus b   -> Don'tWantFocus b

data ReadMessage f a = ReadMessage (D.Message -> f a)

answer :: L.Setter (ReadMessage f a) (ReadMessage g b) (f a) (g b)
answer = L.sets (\f (ReadMessage g) -> ReadMessage (f . g))

instance Functor f => Functor (ReadMessage f) where
  fmap f (ReadMessage fr) = ReadMessage ((fmap.fmap) f fr)

instance A.Applicative f => A.Applicative (ReadMessage f) where
  pure = ReadMessage . A.pure . A.pure
  ReadMessage ff <*> ReadMessage fx = ReadMessage (A.liftA2 (A.<*>) ff fx)

-- Fold s a -> (a -> b -> b) -> Doc s b d -> Doc s b d
handle :: L.Getting (DM.First a) s a -> (a -> b -> b) -> Doc s b d -> Doc s b d
handle l f = handleEvent (\e b -> case e L.^? l of
                             Just m  -> f m b
                             Nothing -> b)
