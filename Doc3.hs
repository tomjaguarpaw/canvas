module Doc3 where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A

data Doc f b d = Doc (US (f b, d))

mapDoc :: (d -> d') -> Doc f b d -> Doc f b d'
mapDoc f (Doc u) = Doc (L.over (L.mapped.L._2) f u)

mapResponse :: Functor f => (b -> b') -> Doc f b d -> Doc f b' d
mapResponse f (Doc u) = Doc (L.over (L.mapped.L._1.L.mapped) f u)

pair :: A.Applicative f => Doc f b d -> Doc f b' d' -> Doc f (b, b') (d, d')
pair (Doc u) (Doc u') = Doc (A.liftA2 pair' u u')
  where pair' (fb, d) (fb', d') = (A.liftA2 (,) fb fb', (d, d'))

data Focus a b = NeedFocus a b
               | Don'tNeedFocus a b
               | WantFocus b b
               | Don'tWantFocus b

instance Functor (Focus a) where
  fmap f (NeedFocus a b) = NeedFocus a (f b)
  fmap f (Don'tNeedFocus a b) = Don'tNeedFocus a (f b)
  fmap f (WantFocus b b') = WantFocus (f b) (f b')
  fmap f (Don'tWantFocus b) = Don'tWantFocus (f b)

instance A.Applicative (Focus a) where
  pure = Don'tWantFocus
  -- These are probably errors
  NeedFocus a f <*> NeedFocus _ x = NeedFocus a (f x)
  NeedFocus a f <*> Don'tNeedFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> NeedFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> Don'tNeedFocus _ x = Don'tNeedFocus a (f x)
  WantFocus f f' <*> WantFocus _ x = WantFocus (f x) (f' x)
  -- These are fine
  NeedFocus a f <*> WantFocus _ x = NeedFocus a (f x)
  NeedFocus a f <*> Don'tWantFocus x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> WantFocus _ x = NeedFocus a (f x)
  Don'tNeedFocus a f <*> Don'tWantFocus x = Don'tNeedFocus a (f x)

  WantFocus _ f <*> NeedFocus a x = NeedFocus a (f x)
  WantFocus f _ <*> Don'tNeedFocus a x = NeedFocus a (f x)
  WantFocus f f' <*> Don'tWantFocus x = WantFocus (f x) (f' x)

  Don'tWantFocus f <*> NeedFocus a x = NeedFocus a (f x)
  Don'tWantFocus f <*> Don'tNeedFocus a x = Don'tNeedFocus a (f x)
  Don'tWantFocus f <*> WantFocus x x' = WantFocus (f x) (f x')
  Don'tWantFocus f <*> Don'tWantFocus x = Don'tWantFocus (f x)
