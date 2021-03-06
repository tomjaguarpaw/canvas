{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Doc3 where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified Doc                as D
import           Focus              (Focus(NeedFocus, Don'tNeedFocus,
                                           WantFocus, Don'tWantFocus))
import qualified Focus              as Focus
import qualified Data.Monoid        as DM
import qualified Control.Monad.Trans.State as S
import qualified Data.Dynamic       as Dyn
import qualified Data.Bifunctor     as BiF
import qualified Data.Biapplicative as BiA

type Message = (Int, Dyn.Dynamic)

data DocP f b d = DocP (US (f b, d))

data DocF m f b d = Doc (DocP (ReadMessage m f) b d)

type DocR a b d = DocF Message (Focus a) b d
type Doc a b d  = DocF D.Message (Focus a) b d

data Void = Void !Void

instance Functor f => BiF.Bifunctor (DocF m f) where
  bimap = mapBD

instance  BiA.Biapplicative (DocF m (Focus e)) where
  bipure = bipure
  f <<*>> x = BiF.bimap (uncurry ($)) (uncurry ($)) (pair f x)

(<<$>>) :: BiF.Bifunctor f => (a -> a', b -> b') -> f a b -> f a' b'
(<<$>>) = uncurry BiF.bimap

($$$) :: BiF.Bifunctor f =>
         (a -> a', b -> b') -> (t -> f a b) -> t -> f a' b'
(f, g) $$$ w = \x -> (f, g) <<$>> w x

(***) :: BiA.Biapplicative f =>
         (s -> f (a -> a') (b -> b'))
         -> (s -> f a b)
         -> s -> f a' b'
w1 *** w2 = \x -> w1 x BiA.<<*>> w2 x

absurd :: Void -> a
absurd (Void v) = absurd v

mapDocP :: (d -> d') -> DocP f b d -> DocP f b d'
mapDocP f (DocP u) = DocP (L.over (L.mapped.L._2) f u)

mapResponseP :: Functor f => (b -> b') -> DocP f b d -> DocP f b' d
mapResponseP f (DocP u) = DocP (L.over (L.mapped.L._1.L.mapped) f u)

mapEvent :: (e -> e') -> DocF m (Focus e) b d -> DocF m (Focus e') b d
mapEvent f (Doc (DocP us)) = Doc (DocP (L.over l f us))
  where l = (L.mapped.L._1.answer.Focus.attached)

emitting :: (b' -> DocF m (Focus e) b d) -> (e -> e')
         -> b' -> DocF m (Focus e') b d
emitting w f b = mapEvent f (w b)

mapBehaviour :: Functor f => (b -> b') -> DocF m f b d -> DocF m f b' d
mapBehaviour f (Doc (DocP us)) = Doc (DocP (L.over (L.mapped.L._1.L.mapped) f us))

mapBD :: Functor f => (b -> b') -> (d -> d') -> DocF m f b d -> DocF m f b' d'
mapBD f g = mapBehaviour f . mapDoc g

contains :: Functor f => (b -> b', d -> d') -> DocF m f b d -> DocF m f b' d'
contains = uncurry mapBD

pairP :: A.Applicative f => DocP f b d -> DocP f b' d' -> DocP f (b, b') (d, d')
pairP (DocP u) (DocP u') = DocP (A.liftA2 pair' u u')
  where pair' (fb, d) (fb', d') = (A.liftA2 (,) fb fb', (d, d'))

pair :: A.Applicative f => DocF m f b d -> DocF m f b' d' -> DocF m f (b, b') (d, d')
pair (Doc dp) (Doc dp') = Doc (pairP dp dp')

also :: A.Applicative f
      => DocF m f (ba -> bb) (da -> db)
      -> DocF m f ba da
      -> DocF m f bb db
also x y = mapBD (uncurry ($)) (uncurry ($)) (pair x y)

pairC :: (b -> Doc a b d) -> (b' -> Doc a b' d')
      -> ((b, b') -> Doc a (b, b') (d, d'))
pairC w1 w2 (b1, b2) = w1 b1 `pair` w2 b2

pairE :: (b -> Doc a b d) -> (b' -> Doc a' b' d')
      -> ((b, b') -> Doc (Either a a') (b, b') (d, d'))
pairE w1 w2 (b1, b2) = mapEvent Left (w1 b1) `pair` mapEvent Right (w2 b2)

mapDoc :: (d -> d') -> DocF m f b d -> DocF m f b d'
mapDoc f (Doc dp) = Doc (mapDocP f dp)

static :: b -> Doc Void b ()
static = flip bipure ()

bipure :: a -> b -> DocF m (Focus e) a b
bipure a b = Doc (DocP (A.pure (ReadMessage (A.pure (Don'tWantFocus a)), b)))

handleEvent :: (e -> b -> b) -> Doc e b d -> Doc e b d
handleEvent h (Doc (DocP us)) = Doc (DocP (f us))
  where f = L.over (L.mapped.L._1.answer) (handleEventFocus h)

handleEventFocus :: (e -> b -> b) -> Focus e b -> Focus e b
handleEventFocus h = \case NeedFocus e b      -> NeedFocus e (h e b)
                           Don'tNeedFocus e b -> Don'tNeedFocus e (h e b)
                           WantFocus b b'     -> WantFocus b b'
                           Don'tWantFocus b   -> Don'tWantFocus b

data ReadMessage m f a = ReadMessage (m -> f a)

answer :: L.Setter (ReadMessage m f a) (ReadMessage m g b) (f a) (g b)
answer = L.sets (\f (ReadMessage g) -> ReadMessage (f . g))

instance Functor f => Functor (ReadMessage m f) where
  fmap f (ReadMessage fr) = ReadMessage ((fmap.fmap) f fr)

instance A.Applicative f => A.Applicative (ReadMessage m f) where
  pure = ReadMessage . A.pure . A.pure
  ReadMessage ff <*> ReadMessage fx = ReadMessage (A.liftA2 (A.<*>) ff fx)

-- Fold s a
handle :: L.Getting (DM.First a) s a -> (a -> S.State b z) -> Doc s b d -> Doc s b d
handle l f = handleEvent (\e b -> case e L.^? l of
                             Just m  -> S.execState (f m) b
                             Nothing -> b)

makeDoc :: (s -> Maybe s' -> f s'') -> (s -> D.DocF m s' d) -> s -> DocF m f s'' d
makeDoc f w s = (Doc . DocP . fmap (\(d, m) -> (ReadMessage (f s . m), d)) . D.unDoc . w) s
