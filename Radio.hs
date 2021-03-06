{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Radio where

import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Control.Arrow      ((>>>))
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified Control.Monad.Trans.State as St
import qualified Data.Functor.Identity as Id

data Radio x o = Chosen x [o]
               | Unchosen o (Radio x o)
               deriving Show

data Radio' x o = Chosen1' x
                | Chosen' x (NEL.NonEmpty o)
                | Unchosen' o (Radio x o)

data RadioO x o = Before (Radio x o) o [o]
                | After  [o] o (Radio x o)
                deriving Show

data RadioX x o = At [o] x [o]
                deriving Show

data NELZ a = NELZ [a] a [a]

consNELZ :: a -> NELZ a -> NELZ a
consNELZ u (NELZ ls a rs) = NELZ (u:ls) a rs

consRadioO :: o -> RadioO x o -> RadioO x o
consRadioO u (Before rs o os) = Before (consRadio u rs) o os
consRadioO u (After os o rs)  = After  (u:os) o rs

consRadioONEL :: x -> NELZ o -> RadioO x o
consRadioONEL x (NELZ ls a rs) = Before (Chosen x ls) a rs

consRadioX :: o -> RadioX x o -> RadioX x o
consRadioX o (At os x os') = At (o:os) x os'

consRadio :: o -> Radio x o -> Radio x o
consRadio o rs = Unchosen o rs

toRadio' :: Radio x o -> Radio' x o
toRadio' (Chosen x [])     = Chosen1' x
toRadio' (Chosen x (y:ys)) = Chosen' x (y :| ys)
toRadio' (Unchosen x rs)   = Unchosen' x rs

fromRadio' :: Radio' x o -> Radio x o
fromRadio' (Chosen1' x)          = Chosen x []
fromRadio' (Chosen' x (y :| ys)) = Chosen x (y:ys)
fromRadio' (Unchosen' x rs)      = Unchosen x rs

stampX :: RadioX x o -> Radio x o
stampX (At [] x rs) = Chosen x rs
stampX (At (l:ls) x rs) = Unchosen l (stampX (At ls x rs))

appendO :: Radio x o -> [o] -> Radio x o
appendO (Chosen x os) os'   = Chosen x (os ++ os')
appendO (Unchosen o rs) os' = Unchosen o (appendO rs os')

prependO :: [o] -> Radio x o -> Radio x o
prependO [] rs     = rs
prependO (o:os) rs = Unchosen o (prependO os rs)

focusedX :: L.Lens (RadioX x o) (RadioX x' o) x x'
focusedX f (At l x r) = fmap (\x' -> At l x' r) (f x)

focusedO :: L.Lens' (RadioO x o) o
focusedO f = \case Before l o r -> fmap (\o' -> Before l o' r) (f o)
                   After  l o r -> fmap (\o' -> After  l o' r) (f o)

unselect :: (x -> o) -> Radio x o -> [o]
unselect u = NEL.toList . radioToNEL . fmapRadio u id

choose :: x -> (x -> o) -> RadioO x o -> Radio x o
choose x u = \case Before rs _ os -> stampX (At (unselect u rs) x os)
                   After  os _ rs -> stampX (At os x (unselect u rs))

stampFocusedX :: x -> RadioX x o -> Radio x o
stampFocusedX x = stampX . L.set focusedX x

stampFocusedO :: o -> RadioO x o -> Radio x o
stampFocusedO o = stampO . L.set focusedO o

stampO :: RadioO x o -> Radio x o
stampO (Before rs o os) = rs `appendO` (o:os)
stampO (After os o rs)  = (os ++ [o]) `prependO` rs

stampNELZ :: NELZ a -> NEL.NonEmpty a
stampNELZ (NELZ bs b bs') = case bs of
  []   -> b NEL.:| bs'
  x:xs -> x NEL.:| (xs ++ [b] ++ bs')

traverseRadio :: A.Applicative f
              => (x -> f x')
              -> (o -> f o')
              -> Radio x o
              -> f (Radio x' o')
traverseRadio fx fo = \case
  Chosen x os -> Chosen A.<$> fx x
                        A.<*> L.traverse fo os
  Unchosen o rs -> Unchosen A.<$> fo o
                            A.<*> traverseRadio fx fo rs

over2 :: ((a -> Id.Identity a')
       -> (b -> Id.Identity b')
       -> (s -> Id.Identity t))
      -> (a -> a') -> (b -> b') -> (s -> t)
over2 l f g = Id.runIdentity . l (return . f) (return . g)

enumerate :: a -> St.State Int (a, Int)
enumerate a = do
  i <- St.get
  St.put (i + 1)
  return (a, i)

getEnumerate :: St.State Int a -> a
getEnumerate = flip St.evalState 0

sequenceRadio :: Functor f =>
                 (forall a b. f a -> f b -> f (a, b))
              -> Radio (f x) (f o)
              -> f (Radio x o)
sequenceRadio (***) = fmap fromRadio' . cases . toRadio'
  where cases = \case
          Chosen1'  fx     -> fmap (\x -> Chosen1' x) fx
          Chosen'   fx fys -> fmap (\(x, ys) -> Chosen' x ys)
                                   (fx *** traverseNEL (***) fys)
          Unchosen' fo fas -> fmap (\(o, as) -> Unchosen' o as)
                                   (fo *** sequenceRadio (***) fas)

duplicateNEL :: NEL.NonEmpty a -> NEL.NonEmpty (NELZ a)
duplicateNEL = ne >>> \case
  Left a        -> singleton (NELZ [] a [])
  Right (a, as) -> NELZ [] a (NEL.toList as)
                   `NEL.cons` (fmap (a `consNELZ`) (duplicateNEL as))

-- Does this have an easier mutually recursive implementation with
-- extendRadio?
duplicateRadio' :: Radio' x o -> Radio' (RadioX x o) (RadioO x o)
duplicateRadio' (Chosen1' x)     = Chosen1' (At [] x [])
duplicateRadio' (Chosen' x xs)   = Chosen' (At [] x (NEL.toList xs))
                                           (fmap (x `consRadioONEL`)
                                                 (duplicateNEL xs))
duplicateRadio' (Unchosen' o rs) = Unchosen' (After [] o rs) rost
  where rest = fromRadio' (duplicateRadio' (toRadio' rs))
        rost = fmapRadio (o `consRadioX`) (o `consRadioO`) rest

duplicateRadio :: Radio x o -> Radio (RadioX x o) (RadioO x o)
duplicateRadio = fromRadio' . duplicateRadio' . toRadio'

extendRadio :: (RadioX x o -> x')
            -> (RadioO x o -> o')
            -> Radio x o -> Radio x' o'
extendRadio fx fo = fmapRadio fx fo . duplicateRadio

extendNEL' :: [a] -> (NELZ a -> a') -> NEL.NonEmpty a -> NEL.NonEmpty a'
extendNEL' as f bs = case ne bs of
  Left c        -> singleton (f (NELZ as c []))
  Right (c, cs) -> f (NELZ as c (NEL.toList cs)) `NEL.cons` extendNEL' (as ++ [c]) f cs

extendNEL :: (NELZ a -> a') -> NEL.NonEmpty a -> NEL.NonEmpty a'
extendNEL = extendNEL' []

traverseNEL :: Functor f =>
               (forall a b. f a -> f b -> f (a, b))
               -> NEL.NonEmpty (f c)
               -> f (NEL.NonEmpty c)
traverseNEL (***) = ne >>> \case
  Left fa         -> fmap singleton fa
  Right (fa, fas) -> fmap (uncurry NEL.cons) (fa *** traverseNEL (***) fas)

sequenceNEL2 :: (forall a b a' b'. f a a' -> f b b' -> f (a, b) (a', b'))
             -> (forall a b a' b'. (a -> b) -> (a' -> b') -> f a a' -> f b b')
             -> NEL.NonEmpty (f c d)
             -> f (NEL.NonEmpty c) (NEL.NonEmpty d)
sequenceNEL2 (***) bimap = ne >>> \case
  Left fa         -> bimap singleton singleton fa
  Right (fa, fas) -> bimap (uncurry NEL.cons) (uncurry NEL.cons)
                           (fa *** sequenceNEL2 (***) bimap fas)

ne :: NEL.NonEmpty a -> Either a (a, NEL.NonEmpty a)
ne (a :| []) = Left a
ne (a :| (a':as)) = Right (a, a' :| as)

singleton :: a -> NonEmpty a
singleton a = a :| []

fmapRadio :: (x -> x') -> (o -> o') -> Radio x o -> Radio x' o'
fmapRadio = over2 traverseRadio

radioToNEL :: Radio a a -> NEL.NonEmpty a
radioToNEL (Chosen x xs) = x :| xs
radioToNEL (Unchosen x xs) = x `NEL.cons` radioToNEL xs

chooseFirst :: (o -> x) -> (x -> o) -> Radio x o -> Radio x o
chooseFirst ch un r = case d of
  Chosen _ _   -> r
  Unchosen c _  -> choose (ch (L.view focusedO c)) un c
  where d = duplicateRadio r

chooseFirstNEL :: NEL.NonEmpty a -> Radio a a
chooseFirstNEL (a :| as) = Chosen a as

-- FIXME: do a strict accumulating version
chosenIndex :: Radio x o -> Int
chosenIndex (Chosen _ _) = 0
chosenIndex (Unchosen _ xs) = 1 + chosenIndex xs

chooseIndexNEL :: Int -> NEL.NonEmpty a -> Radio a a
chooseIndexNEL 0 (a :| as) = Chosen a as
chooseIndexNEL n as        = case ne as
                             of Left b -> Chosen b []
                                Right (b, bs) -> Unchosen b (chooseIndexNEL (n-1) bs)

chooseIndex :: Int -> Radio a a -> Radio a a
chooseIndex 0 c@(Chosen _ _) = c
chooseIndex 0 (Unchosen a as) = Chosen a ((NEL.toList . radioToNEL) as)
chooseIndex n (Chosen a as) = case NEL.nonEmpty as
                              of Nothing -> Chosen a []
                                 Just bs -> Unchosen a (chooseIndexNEL (n-1) bs)
chooseIndex n (Unchosen a as) = Unchosen a (chooseIndex (n-1) as)

chosen :: L.Lens (Radio x o) (Radio x' o) x x'
chosen f (Chosen x os) = fmap (\x' -> Chosen x' os) (f x)
chosen f (Unchosen o rs) = fmap (Unchosen o) (chosen f rs)

traverseRadioX :: A.Applicative f
               => (x -> f x') -> (o -> f o') -> RadioX x o -> f (RadioX x' o')
traverseRadioX fx fo (At os x os') = At A.<$> L.traverse fo os
                                        A.<*> fx x
                                        A.<*> L.traverse fo os'

traverseRadioO :: A.Applicative f
               => (x -> f x') -> (o -> f o') -> RadioO x o -> f (RadioO x' o')

traverseRadioO fx fo = \case
  Before cs o os -> Before A.<$> traverseRadio fx fo cs
                           A.<*> fo o
                           A.<*> L.traverse fo os
  After os o rs  -> After  A.<$> L.traverse fo os
                           A.<*> fo o
                           A.<*> traverseRadio fx fo rs

fmapRadioX :: (x -> x') -> (o -> o') -> RadioX x o -> RadioX x' o'
fmapRadioX = over2 traverseRadioX

fmapRadioO :: (x -> x') -> (o -> o') -> RadioO x o -> RadioO x' o'
fmapRadioO = over2 traverseRadioO

filterRadio :: (o -> Bool) -> Radio x o -> Radio x o
filterRadio f = \case Chosen x os   -> Chosen x (filter f os)
                      Unchosen o rs -> (if f o
                                        then Unchosen o
                                        else id) (filterRadio f rs)
