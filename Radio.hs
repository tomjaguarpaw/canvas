{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Radio where

import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Control.Arrow      ((>>>))
import qualified Control.Lens       as L

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

focusedX :: RadioX x o -> x
focusedX (At _ x _) = x

focusedO :: RadioO x o -> o
focusedO (Before _ o _) = o
focusedO (After _ o _) = o

unselect :: (x -> o) -> Radio x o -> [o]
unselect u = NEL.toList . radioToNEL . fmapRadio u id

choose :: x -> (x -> o) -> RadioO x o -> Radio x o
choose x u = \case Before rs _ os -> stampX (At (unselect u rs) x os)
                   After  os _ rs -> stampX (At os x (unselect u rs))

setFocusedX :: x -> RadioX x o -> RadioX x o
setFocusedX x (At l _ r) = At l x r

stampFocusedX :: x -> RadioX x o -> Radio x o
stampFocusedX x = stampX . setFocusedX x

setFocusedO :: o -> RadioO x o -> RadioO x o
setFocusedO o (Before l _ r) = Before l o r
setFocusedO o (After  l _ r) = After  l o r

stampFocusedO :: o -> RadioO x o -> Radio x o
stampFocusedO o = stampO . setFocusedO o

stampO :: RadioO x o -> Radio x o
stampO (Before rs o os) = rs `appendO` (o:os)
stampO (After os o rs)  = (os ++ [o]) `prependO` rs

traverseRadio' :: A.Applicative f
               => (x -> f x')
               -> (o -> f o')
               -> Radio x o
               -> f (Radio x' o')
traverseRadio' fx fo = traverseRadio (A.liftA2 (,)) . fmapRadio fx fo

traverseRadio :: Functor f =>
                 (forall a b. f a -> f b -> f (a, b))
              -> Radio (f x) (f o)
              -> f (Radio x o)
traverseRadio (***) = fmap fromRadio' . cases . toRadio'
  where cases = \case
          Chosen1'  fx     -> fmap (\x -> Chosen1' x) fx
          Chosen'   fx fys -> fmap (\(x, ys) -> Chosen' x ys)
                                   (fx *** traverseNEL (***) fys)
          Unchosen' fo fas -> fmap (\(o, as) -> Unchosen' o as)
                                   (fo *** traverseRadio (***) fas)

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

traverseNEL :: Functor f =>
               (forall a b. f a -> f b -> f (a, b))
               -> NEL.NonEmpty (f c)
               -> f (NEL.NonEmpty c)
traverseNEL (***) = ne >>> \case
  Left fa         -> fmap singleton fa
  Right (fa, fas) -> fmap (uncurry NEL.cons) (fa *** traverseNEL (***) fas)

ne :: NEL.NonEmpty a -> Either a (a, NEL.NonEmpty a)
ne (a :| []) = Left a
ne (a :| (a':as)) = Right (a, a' :| as)

singleton :: a -> NonEmpty a
singleton a = a :| []

fmapRadio :: (x -> x') -> (o -> o') -> Radio x o -> Radio x' o'
fmapRadio f g (Chosen x os) = Chosen (f x) (fmap g os)
fmapRadio f g (Unchosen o rs) = Unchosen (g o) (fmapRadio f g rs)

radioToNEL :: Radio a a -> NEL.NonEmpty a
radioToNEL (Chosen x xs) = x :| xs
radioToNEL (Unchosen x xs) = x `NEL.cons` radioToNEL xs

chooseFirst :: (o -> x) -> (x -> o) -> Radio x o -> Radio x o
chooseFirst ch un r = case d of
  Chosen _ _   -> r
  Unchosen c _  -> choose (ch (focusedO c)) un c
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

chosen :: Functor f => L.LensLike' f (Radio x o) x
chosen f (Chosen x os) = fmap (\x' -> Chosen x' os) (f x)
chosen f (Unchosen o rs) = fmap (Unchosen o) (chosen f rs)

fmapRadioX :: (x -> x') -> (o -> o') -> RadioX x o -> RadioX x' o'
fmapRadioX fx fo (At os x os') = At (fmap fo os) (fx x) (fmap fo os')

fmapRadioO :: (x -> x') -> (o -> o') -> RadioO x o -> RadioO x' o'
fmapRadioO fx fo = \case (Before cs o os) -> Before (fmapRadio fx fo cs)
                                                    (fo o) (fmap fo os)
                         (After os o rs)  -> After (fmap fo os) (fo o)
                                                   (fmapRadio fx fo rs)
