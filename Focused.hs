{-# LANGUAGE TemplateHaskell #-}

module Focused where

import qualified Control.Lens as L
import qualified Control.Applicative as A

data Focused a = Focused { _mFocused :: Maybe a, _defocused :: a }
                 deriving Show
$(L.makeLenses ''Focused)

focused :: L.Traversal' (Focused a) a
focused = mFocused.L._Just

mostFocused :: Focused a -> a
mostFocused f = case _mFocused f of
  Nothing -> _defocused f
  Just a  -> a

contents :: L.Getter (Focused a) a
contents = L.to (\fo -> case _mFocused fo of Nothing -> _defocused fo
                                             Just a  -> a)

contentsSUnsafe :: L.Traversal (Focused a) (Focused b) a b
contentsSUnsafe f fo = case _mFocused fo of
  Nothing -> A.liftA  (\y ->  Focused { _mFocused  = Nothing
                                       , _defocused = y })
                   (f (_defocused fo))
  Just a  -> A.liftA2 (\x y -> Focused { _mFocused  = Just x
                                       , _defocused = y })
             (f a) (f (_defocused fo))

contentsS :: L.Setter (Focused a) (Focused b) a b
contentsS = contentsSUnsafe

defocusBoth :: Focused (a -> b) -> Focused a -> Focused b
defocusBoth f x = Focused { _mFocused  = Nothing
                          , _defocused = _defocused f (_defocused x) }

instance Functor Focused where
  fmap f fo = Focused { _mFocused  = fmap f (_mFocused fo)
                      , _defocused = f (_defocused fo) }

instance A.Applicative Focused where
  pure a = Focused { _mFocused = Nothing, _defocused = a }
  ff <*> fx = case _mFocused ff of
    Nothing  -> case _mFocused fx of
      Nothing -> Focused { _mFocused  = Nothing
                        , _defocused = _defocused ff (_defocused fx) }
      Just ffx -> Focused { _mFocused  = Just (_defocused ff ffx)
                        , _defocused = _defocused ff (_defocused fx) }
    Just fff -> Focused { _mFocused  = Just (fff (_defocused fx))
                        , _defocused = _defocused ff (_defocused fx) }
