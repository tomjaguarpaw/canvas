{-# LANGUAGE LambdaCase #-}

module Focus where

import Doc                          (US)
import qualified Control.Lens       as L
import qualified Control.Applicative as A
import qualified TextEntry          as T
import qualified Select             as S
import qualified Doc                as D
import qualified Network.WebSockets as WS
import qualified Radio              as R
import qualified Data.Text.Lazy     as DT
import qualified Filter             as F

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

attached :: L.Traversal (Focus a b) (Focus a' b) a a'
attached f = \case NeedFocus a b      -> fmap (\a' -> NeedFocus a' b) (f a)
                   Don'tNeedFocus a b -> fmap (\a' -> Don'tNeedFocus a' b) (f a)
                   WantFocus b b'     -> A.pure (WantFocus b b')
                   Don'tWantFocus b   -> A.pure (Don'tWantFocus b)

mostFocused :: Focus a b -> b
mostFocused x = case x of NeedFocus _ s'      -> s'
                          Don'tNeedFocus _ s' -> s'
                          WantFocus s' _      -> s'
                          Don'tWantFocus s'   -> s'
