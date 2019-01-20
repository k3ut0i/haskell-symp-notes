module Backtracking (
  Backtr(..)
  )
where

import Control.Applicative

data Backtr a = Return a
              | Fail
              | Backtr a :| Backtr a
              deriving (Show)

instance Functor Backtr where
  fmap f (Return x) = Return (f x)
  fmap f Fail = Fail
  fmap f (p :| q) = (fmap f p) :| (fmap f q)

instance Applicative Backtr where
  pure x = Return x
  Return f <*> Return q = Return (f q)
  _ <*> Fail = Fail
  Fail <*> _ = Fail
  (p :| q) <*> r = (p <*> r) :| (p <*> r)
  p <*> (q :| r) = (p <*> q) :| (p <*> r)
  
instance Foldable Backtr where
  foldMap f Fail = mempty
  foldMap f (Return a) = f a
  foldMap f (l :| r) = foldMap f l `mappend` foldMap f r

instance Monad Backtr where
  return a = Return a
  Return a >>= r = r a
  Fail >>= r = Fail
  p :| q >>= r = (p >>= r) :| (q >>= r)

instance Monoid (Backtr a) where
  mempty = Fail
  mappend = (:|)
