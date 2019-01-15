module Backtracking (
  Backtr(..)
  )
where

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
  
instance Monad Backtr where
  return a = Return a
  Return a >>= r = r a
  Fail >>= r = Fail
  p :| q >>= r = (p >>= r) :| (q >>= r)
