module Result where

import Control.Applicative (Alternative (..))
import Data.Bifunctor

data Result e a
  = Err e
  | Ok a

instance Functor (Result e) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Err msg) = Err msg

instance Bifunctor Result where
  bimap _ fa (Ok a) = Ok (fa a)
  bimap fe _ (Err msg) = Err (fe msg)

instance Applicative (Result e) where
  pure = Ok
  (<*>) (Ok f) (Ok a) = Ok (f a)
  (<*>) (Err msg) _ = Err msg
  (<*>) _ (Err msg) = Err msg

instance Monad (Result e) where
  (>>=) (Ok a) f = f a
  (>>=) (Err msg) _ = Err msg

instance Alternative (Result e) where
  empty = Err (error "Err")
  (<|>) (Ok a) _ = Ok a
  (<|>) (Err _) (Ok b) = Ok b
  (<|>) (Err a) (Err _) = Err a
