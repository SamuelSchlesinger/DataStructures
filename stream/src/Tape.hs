module Tape (
    Tape(..)
  , left
  , right ) where

import Prelude hiding (take, iterate, takeWhile)
import Stream
import Control.Comonad

-- | A Tape with a focal point and a stream going left and right
data Tape a = Tape (Stream a) a (Stream a)

-- | Pulls the tape one spot left
left :: Tape a -> Tape a
left (Tape (Stream a m) b r) = Tape m a (Stream b r)

-- | Pulls the tape one spot right
right :: Tape a -> Tape a
right (Tape a m (Stream b r)) = Tape (Stream m a) b r

-- | Tape (forever a) a (forever a)
tape :: a -> Tape a
tape a = Tape (forever a) a (forever a)

-- | Maps the function onto ever every element
instance Functor Tape where
    fmap f (Tape l m r) = Tape (fmap f l) (f m) (fmap f r)

-- | Takes a tape of elements and maps each one over the element in the corresponding space
instance Applicative Tape where
    pure a = Tape (pure a) a (pure a)
    (Tape lf mf rf) <*> (Tape l m r)  = Tape (lf <*> l) (mf m) (rf <*> r)

-- | Extract takes out the element at the focal point
-- | Duplicate brings you up a dimension
instance Comonad Tape where
    extract (Tape _ a _) = a
    duplicate t = Tape (iterate left $ left t) t (iterate right $ right t)
