module Stream ( 
    Stream(..)
  , forever
  , pop
  , push 
  , iterate
  , take
  , takeWhile ) where

import Prelude hiding (iterate, take, takeWhile)
import Control.Comonad
import Control.Parallel.Strategies

-- | An infinite stream
data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
    show (Stream a s) = show a ++ " " ++ show s

instance Functor Stream where
    fmap f (Stream a s) = Stream (f a) (fmap f s)

instance Applicative Stream where
    pure = forever
    (Stream f fs) <*> (Stream a as) = (Stream (f a) (fs <*> as))

-- | Extract takes the first element
-- | Duplicate makes a corner out of a strip
instance Comonad Stream where
    extract (Stream a _) = a
    duplicate (Stream a s) = Stream (Stream a s) (duplicate s)

-- | fmap specialised to Stream
smap :: (a -> b) -> Stream a -> Stream b
smap f (Stream a s) = Stream (f a) (smap f s)

-- | A stream consisting of a single element repeated ad infinitum
forever :: a -> Stream a
forever a = Stream a (forever a)

-- | Pops a single element from the stream. 
pop :: Stream a -> (a, Stream a)
pop (Stream a s) = (a, s)

-- | Pushes a single element to the stream.
push :: (a, Stream a) -> Stream a
push (a, s) = Stream a s
-- ^ push . pop == id
-- ^ pop . push == id

-- | Iterates the value through the function ad infinitum
iterate :: (a -> a) -> a -> Stream a
iterate f a = Stream a (iterate f (f a))

-- | Take elements out of the stream while some predicate is True
takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (Stream a s) | p a = a : (takeWhile p s)
                         | otherwise = []

-- | Takes a number of elements from the stream
take :: Integer -> Stream a -> [a]
take n (Stream a s) | n <= 0 = [] 
                    | otherwise = a : (take (n - 1) s)
