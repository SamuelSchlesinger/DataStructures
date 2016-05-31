module Stream ( 
    Stream(..)
  , element
  , pop
  , push 
  , iterate
  , lengthUntil
  , take ) where

import Prelude hiding (iterate, take)
import Control.Parallel.Strategies

-- | An infinite stream
data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
    show (Stream a s) = show a ++ " " ++ show s

instance Functor Stream where
    fmap f (Stream a s) = Stream (f a) (fmap f s)
    fmap f (Stream a s) = runEval $ do
        fa <- rpar (f a)
        return (Stream fa (fmap f s))

-- | A stream consisting of a single element repeated ad infinitum
element :: a -> Stream a
element a = Stream a (element a)

-- | Pops a single element from the stream. 
pop :: Stream a -> (a, Stream a)
pop (Stream a s) = (a, s)

-- | Pushes a single element to the stream.
push :: (a, Stream a) -> Stream a
push (a, s) = Stream a s
-- ^ push (fst (pop s), snd (pop s)) == pop (push (a, s))

-- | Iterates the value through the function ad infinitum
iterate :: a -> (a -> a) -> Stream a
iterate a f = Stream a (iterate (f a) f)

-- | Takes a number of elements from the stream
take :: Integer -> Stream a -> [a]
take n (Stream a s) | n <= 0 = [] 
                    | otherwise = a : (take (n - 1) s)

-- | Gives the length until the stream contains a certain element
lengthUntil :: (Eq a) => a -> Stream a -> Integer
lengthUntil a s = lengthUntil' 0 a s where 
    lengthUntil' :: (Eq a) => Integer -> a -> Stream a -> Integer
    lengthUntil' n a (Stream a' s) | a == a' = n
                                   | otherwise = lengthUntil' (n + 1) a s


