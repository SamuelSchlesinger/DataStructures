import Stream
import Prelude hiding (take, iterate)
import System.Environment

collatz :: Integer -> Stream Integer
collatz n = iterate n (\n -> if n `mod` 2 == 0 then n `quot` 2 else n * 3 + 1)

collatzNumbers = fmap (lengthUntil 1 . collatz) (iterate 1 (+ 1))

maxAndIndex :: (Ord o) => [o] -> (o, Integer)
maxAndIndex (a:as) = maxAndIndex' (a, 1) 2 as where
    maxAndIndex' :: (Ord o) => (o, Integer) -> Integer -> [o] -> (o, Integer)
    maxAndIndex' p _ [] = p
    maxAndIndex' (a, index) n (a':as) | a' > a = maxAndIndex' (a', n) (n + 1) as
                                      | otherwise = maxAndIndex' (a, index) (n + 1) as


main = do
    [lim] <- getArgs
    putStrLn $ show $ maxAndIndex $ take (read lim) $ collatzNumbers
    putStrLn $ show (maxAndIndex (take 100000000 collatzNumbers))
