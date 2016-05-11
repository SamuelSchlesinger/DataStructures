{-# LANGUAGE GADTs #-}

data T c v where
    N :: (Ord c) => T c v -> T c v -> c -> Maybe v -> T c v -> T c v
    E :: T c v

insert :: (Ord c) => T c v -> [c] -> v -> T c v

get :: (Ord c) => T c v -> [c] -> Maybe v

-- | This took me a weirdly long amount of time to reason about.
-- | Ended up being far shorter and more coherent than the Java version.

insert _ [] _ = E

insert E (c:[]) v = N E E c (Just v) E

insert E (c:cs) v = N E (insert E cs v) c Nothing E

insert (N l d c' v' r) s@(c:[]) v
  | c <  c' = (N (insert l s v) d c' v' r)
  | c == c' = (N l d c (Just v) r)
  | c >  c' = (N l d c' v' (insert r s v))

insert (N l d c' v' r) s@(c:cs) v
  | c <  c' = (N (insert l s v) d c' v' r)
  | c == c' = (N l (insert d cs v) c' v' r)
  | c >  c' = (N l d c' v' (insert r s v ))

get _ [] = Nothing

get E _  = Nothing

get (N l d c' v' r) s@(c:[])
  | c <  c' = get l s
  | c == c' = v'
  | c >  c' = get r s

get (N l d c' v' r) s@(c:cs)
  | c <  c' = get l s
  | c == c' = get d cs
  | c >  c' = get r s


