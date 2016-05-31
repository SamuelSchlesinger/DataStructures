import Data.List (nub)

-- Inductive graphs

type Node = Int
type Adj b = [(b, Node)]
type Context a b = (Adj b, Node, a, Adj b)

data Graph a b 
 = Empty
 | (Context a b) :&: (Graph a b)

isEmpty :: Graph a b -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Graph mapping function
gmap :: (Context a b -> Context c d) -> Graph a b -> Graph c d
gmap f Empty = Empty
gmap f (c :&: g) = f c :&: gmap f g

-- | Edge reversal function
grev :: Graph a b -> Graph a b
grev = gmap swap where swap (p, v, l, s) = (s, v, l, p)

{- 

Theorems to Justify Rewrite Rules

thm 1: gmap f . gmap f' = gmap (f . f')
proof: (gmap f . gmap f') g = gmap f (gmap f' g)
        if g = Empty, then gmap _ g = Empty, so this is evident. As all graphs
                      lie on some Empty, we take this as our inductive hypothesis.
        else, .. = gmap f (gmap f' (c :&: g')) = gmap f (f' c :&: gmap f' g')
        = f (f' c) :&: gmap f (gmap f' g') = (by ind hyp) (f . f') c :&: gmap (f . f') g 
        = gmap (f . f') (c :&: g')
        = gmap (f . f') g => QED

thm 2: grev . grev = id
proof: (swap . swap) (a, _, _, b) = swap (b, _, _, a) = (a, _, _, b) => swap = id
       gmap id = id
       grev . grev = gmap swap . gmap swap = gmap (swap . swap) = gmap id = id => QED

From these two proofs, we can write the following rewrite rules:

-} 

{-# RULES
"gmap/gmap" forall f f'. gmap f . gmap f' = gmap (f . f')
"grev/grev"              grev . grev = id
  #-}

ufold :: (Context a b -> c -> c) -> c -> Graph a b -> c
ufold f u Empty = u
ufold f u (c :&: g) = f c (ufold f u g)

-- gmap f = ufold (\c -> (f c :&:)) Empty

nodes :: Graph a b -> [Node]
nodes = ufold (\(p, v, l, s) -> (v:)) []

undir :: Eq b => Graph a b -> Graph a b
undir = gmap (\(p, v, l, s) -> let ps = nub (p++s) in (ps, v, l, ps))


