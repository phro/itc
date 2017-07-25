{-
 - Interval Tree Clocks based on the paper:
 - Interval Tree Clocks: A Logical Clock for Dynamic Systems
 - Paulo Sérgio Almeida, Carlos Baquero, Victor Fonte
 -}
module ITC
( Stamp
, seed -- base stamp
, fork, peek
, event
) where

import Data.Monoid

-- All functions which have these data as arguments assume they are
-- reduced. All functions that return these data return reduced values.
data ITree = O | I | INode ITree ITree deriving (Eq,Show)
data Event = Leaf Int | ENode Int Event Event deriving (Eq,Show)
data Stamp = Stamp ITree Event deriving (Eq,Show)

instance Ord Event where
  (Leaf n1) <= (Leaf n2) = n1 <= n2
  (Leaf n1) <= (ENode n2 l2 r2) = n1 <= n2
  (ENode n1 l1 r1) <= (Leaf n2) =
    n1 <= n2 && (lift n1 l1) <= Leaf n2 && (lift n1 r1) <= Leaf n2
  (ENode n1 l1 r1) <= (ENode n2 l2 r2) =
    n1 <= n2 &&
    (lift n1 l1) <= (lift n2 l2) &&
    (lift n1 r1) <= (lift n2 r2)

-- Should technically use PartialOrd here, but explicitly defining
-- compare (instead of (<=)) also does the trick.
instance Ord Stamp where
  Stamp _ e1 `compare` Stamp _ e2 = e1 `compare` e2

seed :: Stamp
seed = Stamp I (Leaf 0)

-- Always apply this function when constructing a new Id Tree
inorm :: ITree -> ITree
inorm (INode I I) = I
inorm (INode O O) = O
inorm i = i
{-inorm (INode i j) = inorm $ INode (inorm i) (inorm j)-}

lift :: Int -> Event -> Event
lift m (Leaf n) = Leaf (n + m)
lift m (ENode n e1 e2) = ENode (n + m) e1 e2

sink :: Int -> Event -> Event
sink m (Leaf n) = Leaf (n - m)
sink m (ENode n e1 e2) = ENode (n - m) e1 e2

emax :: Event -> Int
emax (Leaf n) = n
emax (ENode n e1 e2) = n + max (emax e1) (emax e2)

emin :: Event -> Int
emin (Leaf n) = n
emin (ENode n _ _) = n -- Assumes normalized Event
--emin (ENode n e1 e2) = n + min (emin e1) (emin e2) -- Doesn't assume normalized Event

enorm :: Event -> Event
enorm (Leaf n) = Leaf n
enorm (ENode n (Leaf m) (Leaf m'))
  | m == m' = Leaf (n+m)
enorm (ENode n e1 e2) = ENode (n+m) (sink m e1) (sink m e2)
  where m = min (emin e1) (emin e2)

-- Fork
split :: ITree -> (ITree,ITree)
split O             = (O, O)
split I             = (INode I O , INode O I)
split (INode O i)   = (INode O i1, INode O i2) where (i1, i2) = split i
split (INode i O)   = (INode i1 O, INode i2 O) where (i1, i2) = split i
split (INode i1 i2) = (INode i1 O, INode O i2)

fork :: Stamp -> (Stamp,Stamp)
fork (Stamp i e) = (Stamp i1 e, Stamp i2 e) where (i1, i2) = split i

peek :: Stamp -> (Stamp,Stamp)
peek (Stamp i e) = (Stamp O e, Stamp i e)

-- Join
isum :: ITree -> ITree -> ITree
isum O i = i
isum i O = i
isum (INode l1 r1) (INode l2 r2) =
  inorm $ INode (isum l1 l2) (isum r1 r2)

ejoin :: Event -> Event -> Event
ejoin (Leaf n1) (Leaf n2) = Leaf (max n1 n2)
ejoin (Leaf n1) (ENode n2 l2 r2) =
  ejoin (ENode n1 (Leaf 0) (Leaf 0)) (ENode n2 l2 r2)
ejoin (ENode n1 l1 r1) (Leaf n2) =
  ejoin (ENode n1 l1 r1) (ENode n2 (Leaf 0) (Leaf 0))
ejoin (ENode n1 l1 r1) (ENode n2 l2 r2)
  | n1 > n2   = ejoin (ENode n2 l2 r2) (ENode n1 l1 r1)
  | otherwise = enorm $ ENode n1 (ejoin l1 . lift m $ l2) (ejoin r1 . lift m $ r2)
  | otherwise = enorm $ ENode n1 (ejoin l1 . lift m $ l2) (ejoin r1 . lift m $ r2)
  where m = n2 - n1

join :: Stamp -> Stamp -> Stamp
join (Stamp i1 e1) (Stamp i2 e2) = Stamp (isum i1 i2) (ejoin e1 e2)

-- simplify by increasing the event
fill :: Stamp -> Event
fill (Stamp O e) = e
fill (Stamp I e) = Leaf $ emax e
fill (Stamp i n@(Leaf _)) = n -- notational overkill?
fill (Stamp (INode I ir) (ENode n el er)) =
  enorm (ENode n (Leaf $ max (emax el) (emin er')) er')
    where er' = fill $ Stamp ir er
fill (Stamp (INode il I) (ENode n el er)) =
  enorm (ENode n el' (Leaf $ max (emax er) (emin el')))
    where el' = fill $ Stamp il el
fill (Stamp (INode il ir) (ENode n el er)) =
  enorm (ENode n (fill $ Stamp il el) (fill $ Stamp ir er))


-- Alternatively, instead of lexicographical ordering, you can just use
-- N >> 0 instead of (1,0)
type Cost = (Sum Int, Sum Int)
grow :: Stamp -> (Event,Cost)
grow (Stamp I (Leaf n))= (Leaf $ n + 1, (0,0))
grow (Stamp i (Leaf n))= (e', c `mappend` (1,0))
  where (e', c) = grow $ Stamp i (ENode n o o)
        o = Leaf 0
grow (Stamp (INode O ir) (ENode n el er)) =
  (ENode n el er', cr `mappend` (0,1))
    where  (er', cr) = grow $ Stamp ir er
grow (Stamp (INode il O) (ENode n el er)) =
  (ENode n el' er, cl `mappend` (0,1))
    where  (el', cl) = grow $ Stamp il el
grow (Stamp (INode il ir) (ENode n el er))
  | cl < cr   = ((ENode n el' er), cl `mappend` (0,1))
  | otherwise = ((ENode n el er'), cr `mappend` (0,1))
  where (el', cl) = grow $ Stamp il el
        (er', cr) = grow $ Stamp ir er

event :: Stamp -> Stamp
event (Stamp i e)
  | e' /= e    = Stamp i e'
  | otherwise = Stamp i e''
  where e'  = fill $ Stamp i e
        (e'',_) = grow $ Stamp i e
