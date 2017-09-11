module OT()
  where
import ITC

-- maybe eventually a tree structure; should derive Ord
type Location = Int

type Content = String -- Eventually lexemes
data Action = Add Content Location
            | Delete Int Location
            -- Int == amount of content, or should you
            -- explicitly say what you're deleting? With good bookkeeping, this is
            -- not necessary. Also, that's a lot more bandwidth devoted to
            -- (theoretically) unneeded safe-checking.
            deriving (Show)
          -- | Move Location Location Location -- fromStart fromEnd to

shift :: Int -> Location -> Location
shift n l = l + n

-- In the case of a tie, the first action 'happened first'
-- ot a b = (a',b') where b'a = a'b
ot :: Action -> Action -> (Action,Action)
ot (Add c l) (Add c' l') =
  if l>l'
    then ((Add c (shift (length c') l) ),(Add c' l'))
    else ((Add c l),(Add c' (shift (length c) l') ))
ot (Add c l) (Delete n l') =
  if l>l'
     then if l<l'+n
             then (Add c l',Delete n l')
             else (Add c (shift (-n) l),Delete n l')
     else (Add c l,Delete n (shift (length c) l'))

-- ^^ These should really be tested for correctness, but I don't feel
-- like doing that right now.
