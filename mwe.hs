data Event = Leaf Int | ENode Int Event Event deriving (Eq,Show)

lift :: Int -> Event -> Event
lift m (Leaf n) = Leaf (n + m)
lift m (ENode n e1 e2) = ENode (n + m) e1 e2

ejoin :: Event -> Event -> Event
ejoin (ENode n1 l1 r1) (ENode n2 l2 r2) =
  ENode n1 (ejoin l1 . lift m $ l2) (ejoin r1 . lift m $ r2)
    where m = n2-n1
    --where m = n2 − n1
