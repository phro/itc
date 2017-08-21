module OT()
  where
import ITC

type Location = Int -- maybe eventually a tree structure
type Content = String -- Eventually lexemes
data Action = Add Content Location
            | Delete Int Location -- Int == amount of content
            deriving (Show)
          -- | Move Location Location Location -- fromStart fromEnd to
