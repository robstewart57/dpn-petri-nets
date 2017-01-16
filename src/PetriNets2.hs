
module PetriNets2 where

import Data.Set
import Data.MultiSet

-- e.g. (1,"COL")
data Token a = PortData a | VarData String a

-- for one colour set
-- e.g. 1`(1,"COL")
-- or
--      2`(1,"COL")
type TokenSet a = (Int,Token a)
