
module Nets where

import Data.Set (Set)
import qualified Data.Set as S
import PetriNets


h2 = ((VarInt,"H"), 2)
o2 = ((VarInt,"O"), 2)
h2o = Transition 0

arc1 = ArcPT (VarInt,"H") h2o 2
arc2 = ArcPT (VarInt,"O") h2o 1

myNet = newPetrNet
  [h2,o2]
  [h2o]
  [arc1,arc2]
 

