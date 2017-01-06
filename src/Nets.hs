
module Nets where

import Data.Set (Set)
import qualified Data.Set as S
import PetriNets


h2 = Place 0 2
o2 = Place 1 1
h2o = Transition 0

arc1 = ArcPT 0 h2o 2
arc2 = ArcPT 1 h2o 1

myNet = newPetrNet
  [h2,o2]
  [h2o]
  [arc1,arc2]
 

