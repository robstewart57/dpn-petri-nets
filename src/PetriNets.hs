
module PetriNets where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


data PetriNet =
  PetriNet
  { places :: Places
  , transitions :: T
  , arcs :: F
--  , weight :: W
--  , initMark  :: M0
  } deriving (Show)

type T = Set Transition
-- type P = Set Place
type F = Set Arc
type W = F -> Set Int
-- type M0 = P -> Set Int

-- P intersect T = empty set
-- P union T != empty set
newPetrNet ps ts as =
  let places = Map.fromList $ map (\(Place idx store) -> (idx,store)) ps
  in PetriNet places (Set.fromList ts) (Set.fromList as)

type Ident = Int
type Store = Int
data Place = Place Int Int
type Places = Map Ident Store
type PlaceIdx = Int
data Transition = Transition Int
  deriving (Eq,Ord,Show)
-- type Transition = Set Place -> Set Place
data Arc =
  ArcPT
  { inPlace :: PlaceIdx
  , toTrans :: Transition
  , weightPT :: Int
  }
  | ArcTP 
  { outPlace :: PlaceIdx
  , fromTrans :: Transition
  , weightTP :: Int
  } deriving (Eq,Ord,Show)

fire :: PetriNet -> Transition -> Either String PetriNet
fire net t =
  if not (enabled net t)
  then Left "cannot fire disabled transition"
  else
    -- remove from incoming places
    let incomingArcs = incoming net t
        untouched    = (arcs net) Set.\\ incomingArcs
        in' = Set.map (\(ArcPT placeIdx _ w) ->
                       undefined) incomingArcs
        newArcs = Set.union untouched in'
    in Right 
    
    

enabled :: PetriNet -> Transition -> Bool
enabled net t =
  let incomingArcs = incoming net t
  in Set.foldr (\arc b -> b && (arcEnabled net arc)) True incomingArcs

arcEnabled :: PetriNet -> Arc -> Bool
arcEnabled net (ArcPT placeIdx _ w) =
  (fromMaybe
  (error "arc does not exist")
  (Map.lookup placeIdx (places net))) >= w

incoming :: PetriNet -> Transition -> Set Arc
incoming net t =
  Set.filter (\(ArcPT a b w) -> b == t)
  $ Set.filter (\a -> case a of ArcPT{} -> True; _ -> False)
  $ (arcs net)
  
