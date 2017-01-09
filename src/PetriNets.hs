{-# LANGUAGE ScopedTypeVariables #-}

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
  }

instance Show PetriNet where
  show (PetriNet ps ts as) =
    unlines
    [ "PLACES\n-----"
    , unlines (map
    (\(ident,store) ->
       show ident ++ ": " ++ show store) (Map.toList ps))
    ]
  
type T = Set Transition
-- type P = Set Place
type F = Set Arc
type W = F -> Set Int
-- type M0 = P -> Set Int

-- P intersect T = empty set
-- P union T != empty set
newPetrNet ps ts as =
  let places = Map.fromList $ map (\(Place idx,store) -> (idx,store)) ps
  in PetriNet places (Set.fromList ts) (Set.fromList as)

type Ident = Int
type Store = Int
-- data Place = Place Int Int
type Places = Map Ident Store
type PlaceIdx = Int
data Place = Place Int
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

------------------
-- actions

fire :: PetriNet -> Transition -> Either String PetriNet
fire net t =
  if not (enabled net t)
  then Left ("cannot fire disabled transition: " ++ show t)
  else
    -- remove from incoming places
    let incomingArcs = incoming net t
        (net'::PetriNet) =
          Set.foldr (\(ArcPT inPIdx _ w) (PetriNet pl tr as) ->
                       let newPlaces = Map.adjust (\x ->x-w) inPIdx pl
                       in PetriNet newPlaces tr as
                       ) net incomingArcs

    -- add to outgoing places
        outgoingArcs = outgoing net' t
        (net''::PetriNet)=
          Set.foldr (\(ArcTP outPIdx _ w) (PetriNet pl tr as) ->
                       let newPlaces = Map.adjust (\x ->x+w) outPIdx pl
                       in PetriNet newPlaces tr as
                       ) net' outgoingArcs
    in Right net''

---------------------
-- queries

enabled :: PetriNet -> Transition -> Bool
enabled net t =
  let incomingArcs = incoming net t
  in Set.foldr (\arc b -> b && (arcEnabled net arc)) True incomingArcs

arcEnabled :: PetriNet -> Arc -> Bool
arcEnabled net (ArcPT placeIdx _ w) =
  (fromMaybe
  (error "arc does not exist")
  (Map.lookup placeIdx (places net))) >= w

isSource :: PetriNet -> Transition -> Bool
isSource net t = Set.null (incoming net t)

isSink :: PetriNet -> Transition -> Bool
isSink net t = Set.null (outgoing net t)

isSelfLoop :: PetriNet -> Place -> Transition -> Bool
isSelfLoop net (Place p) (Transition t) =
  let arcs' = Set.filter (

placeToPlace :: PetriNet -> (Place,Place)
placeToPlace net =
  let transitionIds = T.map (\(Transition t) -> t) (transitions net)
  
        
---------------------
-- utilities

incoming :: PetriNet -> Transition -> Set Arc
incoming net t =
  Set.filter (\(ArcPT inP toTrans w) -> toTrans == t)
  $ Set.filter (\a -> case a of ArcPT{} -> True; _ -> False)
  $ (arcs net)

outgoing :: PetriNet -> Transition -> Set Arc
outgoing net t =
  Set.filter (\(ArcTP outP fromTrans w) -> fromTrans == t)
  $ Set.filter (\a -> case a of ArcTP{} -> True; _ -> False)
  $ (arcs net)

