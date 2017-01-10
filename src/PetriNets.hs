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
    [ "places\n------"
    , unlines $ map show (Map.toList ps)
    ]
  
type T = Set Transition
-- type P = Set Place
type F = Set Arc
type W = F -> Set Int
-- type M0 = P -> Set Int

-- P intersect T = empty set
-- P union T != empty set
newPetrNet ps ts as =
--  let places = Map.fromList $ map (\(Place idx,store) -> (idx,store)) ps
  PetriNet (Map.fromList ps) (Set.fromList ts) (Set.fromList as)

type Ident = Int
type Store = Int
-- data Place = Place Int Int
type PlaceIdx = (PlaceType,String)
type Places = Map PlaceIdx PlaceValue
data PlaceType =
  VarInt
  | VarBool
  | Port
  deriving (Eq,Ord,Show)

data PlaceValue =
  PortVal Int
  | VarPresent Bool

-- data PlaceIdx =
--   PlaceVarIntIdx String
--   | PlaceVarBoolIdx String
--   | PlacePortIdx String
--   deriving (Ord,Eq,Show)
  
-- data Place =
--   PlacePort String Int
--   | PlaceVarInt String Int
--   | PlaceVarBool String Int
--  | VarBool String Int -- 1 == True, 0 == False
  -- deriving (Show)

placeValue :: PetriNet -> PlaceIdx -> PlaceValue
placeValue net idx = fromJust (Map.lookup idx (places net))

changeValue :: PetriNet -> PlaceIdx -> (Int -> Int) -> PetriNet
changeValue net idx f =
  net { places = Map.adjust f idx (places net) }

-- placeValue :: PetriNet -> PlaceIdx -> Int
-- placeValue net (VarInt,name) = goInt (places net)
--   where
--     goInt [] = error "unable to find place"
--     goInt [PlaceVarInt s' i] =
--       if s == s' then i else error "unable to find place"
--     goInt (PlaceVarInt s' i:ps) =
--       if s == s' then i else goInt ps

-- changeValue :: PetriNet -> PlaceIdx -> PetriNet
-- changeValue net plIdx@(PlaceVarIntIdx idx) =
--   let ix = Set.findIndex plIdx (places net)
--       (Pl

  -- go (places net)
  -- where
  --   go ps | Set.null ps = error "unable to change value"
  --         | otherwise =
  --           let ix = 



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

runNet :: PetriNet -> PetriNet
runNet net = go (transitions net)
  where go ts | Set.null ts = net
              | otherwise =
                let t = Set.elemAt 0 ts
                in if enabled net t
                   then let (Right x) = fire net t in runNet x
                   else go (Set.deleteAt 0 ts)
                
  

fire :: PetriNet -> Transition -> Either String PetriNet
fire net t =
  if not (enabled net t)
  then Left ("cannot fire disabled transition: " ++ show t)
  else
    -- remove from incoming places
    let incomingArcs = incoming net t
        (net'::PetriNet) =
          Set.foldr (\(ArcPT inPIdx _ w) n@(PetriNet pl tr as) ->
                       -- let newPlaces = Map.adjust (\x ->x-w) inPIdx pl
                       -- in PetriNet newPlaces tr as
                       changeValue n inPIdx (\x ->x-w)
                       ) net incomingArcs

    -- add to outgoing places
        outgoingArcs = outgoing net' t
        (net''::PetriNet)=
          Set.foldr (\(ArcTP outPIdx _ w) n@(PetriNet pl tr as) ->
                       -- let newPlaces = Map.adjust (\x ->x+w) outPIdx pl
                       -- in PetriNet newPlaces tr as
                       changeValue n outPIdx (\x ->x+w)
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
  -- (fromMaybe
  -- (error "arc does not exist")
  -- (Map.lookup placeIdx (places net))) >= w
  (placeValue net placeIdx) >= w

isSource :: PetriNet -> Transition -> Bool
isSource net t = Set.null (incoming net t)

isSink :: PetriNet -> Transition -> Bool
isSink net t = Set.null (outgoing net t)

-- isSelfLoop :: PetriNet -> Place -> Transition -> Bool
-- isSelfLoop net (Place p) (Transition t) =
--   let arcs' = Set.filter (

-- placeToPlace :: PetriNet -> (Place,Place)
-- placeToPlace net =
--   let transitionIds = T.map (\(Transition t) -> t) (transitions net)
  
        
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

