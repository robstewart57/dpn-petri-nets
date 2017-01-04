{-# LANGUAGE DataKinds #-}
module Lib where

import Prelude hiding (LT,GT,(<=))
import qualified Data.Map as Map
import Data.Maybe
import Types
-- import Control.Monad.State

-- makeTransition :: String -> Int -> Query a -> (QueryData -> b) -> Transition a
-- makeTransition name fsmId q f = f $ execState q (initTransition name fsmId)

myTrans :: Transition 5 10 s
myTrans = -- (transition "action1")
          -- (when (LT (ValI 4) (ValI 5)))
          transition
          "tran1"
          (when (\state tokens -> undefined))
          -- (put (\st vec -> undefined))
          []
          []
  

-- myTrans :: ActionAST a
-- myTrans =
--   Action
--   "action1"
--   (LT (ValI 4) (ValI 5))
--   []
--   []

-- myT2 :: ActionAST a
-- myT2 = Action
--        (fire "action2")
-- --       ((<=) (ValI 4) (ValI 5))
--        (when (\globSt undefined -> Map.null globSt))
--        (takeT [])
--        (putT [])       

-- myTrans :: Transition a
-- myTrans = fire "action1"
--           `when` (LT (ValI 4) (ValI 5))
--           `takeT` ("in1" 10)
--           `putT`  ("out1" (\st vec -> undefined))

-- fire :: String -> ConsumeRate -> Transition a
-- fire name consumeRate =
--   Transition
--   { label = name
--   , guard = Nothing
--   , from  = 0
--   , to    = 1
--   , modifies = id
--   , consumes = Map.fromList [consumeRate]
--   , produces = Map.fromList []
--   }

type ConsumeRate = (String,Int)

takeTokens :: String -> Int -> ConsumeRate
takeTokens s i = (s,i)

tran1 :: Transition c p a
tran1 = Transition
  { label = "tran1"
  , guard = Just (LT (ValI 4) (ValI 5))
  , from = 0
  , to   = 1
  , modifies = \x -> x
  , consumes = Map.fromList [("in1",0)]
  , produces = Map.fromList [("out1",0)]
  }

tran2 :: Transition c p a
tran2 = Transition
  { label = "tran2"
  , guard = Just (LT (ValI 4) (ValI 5))
  , from = 0
  , to   = 1
  , modifies = \x -> x
  , consumes = Map.fromList [("in1",0)]
  , produces = Map.fromList [("out1",0)]
  }

actor :: Actor c p a
actor = Actor
  { name = "myActor"
  , transitions  = [tran1,tran2]
  , priorities   = Just [(tran1,tran2)]
  , globalState  = Map.fromList [("x",ValI 0)]
  , currentState = 0
  , consumed     = Map.fromList [("in1",0)]
  , produced     = Map.fromList [("out1",0)]
  }

runActor :: Actor c p a -> Actor c p a
runActor actor =
  if isNothing (pickTransition actor)
  then actor 
  else
    let trans = fromJust (pickTransition actor)
    in runActor (fireTransition actor trans)

fireTransition :: Actor c p a -> Transition c p a -> Actor c p a
fireTransition actor transition =
  actor { globalState = modifies transition (globalState actor)
        , currentState = to transition }

pickTransition :: Actor c p a -> Maybe (Transition c p a)
pickTransition actor = go (transitions actor)
  where
    go [] = Nothing
    go (x:xs) =
      if canFire actor x
      then Just x
      else go xs

canFire :: Actor c p a -> Transition c p a -> Bool
canFire actor transition =
  from transition == currentState actor
  && not (or (map (canFire actor) (higherPriority transition actor)))
  -- add checks on guards

higherPriority :: Transition c p a -> Actor c p a -> [Transition c p a]
higherPriority transition actor =
  foldr (\(higher,lower) xs ->
            if label transition == label lower
            then transition : xs
            else xs) [] (fromMaybe [] (priorities actor))

