{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Types where

import Prelude hiding (LT,GT)
import Data.Map (Map)
import GHC.TypeLits
-- import Control.Monad.State

data Actor a = Actor
  { name :: String
  , transitions  :: Transitions a
  , priorities   :: Maybe (Priorities a)
  , globalState  :: GlobalVar a
  , currentState :: StateFSM
  , consumed :: ConsumedTokens
  , produced :: ProducedTokens
  }

instance Show (Actor a) where
  show (Actor name _ _ globVars curState consumed produced) =
    "name: " ++ name ++ "\n" ++ show globVars ++ "\nFSM state: " ++ show curState
    ++ "\n" ++ show consumed ++ "\n" ++ show produced

type StateFSM = Int
type Var   = String
type Port  = String

data Val a where
  ValB :: Bool -> Val a
  ValI :: Int  -> Val a

instance Show (Val a) where
  show (ValB b) = show b
  show (ValI i) = show i

data Guard a where
  GuardB :: Bool -> Guard Bool
  LT :: Val a -> Val a -> Guard Bool

type GlobalVar s = Map Var (Val s)
type ConsumedTokens = Map Port Int
type ProducedTokens = Map Port Int
type Priority a = (Transition a, Transition a)
type Priorities a = [Priority a]

-- type Action a = State (Transition a) a

data ActionAST a where
  Action :: String -> Guard Bool -> [(Port,Int)] -> [(Port,Int)] -> ActionAST a

fire :: String -> String
fire = id

((<=)) = LT

transition = Action

when :: (GlobalVar s -> ConsumedTokens -> Bool) -> Guard Bool
when fun = GuardB True

whenever :: Guard Bool
whenever = GuardB True

takeT :: [(Port,Int)] -> [(Port,Int)]
takeT = id

putT :: [(Port,Int)] -> [(Port,Int)]
putT = id

modify :: GlobalVar s -> GlobalVar s
modify = id

-- initTransition name fsmId =
--   Transition
--   { label = name
--   , guard = Nothing
--   , from = fsmId
--   , to   = fsmId + 1
--   , modified = id
--   , consumes = Map.empty
--   , produced = Map.empty
--   }

data Transition s = Transition
  { label :: String
  , guard :: Maybe (Guard Bool)
  , from :: StateFSM
  , to :: StateFSM
  , modifies :: (GlobalVar s) -> (GlobalVar s)
  , consumes :: ConsumedTokens
  , produces :: ProducedTokens
  }

type Transitions a = [Transition a]
