{-# LANGUAGE PartialTypeSignatures #-}

module ActorToDot where

import LexCAL
import ParCAL
import ErrM
import AbsCAL
import PrintCAL

import Data.List

parseCal :: String -> Actor
parseCal s =
  let parsed = parseCalTree s
  in case parsed of
       Bad s -> error (show parsed ++ "\n" ++ show s)
       Ok tree -> tree

parseCalTree :: String -> Err Actor
parseCalTree s =
  let ts = myLexer s
  in pActor ts

fsmStates :: Actor -> [String]
fsmStates (ActrNoPkg _ _ _ _ _ _ sched _) =
  case sched of
    (SchedfsmFSM _ trans) -> nub $ concatMap showStates trans 
fsmStates (ActrSchd _ _ _ _ _ _ _ sched _) =
  case sched of
    (SchedfsmFSM _ trans) -> nub $ concatMap showStates trans 

showStates :: StateTransition -> [String]
showStates (StTrans (Ident pre) action (Ident post)) =
  [pre,post]

ports :: Actor -> [String]
ports (ActrNoPkg _ _ _ (IOSg ins outs) _ _ _ _) =
  showPorts ins ++ showPorts outs
  where
    showPorts [] = []
    showPorts [PortDcl _ (Ident ident)] =
      [ident ++ "[style=dashed]"]
    showPorts (PortDcl _ (Ident ident):xs) =
      (ident ++ "[style=dashed]") : showPorts xs

transitions :: Actor -> [String]
transitions (ActrNoPkg _ _ _ _ _ blocks _ _) =
  map showActionCode blocks
  where
    showActionCode (ActionCode (AnActn actn)) = showAction actn
    showAction (ActnTagStmts tag actnHead stmts) =
  
digraphDot :: Actor -> String
digraphDot actor =
  unlines
  [ "digraph {"
  , unlines (fsmStates actor)
  , unlines (ports actor)
  , unlines (transitions actor)
  , "}"
  ]

  
  --  s0
  -- s1
  -- In1 [style=dashed]
  -- In2 [style=dashed]
  -- Out1 [style=dashed]
  -- Out2 [style=dashed]

  -- s0->act1 [label=" st"]
  -- In1->act1 [label="x"]
  -- act1->s1 [label="st#bl = not st#bl"]
  -- act1->Out1 [label="x+1"]
  -- act1 [shape=box,xlabel="[not st#bl]"]
  
  -- s1->act2 [label=" st"]
  -- act2->s0 [label=" st#bl = not st#bl"]
  -- act2->Out2 [label="x-2"]
  -- In2->act2 [label="x"]
  -- act2 [shape=box,xlabel="[not st#bl]"]
