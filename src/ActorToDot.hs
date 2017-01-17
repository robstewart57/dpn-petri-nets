{-# LANGUAGE PartialTypeSignatures #-}

module ActorToDot where

import LexCAL
import ParCAL
import ErrM
import AbsCAL
import PrintCAL

import Data.List
import Data.Maybe

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

-- s0->act1 [label=" st"]
-- act1->s1 [label="st#bl = not st#bl"]
fsmTransitions :: Actor -> [String]
fsmTransitions (ActrNoPkg _ _ _ _ globVars blocks (SchedfsmFSM initState trans) _) =
  map (\(StTrans (Ident from) (Ident actionName) (Ident to)) ->
         from
         ++ "->"
         ++ actionName
         ++ " [label=\""
         ++ (if fst (lookupStmts actionName) then "{}" else "st")
         ++ "\"]"
         ++ "\n"
         ++ actionName
         ++ "->"
         ++ to
         ++ " [label=\""
         ++ snd (lookupStmts actionName)
         ++ "\"]"
         ) trans

  where
    lookupStmts :: String -> (Bool,String)
    lookupStmts actionName =
      let allActions =
            concatMap (\block ->
                         case block of
                           UnitCode _ -> []
                           ActionCode (InitActn _) -> []
                           ActionCode (AnActn action) -> [action]
                      ) blocks
          theAction = head (filter isThisAction allActions)
          isThisAction (ActnTags (ActnTagDecl [Ident tag]) _) = tag == actionName
          isThisAction (ActnTagsStmts (ActnTagDecl [Ident tag]) _ _) = tag == actionName
          isThisAction _ = False

          showStmts (ActnTags _ _) = ""
          showStmts (ActnTagsStmts _ _ stmts) =
            concat $ intersperse "," (map printTree (globalAssignStmts stmts))

          globalAssignStmts [] = []
          globalAssignStmts [s@(SemiColonSeparatedStmt (AssignStt st))]
            | isGlobalAssign st = [s]
            | otherwise = []
          -- globalAssignStmts [_] = []
          -- globalAssignStmts (SemiColonSeparatedStmt st@AssignStt{}:xs) =
          --   st : globalAssignStmts xs
          -- globalAssignStmts (_:xs) = globalAssignStmts xs

          isGlobalAssign (AssStmt (Ident ident) _) = elem ident globalMutableVars
          isGlobalAssign (AssStmtIdx (Ident ident) idx _) = elem ident globalMutableVars

          globalMutableVars =
            catMaybes
            $ map (\(GlobVarDecl v) -> varName v) globVars
          varName (VDecl _ (Ident v) _) = Just v
          varName (VDeclExpMut _ (Ident v) _ _) = Just v
          varName (VDeclExpIMut _ _ _ _) = Nothing

          -- not really, can have statements that write only to local action vars
          isActionPure (ActnTags _ _) = True
          isActionPure _ = False

      in (isActionPure theAction, showStmts theAction)

-- In1->act1 [label="x"]
-- act1->Out1 [label="x+1"]
actionsPortIO :: Actor -> [String]
actionsPortIO (ActrNoPkg _ _ _ _ _ blocks _ _) =
  concatMap (\block ->
         case block of
           ActionCode (AnActn action) ->
             case action of
               ActnTags (ActnTagDecl [Ident tag]) actionHead ->
                 showActionHeadPorts tag actionHead
               ActnTagsStmts (ActnTagDecl [Ident tag]) actionHead _ ->
                 showActionHeadPorts tag actionHead
           _ -> [""]) blocks
  where
    showActionHeadPorts tag actionHead =
                 case actionHead of
                   ActnHead inPattern outPattern ->
                     map (showInputPattern tag) inPattern
                     ++ map (showOutputPattern tag) outPattern
                   ActnHeadGuarded inPattern outPattern _ ->
                     map (showInputPattern tag) inPattern
                     ++ map (showOutputPattern tag) outPattern

showInputPattern :: String -> InputPattern -> String
showInputPattern actionName (InPattTagIds (Ident portName) tokens) =
  portName ++ "->" ++ actionName {- ++ " [label=\"" ++ concatMap (\(Ident t) -> t) tokens ++ "\"]" -}

showOutputPattern :: String -> OutputExp -> String
showOutputPattern actionName (OutPattTagIds (Ident portName) _) =
  actionName ++ "->" ++ portName

-- act1 [shape=box,xlabel="[not st#bl]"]
actionNodes :: Actor -> [String]
actionNodes (ActrNoPkg _ _ _ _ _ blocks _ _) =
  map (\block ->
         case block of
           ActionCode (AnActn action) ->
             case action of
               ActnTags (ActnTagDecl [Ident tag]) actionHead ->
                 showActionHeadGuards tag actionHead
               ActnTagsStmts (ActnTagDecl [Ident tag]) actionHead _ ->
                 showActionHeadGuards tag actionHead
           _ -> "") blocks
  where
    showActionHeadGuards tag actionHead =
      case actionHead of
        ActnHead _ _ -> tag ++ " [shape=box]"
        ActnHeadGuarded _ _ exps ->
          tag
          ++ " [shape=box,xlabel=\"[" ++ concatMap printTree exps ++ "]\"]"
      

digraphDot :: Actor -> String
digraphDot actor =
  unlines
  [ "digraph {"
  , "rankdir=LR"
  , unlines (fsmStates actor)
  , unlines (ports actor)
  , unlines (fsmTransitions actor)
  , unlines (actionsPortIO actor)
  , unlines (actionNodes actor)
  , "}"
  ]

  
  --  s0
  -- s1
  -- In1 [style=dashed]
  -- In2 [style=dashed]
  -- Out1 [style=dashed]
  -- Out2 [style=dashed]


  -- s1->act2 [label=" st"]
  -- act2->s0 [label=" st#bl = not st#bl"]
  -- act2->Out2 [label="x-2"]
  -- In2->act2 [label="x"]
  -- act2 [shape=box,xlabel="[not st#bl]"]
