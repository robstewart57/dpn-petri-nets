{-# LANGUAGE PartialTypeSignatures #-}

module CalActorToDot where

import LexCAL
import ParCAL
import ErrM
import AbsCAL
import PrintCAL

parseCal :: String
         -> Actor
parseCal s =
  let parsed = parseCalTree s
  in case parsed of
       Bad s -> error (show parsed ++ "\n" ++ show s)
       Ok tree -> tree

parseCalTree :: String -> Err Actor
parseCalTree s =
  let ts = myLexer s
  in pActor ts
