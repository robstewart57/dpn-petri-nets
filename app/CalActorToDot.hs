
module Main where

import ActorToDot
import System.Environment

main :: IO ()
main = do
  filename <- head <$> getArgs
  calString <- readFile filename
  let actor = parseCal calString
  putStrLn (digraphDot actor)
