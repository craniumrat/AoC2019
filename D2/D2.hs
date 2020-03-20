module D2 where

import qualified Data.Map as M
import System.Environment
import Data.List.Split
import Debug.Trace (trace)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

answer1 :: String -> String
answer1 contents = let
    values = map (read :: String -> Int) $ splitOn "," contents
    prog = M.fromList $ zip [0..] values
  in
    case runProg 0 $ M.insert 2 2 $ M.insert 1 12 prog of
      Nothing -> "error"
      Just (_, prog') -> show $ M.lookup 0 prog'

output = 19690720

answer2 contents = do
  let prog = M.fromList $ zip [0..] $ map (read :: String -> Int) $ splitOn "," contents
  noun <- [0..99]
  verb <- [0..99]
  case runProg 0 $ M.insert 1 noun $ M.insert 2 verb prog of
    Nothing -> "error"
    Just (_, prog') -> case M.lookup 0 prog' of
      Nothing -> "error"
      Just o -> if (o == output) then show $ 100 * noun + verb else ""

runProg :: Int -> M.Map Int Int -> Maybe (Int, M.Map Int Int)
runProg pos prog = case M.lookup pos prog of
  Just 99 -> pure (pos, prog)

  Just 1 -> do
    pos1 <- M.lookup (pos + 1) prog
    pos2 <- M.lookup (pos + 2) prog
    v1 <- M.lookup pos1 prog
    v2 <- M.lookup pos2 prog
    out_pos  <- M.lookup (pos + 3) prog
    let prog' = M.insert out_pos ((v1) + (v2)) prog
    runProg (pos + 4) prog'

  Just 2 -> do
    pos1 <- M.lookup (pos + 1) prog
    pos2 <- M.lookup (pos + 2) prog
    v1 <- M.lookup pos1 prog
    v2 <- M.lookup pos2 prog
    out_pos <- M.lookup (pos + 3) prog
    let prog' = M.insert out_pos (v1 * v2) prog
    runProg (pos + 4) prog'

  Just x -> error $ "We should not be here" ++ show (pos, x)
 

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer2 contents)  
--  putStrLn (answer1 contents)
