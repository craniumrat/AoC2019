module D5 where

import qualified Data.Map as M
import System.Environment
import Data.List.Split
import Debug.Trace (trace)
import Data.List (uncons)
import Data.Digits (digits)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

answer1 :: String -> String
answer1 contents = let
    values = map (read :: String -> Int) $ splitOn "," contents
    prog = M.fromList $ zip [0..] values
    Just (_, outputs, _) = runProg 0 [1] prog
    answer = last outputs
  in
    show answer


toIntCode :: Int -> [Int]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

answer2 :: String -> String
answer2 contents = let
    values = map (read :: String -> Int) $ splitOn "," contents
    prog = M.fromList $ zip [0..] values
    Just (_, outputs, _) = runProg 0 [5] prog
    answer = last outputs
  in
    show answer

runProg :: Int -> [Int] -> M.Map Int Int -> Maybe (Int, [Int], M.Map Int Int)
runProg pos inputs prog = case (toIntCode <$> M.lookup pos prog) of
  Just [_, _, _, 9, 9] -> pure (pos, [], prog)

  Just [_, _, c, 0, 3] -> do 
    loc <- getLocByMode c (pos+1) prog
    (input, rest) <- uncons inputs
    runProg (pos + 2) rest $ M.insert loc input prog

  Just [_, _, c, 0, 4] -> do
    val <- getValByMode c (pos+1) prog
    (ip', outputs, prog') <- runProg (pos + 2) inputs prog
    pure (ip', (val:outputs), prog')

  Just [_, b, c, 0, 5] -> do
    val <- getValByMode c (pos + 1) prog
    ip' <- getValByMode b (pos + 2) prog
    if (val /= 0) then
      runProg ip' inputs prog
    else
      runProg (pos + 3) inputs prog

  Just [_, b, c, 0, 6] -> do
    val <- getValByMode c (pos + 1) prog
    ip' <- getValByMode b (pos + 2) prog
    if (val == 0) then
      runProg ip' inputs prog
    else
      runProg (pos + 3) inputs prog

  Just [a, b, c, 0, 7] -> do
    v1 <- getValByMode c (pos + 1) prog
    v2 <- getValByMode b (pos + 2) prog
    loc <- getLocByMode a (pos + 3) prog
    if (v1 < v2) then
      runProg (pos + 4) inputs $ M.insert loc 1 prog
    else
      runProg (pos + 4) inputs $ M.insert loc 0 prog

  Just [a, b, c, 0, 8] -> do
    v1 <- getValByMode c (pos + 1) prog
    v2 <- getValByMode b (pos + 2) prog
    loc <- getLocByMode a (pos + 3) prog
    if (v1 == v2) then
      runProg (pos + 4) inputs $ M.insert loc 1 prog
    else
      runProg (pos + 4) inputs $ M.insert loc 0 prog

  Just[0, b, c, 0, op] -> do
    v1 <- getValByMode c (pos + 1) prog
    v2 <- getValByMode b (pos + 2) prog
    out_pos  <- M.lookup (pos + 3) prog
    let prog' = M.insert out_pos (getOp op v1 v2) prog
    runProg (pos + 4) inputs prog'

  Just x -> error $ "We should not be here" ++ show (pos, x)

getOp :: Int -> Int -> Int -> Int
getOp 1 = (+)
getOp 2 = (*)
getOp code = error $ "Invalid Op" ++ (show code)

getValByMode :: Int -> Int -> M.Map Int Int -> Maybe Int
getValByMode mode pos m = do
  loc <- getLocByMode mode pos m
  M.lookup loc m
  

getLocByMode :: Int -> Int -> M.Map Int Int -> Maybe Int
getLocByMode mode pos m = case mode of
  0 -> do
    loc <- M.lookup pos m
    pure loc
  1 -> pure pos
  _ -> error $ "Invalid mode" ++ (show mode)

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer2 contents)  
--  putStrLn (answer1 contents)
