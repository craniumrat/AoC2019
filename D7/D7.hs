module D7 where

import qualified Data.Map as M
import System.Environment
import Data.List.Split
import Debug.Trace (trace)
import Data.List (uncons, unfoldr)
import Data.Digits (digits)
import Control.Monad (guard)
import qualified Data.Set as S

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

answer1 :: String -> String
answer1 contents = let
    prog = stringToProg contents
    answer = maximum $ map (\phase -> runAmpChain phase 0 prog) phases

  in
    show answer

stringToProg :: String -> M.Map Int Int
stringToProg str = let
  values = map (read :: String -> Int) $ splitOn "," str
  prog = M.fromList $ zip [0..] values
  in prog


toIntCode :: Int -> [Int]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

answer2 :: String -> String
answer2 contents = let
    prog = stringToProg contents
    m = computeMaximumOutput prog
  in
    show m

runAmplifier :: Int -> Int -> M.Map Int Int -> Int
runAmplifier phase input prog =
  case runProg 0 (phase:input:[]) prog of
    Nothing -> error "Error in running prog"
    Just (_, [output], _) -> output

runAmpChain :: [Int] -> Int -> M.Map Int Int -> Int
runAmpChain [] val prog = val
runAmpChain (phase:rest) val prog =
  let
    output = runAmplifier phase val prog
  in
    runAmpChain rest output prog


--- Run amps ABCDE with phase settings (inputs [5..9]). It will output y and wait for input. 
--- Pass in output of A to B, output of B to C, and output of E to A... Continue until no more outputs from all amplifiers. Output of E is the final one.
runAmpLoop :: [Int] -> Int -> M.Map Int Int -> [Int]
runAmpLoop [] val m = undefined
  

phases2 :: [[Int]]
phases2 = do
  a <- [5..9]
  b <- [5..9]
  c <- [5..9]
  d <- [5..9]
  e <- [5..9]
  let phase = [a, b, c, d, e]
  guard $ S.fromList [5..9] == S.fromList phase
  pure phase

phases :: [[Int]]
phases = do
  a <- [0..4]
  b <- [0..4]
  c <- [0..4]
  d <- [0..4]
  e <- [0..4]
  let phase = [a, b, c, d, e]
  guard $ S.fromList [0..4] == S.fromList phase
  pure phase

makeState :: [Int] -> M.Map Int Int -> [(Int, [Int], M.Map Int Int)]
makeState (first:rest) m = (0, [first, 0], m):map (\phase -> (0, [phase], m)) rest

-- Pass in the phase setting in an array and the initial prog.
computeOutputs :: [Int] -> M.Map Int Int -> [Int]
computeOutputs phases m = unfoldr next (makeState phases m)
  where
    next :: [(Int, [Int], M.Map Int Int)] -> Maybe(Int, [(Int, [Int], M.Map Int Int)])
    next [] = Nothing
    next ((ip, [], prog):rest) = next rest
    next ((ip, input, prog):progState2@(ip2, input2, prog2):rest) = case runProg ip input prog
      of
        Nothing -> error $ "Some issue in IntCode" ++ show (ip, input)
        Just (_, [], _) -> next (progState2:rest)
        Just (ip', [output], prog') -> Just (output, (((ip2, input2 ++ [output], prog2):rest) ++ [(ip', [], prog')]))

computeMaximumOutput prog = maximum $ do
  phase <- phases2
  let maxOutput = last $ computeOutputs phase prog
  pure maxOutput

runProg :: Int -> [Int] -> M.Map Int Int -> Maybe (Int, [Int], M.Map Int Int)
runProg pos inputs prog = case (toIntCode <$> M.lookup pos prog) of
  Just [_, _, _, 9, 9] -> pure (pos, [], prog)

  Just [_, _, c, 0, 3] -> do 
    loc <- getLocByMode c (pos+1) prog
    (input, rest) <- uncons inputs
    runProg (pos + 2) rest $ M.insert loc input prog

  Just [_, _, c, 0, 4] -> do
    val <- getValByMode c (pos+1) prog
    pure ((pos + 2), [val], prog)

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

type ProgState = (Int, M.Map Int Int)

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
