module D15 where

import qualified Data.Map as M
import System.Environment
import Data.List.Split
import Debug.Trace (trace)
import Data.List (uncons, unfoldr)
import Data.Digits (digits)
import Control.Monad (guard, void)
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, catMaybes)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

{- The outputs are going be a single at a time till the end. We are going to modify the program to return a single value every time.  -}

--- State will store a Left of the [(progState, positions)] to try and a Right of distance.
type State = ((Integer, Integer, M.Map Integer Integer), (Integer, Integer), Integer)

nextPt :: (Integer, Integer) -> Integer -> (Integer, Integer)
nextPt (x, y) 1 = (x, y - 1)
nextPt (x, y) 2 = (x, y + 1)
nextPt (x, y) 3 = (x + 1, y)
nextPt (x, y) 4 = (x - 1, y)


{- Since we just need the shortest distance to the oxygen value, we are using Either _ Integer, and for every legit travel, we increase the distance by 1.
For answer 2, we need the starting point of the oxygen, so instead of Either _ Integer, we use Either _ State to store the final path. For answer1, we will then just return the distance. We need the state where the oxygen is as a starting point for answer2 -}

runProgOnInputs :: State -> [Integer] -> Either [State] State
runProgOnInputs ((ip, base, prog), (x, y), d) inputs =
    outputs
      where
        outputs = foldl (\acc input -> execute acc input) (Left []) inputs
        execute acc input =
          case acc of
            Right state -> Right state
            Left states ->
              case runProg ip base [input] prog of
                Nothing -> error "failure running prog"
                Just (ip', base', output, prog') -> case output of
                  Nothing -> error "no output"
                  Just output_val -> case output_val of
                    2 -> Right ((ip', base', prog'), nextPt (x, y) input, d + 1)
                    0 -> Left states
                    1 -> Left $ states ++ [((ip', base', prog'), (nextPt (x, y) input), d + 1)]
          
output :: [State] -> S.Set (Integer, Integer) -> Either [State] State
output (state@((ip, base, prog), (x, y), distance):states) visited =
  let
    testDirections = filter (\dir -> S.notMember (nextPt (x, y) dir) visited) [1..4]
    visited' = S.union visited $ S.fromList $ map (\dir -> nextPt (x, y) dir) testDirections
  in
    case runProgOnInputs state testDirections of
      Right s' -> Right s'
      Left more_states -> output (states ++ more_states) visited' 

oxygen :: String -> State
oxygen contents =
  let
    prog = stringToProg contents
    answer = output [((0, 0, prog), (0, 0), 0)] S.empty
  in
    case answer of
      Left _ -> error "Not found the oxygen pt"
      Right state@((ip, base, prog), pt, d) -> state

answer1 :: String -> String
answer1 contents = show d
  where
    state@(_, _, d) = oxygen contents

output2 :: [State] -> S.Set (Integer, Integer) -> Integer -> Integer
output2 [] _ current_max = current_max
output2 (state@((ip, base, prog), (x, y), d):states) visited current_max
  = let
  directionsToTry = filter (\dir -> S.notMember (nextPt (x, y) dir) visited) [1..4]
  visited' = S.union visited $ S.fromList $ map (\dir -> nextPt (x, y) dir) directionsToTry
  in
    case directionsToTry of
      [] -> output2 states visited' (max current_max d)
      _ -> let
        new_states = catMaybes $ map (\dir -> exec dir) directionsToTry
        exec dir = do
          (ip', base', output, prog') <- runProg ip base [dir] prog
          output_val <- output
          case output_val of
            1 -> Just ((ip', base', prog'), (nextPt (x, y) dir), (d + 1))
            0 -> Nothing
            2 -> error "Should never happen"
        in
          output2 (states ++ new_states) visited' (max current_max d)

answer2 :: String -> String
answer2 contents = 
  let
    start@((ip, base, prog), (x, y), d) = oxygen contents
  in
    show $ output2 [((ip, base, prog), (x, y), 0)] (S.singleton (x, y)) 0

testOxygen :: String -> String
testOxygen contents =
  let
    start@((ip, base, prog), (x, y), d) = oxygen contents
    one@(_, _, o_one, _) = fromJust $ runProg ip base [1] prog
    two@(_, _, o_two, _) = fromJust $ runProg ip base [2] prog
    three@(_, _, o_three, _) = fromJust $ runProg ip base [3] prog
    four@(_, _, o_four, _) = fromJust $ runProg ip base [4] prog
  in
    show (o_one, o_two, o_three, o_four)


stringToProg :: String -> M.Map Integer Integer
stringToProg str = let
  values = map (read :: String -> Integer) $ splitOn "," str
  prog = M.fromList $ zip [0..] values
  in prog

toIntCode :: Integer -> [Integer]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

{- We start at the origin (location of the robot), and test all directions using the same program state (ip, base, M.Map). If the output is zero, this is a wall. Store the point as already visited. If the value is 1, we add the updated program state and new position to the list of entries to try. For every change in pos, we can increment the distance travelled by 1. Once we get the output 2, we can return the distance.
-}

runProg :: Integer -> Integer -> [Integer] -> M.Map Integer Integer -> Maybe (Integer, Integer, Maybe Integer, M.Map Integer Integer)
runProg pos base inputs prog = case (toIntCode <$> M.lookup pos prog) of
  Just [_, _, _, 9, 9] -> pure (pos, base, Nothing, prog)

  Just [_, _, c, 0, 3] -> do 
    loc <- getLocByMode c (pos + 1) base prog
    (input, rest) <- uncons inputs
    runProg (pos + 2) base rest $ M.insert loc input prog

  Just [_, _, c, 0, 4] -> do
    val <- getValByMode c (pos + 1) base prog
    pure ((pos + 2), base, Just val, prog)

  Just [_, b, c, 0, 5] -> do
    val <- getValByMode c (pos + 1) base prog
    ip' <- getValByMode b (pos + 2) base prog
    if (val /= 0) then
      runProg ip' base inputs prog
    else
      runProg (pos + 3) base inputs prog

  Just [_, b, c, 0, 6] -> do
    val <- getValByMode c (pos + 1) base prog
    ip' <- getValByMode b (pos + 2) base prog
    if (val == 0) then
      runProg ip' base inputs prog
    else
      runProg (pos + 3) base inputs prog

  Just [a, b, c, 0, 7] -> do
    v1 <- getValByMode c (pos + 1) base prog
    v2 <- getValByMode b (pos + 2) base prog
    loc <- getLocByMode a (pos + 3) base prog
    if (v1 < v2) then
      runProg (pos + 4) base inputs $ M.insert loc 1 prog
    else
      runProg (pos + 4) base inputs $ M.insert loc 0 prog

  Just [a, b, c, 0, 8] -> do
    v1 <- getValByMode c (pos + 1) base prog
    v2 <- getValByMode b (pos + 2) base prog
    loc <- getLocByMode a (pos + 3) base prog
    if (v1 == v2) then
      runProg (pos + 4) base inputs $ M.insert loc 1 prog
    else
      runProg (pos + 4) base inputs $ M.insert loc 0 prog

  Just [a, b, c, 0, 9] -> do
    rel <- getValByMode c (pos + 1) base prog
    runProg (pos + 2) (base + rel) inputs prog

  Just[a, b, c, 0, op] -> do
    v1 <- getValByMode c (pos + 1) base prog
    v2 <- getValByMode b (pos + 2) base prog
    out_pos  <- getLocByMode a (pos + 3) base prog
    let prog' = M.insert out_pos (getOp op v1 v2) prog
    runProg (pos + 4) base inputs prog'

  Just x -> error $ "We should not be here" ++ show (pos, x)

type ProgState = (Integer, Integer, M.Map Integer Integer)

getOp :: Integer -> Integer -> Integer -> Integer
getOp 1 = (+)
getOp 2 = (*)
getOp code = error $ "Invalid Op" ++ (show code)

getValByMode :: Integer -> Integer -> Integer -> M.Map Integer Integer -> Maybe Integer
getValByMode mode pos base m = do
  loc <- getLocByMode mode pos base m
  M.lookup loc m <|> Just 0
  
getLocByMode :: Integer -> Integer -> Integer -> M.Map Integer Integer -> Maybe Integer
getLocByMode mode pos base m = case mode of
  0 -> do
    loc <- M.lookup pos m
    pure loc
  1 -> pure pos
  2 -> do
    loc <- M.lookup pos m
    pure $ base + loc
  _ -> error $ "Invalid mode " ++ (show mode)

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer2 contents)
--  putStrLn (testOxygen contents)
--  putStrLn (answer1 contents)
