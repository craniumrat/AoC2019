module D13 where

import qualified Data.Map as M
import System.Environment
import Data.List.Split
import Debug.Trace (trace)
import Data.List (uncons, unfoldr)
import Data.Digits (digits)
import Control.Monad (guard, void)
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

{- Since the outputs are going be a triple at a time till the end, it's better to collect them one at a time. We are going to modify the program to return a single value every time.  -}

output1 ip base prog = unfoldr step (ip, base, prog)
  where
    step :: (Integer, Integer, M.Map Integer Integer) -> Maybe (Integer, (Integer, Integer, M.Map  Integer Integer))
    step (ip, base, prog) = do
      (ip', base', val, prog') <- runProg ip base [] prog
      case val of
        Nothing -> Nothing
        Just v -> Just (v, (ip', base', prog'))
      
answer1 :: String -> String
answer1 contents = let
    prog = stringToProg contents
    l = length $ filter (\[_, _, ob] -> ob == 2) $ chunksOf 3 $ output1 0 0 prog
  in
    show l

stringToProg :: String -> M.Map Integer Integer
stringToProg str = let
  values = map (read :: String -> Integer) $ splitOn "," str
  prog = M.fromList $ zip [0..] values
  in prog

toIntCode :: Integer -> [Integer]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

{- Instead of getting a fixed board, we start off with an initial board by setting the pos 0 of the prog to value 2. That will result in a board along with a score given by the triple (-1, 0, score). Based on the position of the paddle, we have to provide an input of one of [-1, 0, 1] to indicate that the joystick moved left, stayed in the same place or right. We play the game until there are no block tiles. Then we get the final score.

We have to keep track of 3 different values in the triplets:
1. The number of blocks. The game stops when the block count is 0.
2. The paddle x position. We will use it to calculate the direction the joystick iis supposed to move. Basically reduce the distance between the tiles.
3. The score.
-}
--           (Maybe ballX, Maybe paddleX, Maybe score)        
type State = (Maybe Integer, Maybe Integer, Maybe Integer)

updateState :: (Integer, Integer, Integer) -> State -> State
updateState ((-1), _, score) (bx, px, _) = (bx, px, Just score)
updateState (bx, _, 4) (_, Just px, score) = (Just bx, Just px, score)
updateState (px, _, 3) (Just bx, _, score) = (Just bx, Just px, score)
updateState _ state = state

nextInput :: State -> Integer
nextInput ((Just bx), (Just px), _) = if (bx < px) then (-1) else if (bx > px) then 1 else 0
nextInput _ = 0

getScore :: State -> Integer
getScore (_, _, Just score) = score
    
output2 prog = unfoldr step ((Just 0, Just 0, Just 0), (0, 0, prog))
  where
--    step :: (State, (Integer, Integer, M.Map Integer Integer)) -> Maybe (Integer, (State, (Integer, Integer, M.Map  Integer Integer)))
    step (state, (ip, base, prog')) = do
      let i = [nextInput state]
      (ip1, base1, o1, prog1) <- runProg ip base i  prog'
      x <- o1
      (ip2, base2, o2, prog2) <- runProg ip1 base1 i prog1
      y <- o2
      (ip3, base3, o3, prog3) <- runProg ip2 base2 i prog2
      ob <- o3
      let state' = updateState (x, y, ob) state
      let score = getScore state'
      pure $ (score, (state', (ip3, base3, prog3)))

answer2 :: String -> String
answer2 contents = let
    prog = stringToProg contents
    answer = last $ output2 $ M.insert 0 2 prog
  in
    show answer

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
--   putStrLn (answer1 contents)
