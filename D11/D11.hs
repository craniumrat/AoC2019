module D11 where

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

{- We start with an empty map of points. All subsequent values of points if not present in map are treated as 0. We have to repeatedly runProg, passing in the value from map, and then the next couple of output instructions. Update the position and direction of the robot subsequently  -}

data Direction = N | E | S | W deriving (Show, Eq, Ord)

instance Enum Direction where
  succ N = E
  succ E = S
  succ S = W
  succ W = N

  pred N = W
  pred W = S
  pred S = E
  pred E = N

  toEnum n | n == 0 = N
           | n == 1 = E
           | n == 2 = S
           | n == 3 = W
  toEnum _ = error $ "Bad ordering"

  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

-- runPainter (M.Map <position> <color at position>, (robot pos, robot current direction), program state) -> Maybe same
-- returning Nothing means stop execution.
runPainter :: M.Map (Integer, Integer) Integer -> ((Integer, Integer), Direction) -> ProgState -> Maybe (M.Map (Integer, Integer) Integer)
runPainter m (pos, d) (ip, base, prog) = do
  color <- M.lookup pos m <|> Just 0
  (ip', base', output1, prog') <- runProg ip base [color] prog
  if (length output1 == 0) then
    pure m
  else do
    (color', _) <- uncons output1
    (ip'', base'', [output2], prog'') <- runProg ip' base' [] prog'
    let m'' = M.insert pos color' m
    (pos', d') <- movePainter (pos, d) output2
    runPainter m'' (pos', d') (ip'', base'', prog'')

movePainter :: ((Integer, Integer), Direction) -> Integer -> Maybe ((Integer, Integer), Direction)
movePainter ((x, y), d) rot = pure $ ((x', y'), d')
  where
    d' = if (rot == 0) then pred d else succ d
    (x', y') = case d' of
      N -> (x, y - 1)
      S -> (x, y + 1)
      E -> (x + 1, y)
      W -> (x - 1, y)
      
answer1 :: String -> String
answer1 contents = let
    prog = stringToProg contents
    output = runPainter M.empty ((0, 0), N) (0, 0, prog)
    answer = M.size <$> output
  in
    show answer

stringToProg :: String -> M.Map Integer Integer
stringToProg str = let
  values = map (read :: String -> Integer) $ splitOn "," str
  prog = M.fromList $ zip [0..] values
  in prog

toIntCode :: Integer -> [Integer]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

answer2 :: String -> [String]
answer2 contents = let
    prog = stringToProg contents
    output = runPainter (M.singleton (0, 0) 1)  ((0, 0), N) (0, 0, prog)
    lines = fromJust $ chunksOf 7  <$> pixel <$> output -- We have to convert this map into an image
  in
    lines

pixel m = do
  x <- [0..43]
  y <- [0..6]
  let val = case M.lookup (x, y) m of
        Nothing -> ' '
        Just 0 -> ' '
        Just 1 -> '#'
  pure val
        
  
runProg :: Integer -> Integer -> [Integer] -> M.Map Integer Integer -> Maybe (Integer, Integer, [Integer], M.Map Integer Integer)
runProg pos base inputs prog = case (toIntCode <$> M.lookup pos prog) of
  Just [_, _, _, 9, 9] -> pure (pos, base, [], prog)

  Just [_, _, c, 0, 3] -> do 
    loc <- getLocByMode c (pos + 1) base prog
    (input, rest) <- uncons inputs
    runProg (pos + 2) base rest $ M.insert loc input prog

  Just [_, _, c, 0, 4] -> do
    val <- getValByMode c (pos + 1) base prog
    pure ((pos + 2), base, [val], prog)

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
  mapM_ putStrLn (answer2 contents)  
--  putStrLn (answer1 contents)
