module D17 where

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
import Data.Char (chr)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

{- The outputs are going be a single at a time till the end. We are going to modify the program to return a single value every time.  -}

neighbours :: Integer -> Integer -> [(Integer, Integer) -> (Integer, Integer)]
neighbours x_max y_max = [(\(x, y) -> ((x + 1) `mod` x_max, y)), (\(x, y) -> ((x - 1) `mod` x_max, y)), (\(x, y) -> (x, (y + 1) `mod` y_max)), (\(x, y) -> (x, (y - 1) `mod` y_max))]

-- 35 (#), 60 (<), 62 (>), 94 (^), 118 (v)
isScaffold x = x == 35 || x == 60 || x == 62 || x == 94 || x == 118

x_max = 39
y_max = 29

isIntersection :: M.Map (Integer, Integer) Int -> (Integer, Integer) -> Integer -> Integer -> Bool
isIntersection m (x, y) x_max y_max = M.lookup (x, y) m == Just 35 && (all check $ map (\f -> f (x, y)) $ neighbours x_max y_max)
  where
    check :: (Integer, Integer) -> Bool
    check = \(a, b) -> fromJust $ isScaffold <$> M.lookup (a, b) m

mapGrid :: [Int] -> M.Map (Integer, Integer) Int
mapGrid ints = M.fromList values
  where
    col_rows = zip [0..] (splitOn [10] ints)
    values = concat $ map(\(c, row) -> zipWith(\r d -> ((c, r), d)) [0..] row) col_rows

showGrid :: M.Map (Integer, Integer) Int -> String
showGrid grid = fromJust $ sequence $ do
  x <- [0 .. (x_max - 1)]
  y <- [0 .. (y_max - 1)]
  let val = M.lookup (x, y) grid
  pure $ chr <$> val

getGrid contents = unfoldr step (0, 0, stringToProg contents)
  where
    step :: (Integer, Integer, M.Map Integer Integer) -> Maybe (Int, (Integer, Integer, M.Map Integer Integer))
    step (ip, base, prog) = do
      (ip', base', _, output, prog') <- runProg ip base [] prog
      o <- output
      pure (fromIntegral $ o, (ip', base', prog'))
      
answer1 :: String -> String
answer1 contents = show answer
  where
    grid = mapGrid.getGrid $ contents
    intersections = M.filterWithKey (\k v -> isIntersection grid k x_max y_max) grid
    alignment = M.foldlWithKey(\acc (x, y) _ -> acc + (x * y)) 0 intersections
    answer = alignment

{-
   Sigh.. just found this by hand. Look at path.txt
-}
m = [65,44,66,44,65,44,67,44,65,44,66,44,67,44,66,44,67,44,66,10]
a = [82,44,49,48,44,82,44,49,48,44,82,44,54,44,82,44,52,10]
b = [82,44,49,48,44,82,44,49,48,44,76,44,52,10]
c = [82,44,52,44,76,44,52,44,76,44,49,48,44,76,44,49,48,10]
n = [110,10]
y = [121, 10]

-- 35 (#), 60 (<), 62 (>), 94 (^), 118 (v)
-- rotate 82 ((x, y), 60) = ((x, y), 94)
-- rotate 82 ((x, y), 94) = ((x, y), 62)
-- rotate 82 ((x, y), 62) = ((x, y), 118)
-- rotate 82 ((x, y), 118) = ((x, y), 60)
-- rotate 76 ((x, y), 60) = ((x, y), 118)
-- rotate 76 ((x, y), 118) = ((x, y), 62)
-- rotate 76 ((x, y), 62) = ((x, y), 94)
-- rotate 76 ((x, y), 94) = ((x, y), 60)

-- move 0 ((x, y), 60) = ((x, y), 60)
-- move 0 ((x, y), 94) = ((x, y), 94)
-- move 0 ((x, y), 62) = ((x, y), 62)
-- move 0 ((x, y), 118) = ((x, y), 118)
-- move d ((x, y), 60) = ((x, y), 60)
-- move d ((x, y), 94) = ((x, y), 94)
-- move d ((x, y), 62) = ((x, y), 62)
-- move d ((x, y), 118) = ((x, y), 118)

-- testPathOnGrid :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> M.Map (Integer, Integer) Integer -> Bool
-- testPathOnGrid main a b c grid = False
--   where
--     remove_commas fn = filter(\c -> c /= 44 && c /= 10) fn
--     moves = remove_commas $ concat $ map (\f -> if f == 65 then a else if f == 66 then b else if f == 67 then c else error "Invalid function") $ remove_commas main
--     move c ((x, y), d) = case c of
--       82 -> right 

inputForAnswer2 :: [Integer]
inputForAnswer2 = m ++ a ++ b ++ c ++ n

{- The IntCode program outputs the starting grid, the query for the Main, A, B, C functions and the final grid, along with the output non-ASCII dust value. To visualize all this, we should output the String, which will show all this, except the output value. To get the actual output value, return the Integer array. The output will be the one before the final two [10, 10] value -}

answer2 :: String -> [Integer]
answer2 contents = unfoldr step (0, 0, inputForAnswer2, (M.insert 0 2 $ stringToProg contents))
  where
    step :: (Integer, Integer, [Integer], M.Map Integer Integer) -> Maybe (Integer, (Integer, Integer, [Integer], M.Map Integer Integer))
    step (ip, base, inputs, prog) = do
      (ip', base', inputs', output, prog') <- runProg ip base inputs prog
      c <- output
      pure $ (c, (ip', base', inputs', prog'))

    
stringToProg :: String -> M.Map Integer Integer
stringToProg str = let
  values = map (read :: String -> Integer) $ splitOn "," str
  prog = M.fromList $ zip [0..] values
  in prog

toIntCode :: Integer -> [Integer]
toIntCode i = replicate (5 - length d) 0 ++ d
  where
    d = digits 10 i

-- Changing the program to output remaining inputs. We can feed it right back in
runProg :: Integer -> Integer -> [Integer] -> M.Map Integer Integer -> Maybe (Integer, Integer, [Integer], Maybe Integer, M.Map Integer Integer)
runProg pos base inputs prog = case (toIntCode <$> M.lookup pos prog) of
  Just [_, _, _, 9, 9] -> pure (pos, base, inputs, Nothing, prog)

  Just [_, _, c, 0, 3] -> do 
    loc <- getLocByMode c (pos + 1) base prog
    (input, rest) <- uncons inputs
    runProg (pos + 2) base rest $ M.insert loc input prog

  Just [_, _, c, 0, 4] -> do
    val <- getValByMode c (pos + 1) base prog
    pure ((pos + 2), base, inputs, Just val, prog)

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
  putStrLn (show $ answer2 contents)
--  putStrLn (answer1 contents)
