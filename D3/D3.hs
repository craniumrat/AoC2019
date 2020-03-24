
module D3 where

import System.Environment
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, digit, satisfy)
import Text.Parsec (parse)
import Text.Parsec.Combinator (many1)
import Control.Monad (void, liftM2)
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (sortBy, unfoldr, sort)
import qualified Data.Map.Strict as M

data Direction = D Integer | U Integer | L Integer | R Integer deriving (Eq, Show)

-- toDirection :: Parser Direction
-- toDirection =
--   (do
--     void $ char 'D'
--     n <- many1 digit
--     pure $ D (read n))

toDirection :: Parser Direction
toDirection = (char 'D' *> pure D <*> var) <|>
  (char 'U' *> pure U <*> var) <|>
  (char 'L' *> pure L <*> var) <|>
  (char 'R' *> pure R <*> var)
  where
    var :: Parser Integer
    var = read <$> many1 digit

parseDirection :: Parser Direction -> String -> Direction
parseDirection p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right dir -> dir

parseLine :: String -> [Direction]
parseLine line = dirs
  where
    elements = splitOn "," line
    dirs = parseDirection toDirection <$> elements

toCoords :: [(Int, Int)] -> Direction -> [(Int, Int)]
toCoords path (D 0) = path
toCoords path (U 0) = path
toCoords path (L 0) = path
toCoords path (R 0) = path
toCoords path (D n) = toCoords (path ++ [getNextD (last path)]) $ D (n - 1)
toCoords path (U n) = toCoords (path ++ [getNextU (last path)]) $ U (n - 1)
toCoords path (L n) = toCoords (path ++ [getNextL (last path)]) $ L (n - 1)
toCoords path (R n) = toCoords (path ++ [getNextR (last path)]) $ R (n - 1)

getNextD (x, y) = (x, y+1)
getNextU (x, y) = (x, y-1)
getNextL (x, y) = (x-1, y)
getNextR (x, y) = (x+1, y)

{--- This works, but it causes a
   fromList D3.hs: internal error: getMBlock: mmap: Invalid argument
       (GHC version 8.4.4 for arm_unknown_linux)
       Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
   Aborted (core dumped)

   Most likely because of too much data. Right now, we maintain even duplicate co-ordinates in the list, but we shouldn't need to.
-}
-- answer1 :: String -> String
-- answer1 contents = (show min_dist)
--   where
--     [l1, l2] = lines contents
--     l1_values = parseLine l1
--     l2_values = parseLine l2
--     l1_coords = S.fromList $ tail $ foldl (\acc dir -> toCoords acc dir) [(0, 0)] l1_values
--     l2_coords = S.fromList $ tail $ foldl (\acc dir -> toCoords acc dir) [(0, 0)] l2_values
--     d = S.toList $ l1_coords `S.intersection` l2_coords
--     (min_x, min_y) = head $ sortBy (\(x1, y1) (x2, y2) -> ((abs x1) + (abs y1)) `compare` ((abs x2) + (abs y2))) d
--     min_dist = abs min_x + abs min_y

toCoordsSet :: (S.Set (Int, Int), (Int, Int)) -> Direction -> (S.Set (Int, Int), (Int, Int))
toCoordsSet (s, n) (D 0) = (s, n)
toCoordsSet (s, n) (U 0) = (s, n)
toCoordsSet (s, n) (L 0) = (s, n)
toCoordsSet (s, n) (R 0) = (s, n)
toCoordsSet (s, n@(x, y)) (D m) = toCoordsSet ((S.insert (x, y+1) s), (x, y+1)) $ D (m - 1)
toCoordsSet (s, n@(x, y)) (U m) = toCoordsSet ((S.insert (x, y-1) s), (x, y-1)) $ U (m - 1)
toCoordsSet (s, n@(x, y)) (L m) = toCoordsSet ((S.insert (x-1, y) s), (x-1, y)) $ L (m - 1)
toCoordsSet (s, n@(x, y)) (R m) = toCoordsSet ((S.insert (x+1, y) s), (x+1, y)) $ R (m - 1)

-- Round 2 ---- Insert entries into set as soon you determine the next co-ordinate. We don't need the path to the end, we just need all unique co-ordinates

answer1 :: String -> String
answer1 contents = show min_dist
  where
    [l1, l2] = lines contents
    l1_values = parseLine l1
    l2_values = parseLine l2
    l1_coords = foldl (\acc dir -> toCoordsSet acc dir) (S.empty, (0, 0)) l1_values
    l2_coords = foldl (\acc dir -> toCoordsSet acc dir) (S.empty, (0, 0)) l2_values
    d = S.toList $ (fst l1_coords) `S.intersection` (fst l2_coords)
    (min_x, min_y) = head $ sortBy (\(x1, y1) (x2, y2) -> ((abs x1) + (abs y1)) `compare` ((abs x2) + (abs y2))) d
    min_dist = abs min_x + abs min_y

toCoordsSet2 :: [Direction] -> S.Set (Int, Int)
toCoordsSet2 dirs = S.fromList $ unfoldr step (dirs, (0, 0))
  where
    step ([], p) = Nothing
    step (U 0:rest, p) = step (rest, p)
    step (U n:rest, p@(x, y)) = Just ((x, y - 1), (U (n - 1):rest, (x, y - 1)))
    step (D 0:rest, p) = step (rest, p)
    step (D n:rest, p@(x, y)) = Just ((x, y + 1), (D (n - 1):rest, (x, y + 1)))
    step (L 0:rest, p) = step (rest, p)
    step (L n:rest, p@(x, y)) = Just ((x - 1, y), (L (n - 1):rest, (x - 1, y)))
    step (R 0:rest, p) = step (rest, p)
    step (R n:rest, p@(x, y)) = Just ((x + 1, y), (R (n - 1):rest, (x + 1, y)))

--- I would love to use unfoldr, but a) there is no way to use M.fromList so that it ignores values if key is already present, and b) I want to be able to make a single pass through all Directions instead of making a list and then converting to map
-- We need to keep track of the map, the current node and the current distance from origin. Using the current direction, we can insert a new node and distance and update current node.

type MapWithCurrentNode = (M.Map (Int, Int) Int, (Int, Int), Int)

toCoordsMap :: [Direction] -> MapWithCurrentNode -> MapWithCurrentNode
toCoordsMap [] m = m
toCoordsMap (D 0:rest) m = toCoordsMap rest m
toCoordsMap (D n:rest) (m, (x, y), d) = toCoordsMap (D (n - 1):rest) ((M.insertWith (flip const) (x, y) d) m, (x, y + 1), d + 1)
toCoordsMap (U 0:rest) m = toCoordsMap rest m
toCoordsMap (U n:rest) (m, (x, y), d) = toCoordsMap (U (n - 1):rest) ((M.insertWith (flip const) (x, y) d) m, (x, y - 1), d + 1)
toCoordsMap (R 0:rest) m = toCoordsMap rest m
toCoordsMap (R n:rest) (m, (x, y), d) = toCoordsMap (R (n - 1):rest) ((M.insertWith (flip const) (x, y) d) m, (x + 1, y), d + 1)
toCoordsMap (L 0:rest) m = toCoordsMap rest m
toCoordsMap (L n:rest) (m, (x, y), d) = toCoordsMap (L (n - 1):rest) ((M.insertWith (flip const) (x, y) d) m, (x - 1, y), d + 1)

---- We use unfoldr to create a (node, distance) pair and use that to insert into a map.
-- Once a value is inserted into the map, any subsequent entry is going to be a larger value, so 

answer2 :: String -> String    
answer2 contents = show distances
  where
    [l1, l2] = lines contents
    l1_values = parseLine l1
    l2_values = parseLine l2
    (m1, _, d1) = toCoordsMap l1_values (M.empty, (0, 0), 0)
    (m2, _, d2) = toCoordsMap l2_values (M.empty, (0, 0), 0)
    s1 = M.keysSet m1
    s2 = M.keysSet m2
    d = S.toList $ S.delete (0, 0) $ s1 `S.intersection` s2
    distances = head $ sort $ (\pt -> liftM2 (+) (M.lookup pt m1) (M.lookup pt m2)) <$> d
    
main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
--  putStrLn $ answer1 contents
  putStrLn $ answer2 contents
