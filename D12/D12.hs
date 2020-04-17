module D12 where

import System.Environment
import Debug.Trace (trace)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string)
import Text.ParserCombinators.Parsec.Numeric (int)
import Text.Parsec (parse)
import Control.Monad (void)
import Data.List (zipWith6)
import qualified Data.Set as S

traceShow' a = trace (show a) a

data Position = Position (Integer, Integer, Integer) deriving (Show, Eq) --- (x, y, z)
data Velocity = Velocity (Integer, Integer, Integer) deriving (Show, Eq) --- (dx, dy, dz)

toPosition :: Parser Position
toPosition = do
  void $ string "<x="
  x <- int
  void $ string ", y="
  y <- int
  void $ string ", z="
  z <- int
  pure $ Position (x, y, z)

parsePosition :: String -> Position
parsePosition s = case parse toPosition "" s of
  Left e -> error $ "Parse error at: " ++ (show e)
  Right position -> position

--- Since positions and velocities on an axis are independant of each other,
--- we will make functions using just values on a single axis.

toX :: Position -> Integer
toX (Position (x, _, _)) = x

toY :: Position -> Integer
toY (Position (_, y, _)) = y

toZ :: Position -> Integer
toZ (Position (_, _, z)) = z

toDX :: Velocity -> Integer
toDX (Velocity (x, _, _)) = x

toDY :: Velocity -> Integer
toDY (Velocity (_, y, _)) = y

toDZ :: Velocity -> Integer
toDZ (Velocity (_, _, z)) = z

deltaV :: Integer -> Integer -> Integer
deltaV a1 a2 = if (a1 > a2) then -1 else if (a1 == a2) then 0 else 1

-- Apply the function for each argument against the other arguments
update :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer]
update f xs = xs'
  where
    xs' = do
      x <- xs
      x'' <- pure $ sum $ map (\x' -> f x x') xs
      pure x''

deltaP :: Integer -> Integer -> Integer
deltaP = (+)

energy :: (Position, Velocity) -> Integer
energy ((Position (x, y, z)), (Velocity (dx, dy, dz))) = (sum $ map (abs) [x, y, z]) * (sum $ map (abs) [dx, dy, dz])

toPosVel :: Position -> (Position, Velocity)
toPosVel p = (p, Velocity (0, 0, 0))

step :: [(Position, Velocity)] -> [(Position, Velocity)]
step posvels = posvels'
  where
    xs = map (toX.fst) posvels
    ys = map (toY.fst) posvels
    zs = map (toZ.fst) posvels
    dxs = map (toDX.snd) posvels
    dys = map (toDY.snd) posvels
    dzs = map (toDZ.snd) posvels
    dxs' = zipWith (+) dxs $ update deltaV xs
    dys' = zipWith (+) dys $ update deltaV ys
    dzs' = zipWith (+) dzs $ update deltaV zs
    xs' = zipWith (+) xs dxs'
    ys' = zipWith (+) ys dys'
    zs' = zipWith (+) zs dzs'
    posvels' = zipWith6 (\x y z dx dy dz -> (Position (x, y, z), Velocity (dx, dy, dz))) xs' ys' zs' dxs' dys' dzs'

-- energyAt turn -> energy
stepAt :: Integer -> [(Position, Velocity)] -> [(Position, Velocity)]
stepAt 0 posvels = posvels
stepAt n posvels = stepAt (n - 1) (step posvels)

answer1 :: String -> String
answer1 contents = show $ sum $ map energy $ stepAt 1000 $ map (toPosVel.parsePosition) $ lines contents

{-
  The intuition for part 2 is that its going to take an inredibly large number of steps to a point where x, y, z and dx, dy and dz are the same. Since x and dx and independant of the rest, we can find the point at which x and dx repeat. Compute the point at which y and dy repeat. The answer will be the LCM of these numbers 
-}

stepSingle :: [(Integer, Integer)] -> [(Integer, Integer)]
stepSingle posvels = posvels'
  where
    xs = map (fst) posvels
    dxs = map (snd) posvels
    dxs' = zipWith (+) dxs $ update deltaV xs
    xs' = zipWith (+) xs dxs'
    posvels' = zipWith (,) xs' dxs'

checkRepeat :: Integer -> [(Integer, Integer)] -> S.Set [(Integer, Integer)] -> Integer
checkRepeat n posvels s = if S.member posvels s then n
  else
     checkRepeat (n + 1) (stepSingle posvels) $ S.insert posvels s

answer2 :: String -> String
answer2 contents =  show $ lcm (lcm x_cnt y_cnt) z_cnt
  where
    posvels = map (toPosVel.parsePosition) $ lines contents
    xs = map (toX.fst) posvels
    ys = map (toY.fst) posvels
    zs = map (toZ.fst) posvels
    dxs = map (toDX.snd) posvels
    dys = map (toDY.snd) posvels
    dzs = map (toDZ.snd) posvels
    x_dxs = zipWith (,) xs dxs
    y_dys = zipWith (,) ys dys
    z_dzs = zipWith (,) zs dzs
    x_cnt = checkRepeat 0 x_dxs S.empty
    y_cnt = checkRepeat 0 y_dys S.empty
    z_cnt = checkRepeat 0 z_dzs S.empty
    

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer2 contents)  
--  putStrLn (answer1 contents)
