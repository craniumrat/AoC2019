module D10 where

import System.Environment
import Debug.Trace (trace)
import Control.Monad (guard, join)
import Data.Function (on)
import qualified Data.Set as S
import Data.List (sortBy, groupBy, transpose)

answer1 :: String -> String
answer1 contents = show $ S.findMax angles
  where
    s = S.fromList $ toCoords contents
    angles = S.map (\pt -> length $ toAngleSet s pt) s

{- This is not sufficient... consider an asteroid at offset (a, b) and (-a, -b) from origin. In both cases, the angle will be the same), but the two do not hide each other. The correct way is to also keep the quadrant of the point relative to "origin" in the set
toAngleSetOld :: S.Set (Integer, Integer) -> (Integer, Integer) -> S.Set Double
toAngleSetOld s (x1, y1) = S.map (\(x2, y2) -> angle (x1, y1) (x2, y2)) $ S.delete (x1, y1) s
-}

data Quadrant = PN | PP | NP | NN deriving (Show, Eq, Ord)

toAngleSet :: S.Set (Integer, Integer) -> (Integer, Integer) -> S.Set (Double, Quadrant)
toAngleSet s (x1, y1) = S.map (\(x2, y2) -> angle (x1, y1) (x2, y2)) $ S.delete (x1, y1) s

angle :: (Integer, Integer) -> (Integer, Integer) -> (Double, Quadrant)
angle origin@(x1, y1) (x2, y2) = (a, q)
  where
  a = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)
  q = if (x2 >= x1) && (y2 >= y1) then PN
    else if (x2 >= x1) && (y2 < y1) then PN
    else if (x2 < x1) && (y2 >= y1) then NP
    else NN

toCoords content = do
  (line, y) <- zip (lines content) [0..]
  (c, x) <- zip line [0..]
  guard $ c == '#'
  pure (x, y)

{-
  First we have to find the point where to set up the laser station. This is the coord where the max. asteroids are visible.
  For answer2, we need the distance along with the angle and quadrant.
  We then have to sort the points by angle and then for points that share the same angle, by the distance. We get the answer by !!200
-}
answer1Pt :: String -> String
answer1Pt contents =
  show $ S.filter (\(pt, cnt) -> cnt == 210) angles
  --show s
  where
    s = S.fromList $ toCoords contents
    angles = S.map (\pt -> (pt, length $ toAngleSet s pt)) s


origin = (26, 28)
-- origin = (11, 13) for sample.txt.

atan2ToClockwise :: Double -> Double
atan2ToClockwise angle =
  if (angle >= 0) && (angle < pi / 2) then
    (pi / 2 + angle)
  else if (angle <= 0 && angle >= ((-pi) / 2)) then
    (pi / 2 + angle)
  else if (angle <= ((-pi)/2) && angle > (-pi)) then
    (5 * pi/2 + angle)
  else if (angle == pi) then
    3 * pi / 2
  else
    pi/2 + angle -- angle >= pi/2 && angle <= pi

polar :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Double)
polar origin@(x1, y1) pt@(x2, y2) = (d, a)
  where
    d = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    a = atan2ToClockwise $ atan2 (fromIntegral (y2 - y1)) (fromIntegral (x2 - x1)) --- Range is from -pi to pi. We want range from 0 to 2 * pi, where 0 == pi / 2 (North).
    

answer2 :: String -> String
answer2 contents = show $ answer
  where
    coords = filter (/= origin) $ toCoords contents
--    ordered = join $ transpose $ map (sortBy (compare `on` fst.snd)) $ groupBy ((==) `on` snd.snd) $ sortBy (compare `on` snd.snd) $ map (\pt -> (pt, polar origin pt)) coords
    ordered = join $ transpose $ map (sortBy (compare `on` fst.snd)) $ groupBy ((==) `on` snd.snd) $ sortBy (compare `on` snd.snd) $ map (\pt -> (pt, polar origin pt)) coords
    answer = (fst <$> ordered) !! 199
    
main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer2 contents)  
--  putStrLn (answer1Pt contents)
--  putStrLn (answer1 contents)
