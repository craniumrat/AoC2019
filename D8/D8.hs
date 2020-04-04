module D8 where

import System.Environment
import Data.List.Split (chunksOf)
import Debug.Trace (trace)
import Data.List (uncons, unfoldr, minimumBy)
import Data.Char (digitToInt, isSpace)
import Control.Monad (guard)
import Data.Function (on)
import qualified Data.Set as S

answer1 :: String -> String
answer1 contents = show answer
  where
    layers = chunksOf (25 * 6) $ map (digitToInt) $ takeWhile (not.isSpace) contents
    minlayer = snd $ minimumBy (compare `on` fst) $ map (\layer -> (countVals layer 0, layer)) layers
    answer = (countVals minlayer 1) * (countVals minlayer 2)

countVals :: [Int] -> Int -> Int
countVals vals val= foldl (\count x -> if x == val then count + 1 else count) 0 vals

answer2 :: String -> [String]
answer2 contents = finalLayer
  where
    layers = chunksOf (25 * 6) $ map (digitToInt) $ takeWhile (not.isSpace) contents
    finalLayer = map (map encodePixel) $ chunksOf 25 $ foldl1 (\layer1 layer2 -> map (decodePixel) $ zip layer1 layer2) layers

encodePixel :: Int -> Char
encodePixel 0 = '.'
encodePixel 1 = 'X'

decodePixel :: (Int, Int) -> Int
decodePixel (2, x) = x
decodePixel (x, _) = x
    
main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  mapM_ putStrLn (answer2 contents)  
--  putStrLn (answer1 contents)
