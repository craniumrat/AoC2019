module D1 where

import System.Environment
import Debug.Trace (trace)

-- https://adventofcode.com/2019/day/1

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

getFuel :: Integer -> Integer
getFuel n = if (f <= 0) then 0 else f
  where
    f = n `div` 3 - 2

answer1 :: String -> String
answer1 contents = show $ sum $ map getFuel $ map (read :: String -> Integer) $ lines contents

getProperFuel :: Integer -> Integer
getProperFuel n = if (n <= 0) then 0
  else
    f'
  where
    f = getFuel n
    f' = f + getProperFuel f

answer :: (Integer -> Integer) -> String -> String
answer f contents = show $ sum $ map f $ map (read :: String -> Integer) $ lines contents
  
main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
--  putStrLn (answer getFuel contents) 
  putStrLn (answer getProperFuel contents)
