module D4 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit)
import Text.Parsec (parse)
import Text.Parsec.Combinator (many1)
import Control.Monad (void, guard)
import System.Environment
import Data.List (sort, group)

data Input = Input Integer Integer  deriving Show -- Input start end

toInput :: Parser Input
toInput = do
  start <- many1 digit
  void $ char '-'
  end <- many1 digit
  pure $ Input (read start) (read end)

parseInput :: String -> Input
parseInput str = case parse toInput "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right input -> input

twoAdjacent :: Integer -> Bool
twoAdjacent = go. show
  where
    go (x:y:rest) = x == y || go (y:rest)
    go (y:[]) = False

noDecreasing :: Integer -> Bool
noDecreasing = go . show
  where
    go (x:y:rest) = if (y < x) then False else go (y:rest)
    go (x:[]) = True

atLeastOnePair :: Integer -> Bool
atLeastOnePair d = twoAdjacent d &&
  ((length $ filter (\l -> length l == 2) $ group $ sort $ show d) >= 1)

findPassword :: Input -> [Integer]
findPassword (Input start end) =
  do
    candidate <- [start .. end]
    guard $ twoAdjacent candidate && noDecreasing candidate
    pure candidate

answer1 :: String -> String
answer1 contents = show l
  where
    input = parseInput contents
    l = length $ findPassword input

findPassword2 :: Input -> [Integer]
findPassword2 (Input start end) =
  do
    candidate <- [start .. end]
    guard $ atLeastOnePair candidate && noDecreasing candidate
    pure candidate

answer2 :: String -> String
answer2 contents = show l
  where
    input = parseInput contents
    l = length $ findPassword2 input

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
--  putStrLn $ answer1 contents
  putStrLn $ answer2 contents


