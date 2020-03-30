module D6 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (alphaNum, char)
import Text.Parsec (parse)
import Text.Parsec.Combinator (many1)
import Control.Monad (void, guard)
import System.Environment
import Control.Applicative ((<|>))
import Data.List (sort, group)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Orbit = Orbit String String  deriving Show -- Input start end

toOrbit :: Parser Orbit
toOrbit = do
  inner <- many1 alphaNum
  void $ char ')'
  outer <- many1 alphaNum
  pure $ Orbit inner outer

parseOrbit :: String -> Orbit
parseOrbit str = case parse toOrbit "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right orbit -> orbit

answer1 :: String -> String
answer1 contents = show answer
  where
    orbits = map parseOrbit  $ lines contents
    s = orbitSet orbits
    m = orbitMap orbits
    answer = orbitCounts s m


orbitMap :: [Orbit] -> M.Map String String
orbitMap orbits = foldl (\m o@(Orbit a b) -> M.insert b a m) M.empty orbits

orbitSet :: [Orbit] -> S.Set String
orbitSet orbits = foldl (\s o@(Orbit a b) -> S.insert a $ S.insert b s) S.empty orbits

orbitCounts :: S.Set String -> M.Map String String -> Int
orbitCounts s m = S.foldl (\acc key -> acc + orbitCount key m) 0 s

orbitCount :: String -> M.Map String String -> Int
orbitCount key m = case M.lookup key m of
  Nothing -> 0
  Just val -> 1 + orbitCount val m

answer2 :: String -> String
answer2 contents = show answer
  where
    orbits = map parseOrbit  $ lines contents
    m = orbitMap orbits
    answer = minOrbitalTransfer "YOU" "COM" m

pathToCOM :: String -> M.Map String String -> [String] -> [String]
pathToCOM key m l = case M.lookup key m of
  Nothing -> l
  Just val -> pathToCOM val m (val:l)

removeCommonNodes :: [String] -> [String] -> [String]
removeCommonNodes (x:xs) (y:ys) = if (x == y) then removeCommonNodes xs ys
  else (x:xs) ++ (y:ys)

minOrbitalTransfer :: String -> String -> M.Map String String -> Maybe Int
minOrbitalTransfer first second m =
  if (first == second) then
    Just 0
  else
    let
      youPath = pathToCOM "YOU" m []
      sanPath = pathToCOM "SAN" m []
      path = removeCommonNodes youPath sanPath
    in  
      Just $ length path

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
--  putStrLn $ answer1 contents
  putStrLn $ answer2 contents


