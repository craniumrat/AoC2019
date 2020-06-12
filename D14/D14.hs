module D14 where

{-
  The ORE, FUEL and intermediate products from a directed acyclic graph. We have to sort the intermediate products in order of how close they are to FUEL. This will allow us to add up and then break down the intermediate products into the next intermediate products in the graph.  Finally, we will be left with only FUEL.

-}

import System.Environment
import Debug.Trace (trace)
import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)
import Control.Applicative as CA
import qualified Data.Map.Strict as M
import Data.List (sortBy, groupBy)
import Data.Function (on)

type Parser = Parsec Void String

traceShow' a = trace (show a) a

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

sepP = L.symbol sc "=>"
commaP = L.symbol sc ","

data Chemical = Chemical { _name :: String, _quantity :: Integer } deriving (Show, Ord, Eq)
data Reaction = Reaction { _chemical :: Chemical, _ingredients :: [Chemical] } deriving (Show, Ord, Eq)

toChemical :: Parser Chemical
toChemical = do
  q <- L.lexeme sc L.decimal
  n <- L.lexeme sc $ CA.some alphaNumChar
  pure $ Chemical n q

toChemicals :: Parser [Chemical]
toChemicals = toChemical `sepBy` commaP

chemicalToPair :: Chemical -> (String, Integer)
chemicalToPair c = (_name c, _quantity c)

toReaction :: Parser Reaction
toReaction = do
  cs <- toChemicals
  void $ sepP
  c <- toChemical
  pure $ Reaction c cs

parseReaction :: String -> Reaction
parseReaction s = case parse toReaction "" s of
  Left _error -> error $ "Failed to parse"
  Right reaction -> reaction

type Ingredients = M.Map String (Integer, [(String, Integer)])

reactionsToIngredients :: [Reaction] -> Ingredients 
reactionsToIngredients reactions = foldl convert M.empty reactions
  where
    convert m reaction = M.insert key value m
      where
        key = _name._chemical $ reaction
        value  = (_quantity._chemical $ reaction, chemicalToPair <$> _ingredients reaction)
    
answer1 :: String -> String
answer1 contents = show output
  where
    reactions = reactionsToIngredients $ parseReaction <$> lines contents
    output = quantity [("FUEL", 1)] reactions

{- Given a [(Integer, String)] pair, grab the first one.
   In the map, find the ingredients list and multiply the quantity with the fst of the value. Once we do that, group by name, add the values and repeat. The only key with no value in map will be "ORE". If we see an "ORE", leave it and go to the next one
-}

quantity :: [(String, Integer)] -> Ingredients -> [(String, Integer)]
quantity (c:cs) reactions =
  if (snd c <= 0) then quantity (traceShow' $ (cs ++ [c])) reactions
  else
    case M.lookup (fst c) reactions of
      Nothing -> if end (c:cs) then (c:cs)  else quantity (cs ++ [c]) reactions
      Just (weight, ingredients) ->
        let
          multiplier = traceShow' $ (ceiling $ fromIntegral (snd c) / fromIntegral weight)
          negative = (traceShow' (snd c)) - (traceShow' $ (multiplier * weight))
          neg = if (negative /= 0) then (traceShow' [(fst c, negative)]) else (traceShow' [])
          weighted = (map (\(k, v) -> (k, multiplier * v)) ingredients) ++ neg
          sorted = sortBy (compare `on` fst) $ cs ++ weighted
          grouped = groupBy ((==) `on` fst) sorted
          mapped = map (\chemicals -> foldl1 (\(k, v1) (_, v2) -> (k, v1 + v2)) chemicals)  grouped
        in
          quantity (traceShow' mapped) reactions
      

end :: [(String, Integer)] -> Bool
end [] = True
end (x@(chem, q):xs) = if (chem /= "ORE" && q > 0) then False else end xs

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer1 contents)  

