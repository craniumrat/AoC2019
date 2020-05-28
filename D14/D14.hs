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
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

sepP = L.symbol sc "=>"
commaP = L.symbol sc ","

data Chemical = Chemical { _name :: String, _quantity :: Integer } deriving (Show, Ord, Eq)
data Reaction = Reaction { _chemical :: Chemical, _ingredients :: S.Set Chemical } deriving (Show, Ord, Eq)

toChemical :: Parser Chemical
toChemical = do
  q <- L.lexeme sc L.decimal
  n <- L.lexeme sc $ CA.some alphaNumChar
  pure $ Chemical n q
  
toChemicals :: Parser [Chemical]
toChemicals = toChemical `sepBy` commaP

toReaction :: Parser Reaction
toReaction = do
  cs <- toChemicals
  void $ sepP
  c <- toChemical
  pure $ Reaction c (S.fromList cs)

parseReaction :: String -> Reaction
parseReaction s = case parse toReaction "" s of
  Left _error -> error $ "Failed to parse"
  Right reaction -> reaction

answer1 :: String -> String
answer1 contents = show reactions
  where
    reactions = S.fromList $ parseReaction <$> lines contents
  

main :: IO()
main = do
  [file] <- getArgs
  contents <- readFile file
  putStrLn (answer1 contents)  

