module D16 where

import Data.Char (intToDigit, digitToInt)
import Debug.Trace (trace)

traceShow' :: Show a => a -> a
traceShow' a = trace (show a) a

input = "59766299734185935790261115703620877190381824215209853207763194576128635631359682876612079355215350473577604721555728904226669021629637829323357312523389374096761677612847270499668370808171197765497511969240451494864028712045794776711862275853405465401181390418728996646794501739600928008413106803610665694684578514524327181348469613507611935604098625200707607292339397162640547668982092343405011530889030486280541249694798815457170337648425355693137656149891119757374882957464941514691345812606515925579852852837849497598111512841599959586200247265784368476772959711497363250758706490540128635133116613480058848821257395084976935351858829607105310340"

-- input = "12345678"
-- input = "80871224585914546619083218645595"

base_pattern = [0, 1, 0, -1] :: [Int]

pattern :: Int -> [Int]
pattern repeats = tail $ cycle $ concat $ map (\val -> take (repeats + 1) $ repeat val) base_pattern

fftForPos :: String -> Int -> Char
fftForPos str pos = c
  where
    val = foldl (\acc (c, mul) -> acc + (digitToInt c) * mul) 0 $ zip str (patterns !! pos)
    c = intToDigit $ abs $ val `rem` 10
    patterns = map(\pos -> pattern pos) [0..]

fft :: String -> String
fft str = map (\(c, pos) -> fftForPos str pos) $ zip str [0..]

answer1 = take 8 $ foldr (.) (id) (replicate 100 fft) input

{-
    The solution above is way too slow for a message that's a repeat of the input * 10000.
-}

{-
   Thanks to the kind people on reddit and Joel Grus' video https://www.youtube.com/watch?v=Xmcw8m0rukk to help me understand what's going on....
   The final offset after 100 iterations is given by the first 7 digits of our original input. Since all digits depend on the subsequent digits and the final digits are just (original digit + next output digit) `mod` 10 for the second half of the 6_500_000 inuput, we can just use a scanr to get the new digits.
-}

answer2 = show value
  where
    new_input = concat $ replicate 10000 input
    offset = read $ take 7 new_input
    suffix = map (digitToInt) $ drop offset new_input
    value = take 8 $ foldr (.) (id) (replicate 100 fftFast) suffix
    

fftFast :: [Int] -> [Int]
fftFast input = scanr (\c acc -> (c + acc) `mod` 10) 0 input

main =
--   putStrLn answer1
  putStrLn answer2
