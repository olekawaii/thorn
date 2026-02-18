{-# Language MultiWayIf, LambdaCase #-}

module Parse where

import System.Exit (die)
import System.Process (callCommand)
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Control.Monad ((<=<), (>=>), guard, unless, when)
import Control.Arrow ((<<<), (>>>), (***), (&&&), (+++), (|||))
import qualified Data.Map.Strict as Map 
import Data.Bifunctor (first, second, bimap)
import Data.Either (fromRight, fromLeft)
import Data.Maybe (isJust, fromJust, listToMaybe, isNothing, fromMaybe)
import Data.Tuple (swap)
import Data.List (find, nub, transpose, sortOn, intercalate)
import Text.Read (readMaybe) 

getVideo :: IO (Int, Int, [[[Character]]])
getVideo = getContents >>= pure . changeFormat . parseVideo . words 

dimensions :: [Map Coordinate Character] -> (Int, Int, Int, Int)
dimensions gif = case concatMap (map fst) gif of
  [] -> (0, 0, 0, 0)
  g  -> getDimensions g
    where
      getDimensions :: [Coordinate] -> (Int, Int, Int, Int)
      getDimensions x =
        let
          xCoords = map fst x
          yCoords = map snd x
        in
          (
            minimum xCoords,
            maximum xCoords,
            minimum yCoords,
            maximum yCoords
          )

-- check that it's not 0
changeFormat :: [Map Coordinate Character] -> (Int, Int, [[[Character]]])
changeFormat x = let (x_min, x_max, y_min, y_max) = dimensions x in
  let
    chart = liftA2 (flip (,))  [y_max, y_max -1 .. y_min] [x_min..x_max]
    convertFrame :: Map Coordinate Character -> [[Character]]
    convertFrame a =  chunksOf (x_max - x_min + 1) $ map lookupChar chart
      where
        lookupChar want = fromMaybe Space $ lookup want a
  in
  (x_max - x_min + 1, y_max - y_min + 1, map convertFrame x) 

cons = (:)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = uncurry ((. chunksOf n) . cons) $ splitAt n x

type Map a b       = [(a,b)]

data OutputType = Single | Looping deriving Show

type Coordinate    = (Int,Int)

data Character = Space | Character Char Color deriving Eq

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
  deriving (Show, Eq)

parseVideo :: [String] -> [Map Coordinate Character]
parseVideo ("empty_video": xs) = []
parseVideo ("cons_frame": xs) = let (frame, leftover) = parseFrame xs in frame : parseVideo leftover

parseFrame :: [String] -> (Map Coordinate Character, [String])
parseFrame ("unsafe_cons_cell" : xs) = 
  let (cell, leftover) = parseCell xs in 
  first (cell :) (parseFrame leftover)
parseFrame ("empty_frame" : xs) = ([], xs)

parseCell :: [String] -> ((Coordinate, Character), [String])
parseCell ("cell" : xs) =
  let (coord, leftover1) = parseCoordinate xs in
  let (character, leftover2) = parseChar leftover1 in 
  ((coord, character), leftover2)

parseCoordinate :: [String] -> (Coordinate, [String])
parseCoordinate ("coordinate": xs) = 
  let (x, leftover1) = parse_int xs in
  let (y, leftover2) = parse_int leftover1 in
  ((x, y), leftover2)

parse_int :: [String] -> (Int, [String])
parse_int ("pos" : xs) = parseNat xs
parse_int ("neg" : xs) = first (* (-1)) (parseNat xs)
parse_int ("zero" : xs) = (0, xs)

parseNat :: [String] -> (Int, [String])
parseNat ("succ": xs) = first (1 +) (parseNat xs)
parseNat ("one": xs) = (1, xs)

parseChar :: [String] -> (Character, [String])
parseChar ("space": xs) = (Space, xs)
parseChar ("char": char : color : xs) = 
  let 
    c = case char of 
       "exclamation_mark" -> '!'
       "quotation_mark" -> '"'
       "number_sign" -> '#'
       "dollar_sign" -> '$'
       "percent_sign" -> '%'
       "ampersand" -> '&'
       "apostrophe" -> '\''
       "left_paranthesis" -> '('
       "right_paranthesis" -> ')'
       "asterisk" -> '*'
       "plus_sign" -> '+'
       "comma" -> ','
       "hyphen_minus" -> '-'
       "full_stop" -> '.'
       "solidus" -> '/'
       "digit_zero" -> '0'
       "digit_one" -> '1'
       "digit_two" -> '2'
       "digit_three" -> '3'
       "digit_four" -> '4'
       "digit_five" -> '5'
       "digit_six" -> '6'
       "digit_seven" -> '7'
       "digit_eight" -> '8'
       "digit_nine" -> '9'
       "colon" -> ':'
       "semicolon" -> ';'
       "less_than_sign" -> '<'
       "equals_sign" -> '='
       "greater_than_sign" -> '>'
       "question_mark" -> '?'
       "commercial_at" -> '@'
       "capital_a" -> 'A'
       "capital_b" -> 'B'
       "capital_c" -> 'C'
       "capital_d" -> 'D'
       "capital_e" -> 'E'
       "capital_f" -> 'F'
       "capital_g" -> 'G'
       "capital_h" -> 'H'
       "capital_i" -> 'I'
       "capital_j" -> 'J'
       "capital_k" -> 'K'
       "capital_l" -> 'L'
       "capital_m" -> 'M'
       "capital_n" -> 'N'
       "capital_o" -> 'O'
       "capital_p" -> 'P'
       "capital_q" -> 'Q'
       "capital_r" -> 'R'
       "capital_s" -> 'S'
       "capital_t" -> 'T'
       "capital_u" -> 'U'
       "capital_v" -> 'V'
       "capital_w" -> 'W'
       "capital_x" -> 'X'
       "capital_y" -> 'Y'
       "capital_z" -> 'Z'
       "left_square_bracket" -> '['
       "reverse_solidus" -> '\\'
       "right_square_bracket" -> ']'
       "circumflex_accent" -> '^'
       "low_line" -> '_'
       "grave_accent" -> '`'
       "small_a" -> 'a'
       "small_b" -> 'b'
       "small_c" -> 'c'
       "small_d" -> 'd'
       "small_e" -> 'e'
       "small_f" -> 'f'
       "small_g" -> 'g'
       "small_h" -> 'h'
       "small_i" -> 'i'
       "small_j" -> 'j'
       "small_k" -> 'k'
       "small_l" -> 'l'
       "small_m" -> 'm'
       "small_n" -> 'n'
       "small_o" -> 'o'
       "small_p" -> 'p'
       "small_q" -> 'q'
       "small_r" -> 'r'
       "small_s" -> 's'
       "small_t" -> 't'
       "small_u" -> 'u'
       "small_v" -> 'v'
       "small_w" -> 'w'
       "small_x" -> 'x'
       "small_y" -> 'y'
       "small_z" -> 'z'
       "left_curly_brace" -> '{'
       "vertical_line" -> '|'
       "right_curly_brace" -> '}'
       "tilde" -> '~'
       x -> error x
   in 
   let 
     col = case color of
        "black" -> Black
        "red" -> Red
        "green" -> Green
        "yellow" -> Yellow
        "blue" -> Blue
        "magenta" -> Magenta
        "cyan" -> Cyan
        "white" -> White
   in
      (Character c col, xs)

