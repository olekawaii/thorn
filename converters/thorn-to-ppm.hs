{-# Language MultiWayIf, LambdaCase #-}

import Data.List 
import Data.Char (ord)

import Parse

padding = 20

showNum :: Int -> String
showNum x = take (5 - length s) (cycle ['0']) <> s
  where s = show x

makeFiles :: String -> [String] -> Int -> IO ()
makeFiles _ [] _ = pure ()
makeFiles header (x:xs) n = writeFile fileName content >> makeFiles header xs (n + 1)
    where 
        content = header <> x
        fileName = "output" <> showNum n <> ".ppm"

main :: IO ()
main = 
    parseFontFile <$> readFile "font" >>= \(font, fwidth, fheight) ->
    getVideo >>= \(width, height, newGif) ->
    let 
        horizontalPixels = width  * fwidth  + padding * 2 
        verticalPixels   = height * fheight + padding * 2 
        gifFrames = map (flip (pipelineGif font fwidth fheight) horizontalPixels) newGif 
        header = "P3\n" <> show (horizontalPixels) <> " " <> show (verticalPixels) <> "\n255\n"
    in
    makeFiles header gifFrames 0

pipelineGif :: [[[Bool]]] -> Int -> Int -> [[Character]] -> Int -> String
pipelineGif font width height x y = unlines . map showColor . surround . uwu font width height $ x
    where surround x = let lns = replicate (y * padding) Black in lns <> x <> lns

uwu :: [[[Bool]]] -> Int -> Int -> [[Character]] -> [Color]
uwu font width height x = concat . map 
    (concat . map surround . foldr1 combine . map (\c -> (fontLookup c width height font))) 
    $ x
    where surround x = replicate padding Black <> x <> replicate padding Black

combine :: [[Color]] -> [[Color]] -> [[Color]] 
combine = (map (uncurry (++)) .) . zip

showColor :: Color -> String
showColor Black    = " 29  32  33"
showColor Red      = "234 105  98"
showColor Green    = "169 182 101"
showColor Yellow   = "216 166  87"
showColor Blue     = "125 174 163"
showColor Magenta  = "211 134 155"
showColor Cyan     = "137 180 130"
showColor White    = "212 190 152"

isSpace Space = True
isSpace (Character _ _) = False

parseFontFile :: String -> ([[[Bool]]], Int, Int)
parseFontFile x = 
    let 
        (header : body) = lines x
        (width : height : []) = map read (words header) :: [Int]
    in (
          concatMap transpose 
            . chunksOf height 
            . map (chunksOf width) 
            . filter (not . null) 
            . map (map (/= '.') . filter (/= ' ')) 
            $ body
          ,
          width, 
          height
       )

fontLookup :: Character -> Int -> Int -> [[[Bool]]] -> [[Color]]
fontLookup (Character char color) _ _ table = map (map (\x -> if x then color else Black)) (table !! (ord char - 33))
fontLookup Space width height _ = take height (chunksOf width (repeat Black))
