{-# Language MultiWayIf, LambdaCase #-}

import Data.List 
import Data.Char (ord)

import System.Exit (die)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Environment (getArgs)

import Parse

data Font = Font {
    width    :: Int,
    height   :: Int,
    pixels   :: [[[Bool]]]
}

showNum :: Int -> String
showNum x = take (5 - length s) (cycle ['0']) <> s
  where s = show x

makeFiles :: String -> [String] -> Int -> IO ()
makeFiles _ [] _ = pure ()
makeFiles header (x:xs) n = writeFile fileName content >> makeFiles header xs (n + 1)
    where 
        content = header <> x
        fileName = "output" <> showNum n <> ".ppm"

data Args = Args {
    fontDir   :: String,
    themeDir  :: String
}

parseArgs :: [String] -> String -> IO Args
parseArgs ("--font-dir"  : f : xs) home = 
    parseArgs xs home >>= \x -> pure $ x { fontDir  = f }
parseArgs ("--theme-dir" : t : xs) home = 
    parseArgs xs home >>= \x -> pure $ x { themeDir = t }
parseArgs [] home = 
    pure Args { 
        fontDir  = home </> ".local/share/thorn/fonts", 
        themeDir = home </> ".local/share/thorn/themes" 
    }
parseArgs _ _ = die "bad arguments"

main :: IO ()
main = 
    getHomeDirectory >>= \home -> 
    getArgs >>= flip parseArgs home >>= \Args {fontDir, themeDir} ->
    getVideo >>= \(width, height, newGif, GraphicalOptions font theme fps) ->
        fmap parseFontFile (readFile (fontDir </> font)) 
          >>= \font@Font {width = fwidth, height = fheight} ->
        let 
            padding = fwidth * 2
            horizontalPixels = width  * fwidth  + padding * 2 
            verticalPixels   = height * fheight + padding * 2 
            gifFrames = map (flip (pipelineGif font) horizontalPixels) newGif 
            header = "P3\n" 
                  <> show (horizontalPixels) 
                  <> " " 
                  <> show (verticalPixels) 
                  <> "\n255\n"
        in
        makeFiles header gifFrames 0

pipelineGif :: Font -> [[Character]] -> Int -> String
pipelineGif font@Font { width } x y = unlines . map showColor . surround . uwu font $ x
    where surround x = let lns = replicate (y * (width * 2)) Black in lns <> x <> lns

uwu :: Font -> [[Character]] -> [Color]
uwu font@Font {width} x = concatMap 
    (concatMap surround . foldr1 combine . map (\c -> (fontLookup c font))) 
    x
    where 
        surround x = padding <> x <> padding
        padding = replicate (width * 2) Black

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

parseFontFile :: String -> Font
parseFontFile x = Font {
        pixels = concatMap transpose 
            . chunksOf height 
            . map (chunksOf width) 
            . filter (not . null) 
            . map (map (/= '.') . filter (/= ' ')) 
            $ body,
        width = width, 
        height = height
    } where
        (header : body) = lines x
        [width, height] = map read (words header) :: [Int]

fontLookup :: Character -> Font -> [[Color]]
fontLookup (Character char color) Font {pixels = table} = map (map (\x -> if x then color else Black)) (table !! (ord char - 33))
fontLookup Space Font {width, height} = take height (chunksOf width (repeat Black))
