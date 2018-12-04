import System.IO
import System.Environment
import System.Directory
import Data.Numbers.Primes
import Color
import Codec.Picture

-- pixels

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 255 255 255

cyan :: PixelRGB8
cyan = PixelRGB8 0 255 255

red :: PixelRGB8
red = PixelRGB8 255 0 0

-- OEIS A003418 : Least common multiple (or LCM) of {1, 2, ..., n} for n >= 1, a(0) = 1

towers :: [Int]
towers = map (\n -> foldl lcm 1 [1..n]) [1..]

isTower :: Int -> Bool
isTower x = (head $ dropWhile (< x) towers) == x
-- isTower x = elem x $ takeWhile (<= x) towers

-- function

harmonic :: Int -> Int -> PixelRGB8
harmonic x y = if mod x (y+1) == 0
    then if isTower x
        then black
        else red
    else if isPrime x
        then cyan
        else white

-- dimensions

type Dimensions = (Int,Int)

-- rendering

draw :: Dimensions -> IO()
draw d = do
    createDirectoryIfMissing True "io"
    done <- doesFileExist file
    if done then
        putClrLn W (file ++ " skipped") -- print existing file to console
    else do
        savePngImage file (ImageRGB8 (generateImage harmonic w h))
        putClrLn G file -- print drawn file to console
    where
        (w,h) = d
        file = "io/" ++ show w ++ "x" ++ show h ++ ".png"

-- prompt

main :: IO ()
main = do 
    args <- getArgs
    if length args == 2 then let
        (w:h:[]) = map read args
        in
        draw (w,h)
    else do
        putClr R "args:"
        putClrLn W "width height"
        pure ()
