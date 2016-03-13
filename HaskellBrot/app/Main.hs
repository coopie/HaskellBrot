import MandelBrot
import PrintGrid
import Data.Complex
import GHC.Float

import Codec.Picture

import Debug.Trace

width = 1000
height = 1000
resolution :: Float
resolution = 0.004

main = do
-- TODO: argument passing for   dimensions, resolution, centre

    saveBmpImage "image.bmp" (ImageYF (makeImage width height resolution))

makeImage :: Int -> Int -> Float -> Image PixelF
makeImage w h r = generateImage (pixelGenerator w h r) w h

pixelGenerator :: Int -> Int -> Float -> (Int -> Int -> Float)
pixelGenerator width height resolution
  = gen
    where
        gen x y = double2Float (inMandelC (((fromIntegral x)*resolution - 2) :+ (2 - (fromIntegral y)*resolution)))

-- functions for the simple implementation of the mandelbrot set

makeGrid :: Int -> Int -> [[Bool]]
makeGrid width height =  makeGrid' width height height []


makeGrid' :: Int -> Int -> Int -> [[Bool]] -> [[Bool]]
makeGrid' width height 0   grid  = grid
makeGrid' width height row grid
  = makeGrid' width height (row-1) grid'
      where
          grid'      = (mandelate startPoint endPoint width) : grid
          startPoint = (-2) :+ currHeight
          endPoint   =   0.6  :+ currHeight
          currHeight =  -1 + 2*(fromIntegral(row)/fromIntegral(height))


asciiGrid :: Int -> Int -> [String]
asciiGrid width height
    = map (concatMap f) (makeGrid width height)
      where
          f = (\x -> if(x) then "+" else " ")

-- functions for the "colourful" inplementation of the mandelbrot set

makeGridC :: Int -> Int -> [[Double]]
makeGridC width height =  makeGridC' width height height []


makeGridC' :: Int -> Int -> Int -> [[Double]] -> [[Double]]
makeGridC' width height 0   grid  = grid
makeGridC' width height row grid
  = makeGridC' width height (row-1) grid'
      where
          grid'      = (mandelateC startPoint endPoint width) : grid
          startPoint = (-2) :+ currHeight
          endPoint   =   0.6  :+ currHeight
          currHeight =  -1 + 2*(fromIntegral(row)/fromIntegral(height))


asciiGridC :: Int -> Int -> [String]
asciiGridC width height
    = map (concatMap charify) (makeGridC width height)

charify :: Double -> String
charify d
  | d < 0.1   = "#"
  | d < 0.2   = "@"
  | d < 0.3   = "a"
  | d < 0.4   = "*"
  | d < 0.5   = "o"
  | d < 0.6   = "|"
  | d < 0.7   = ":"
  | d < 0.8   = "\'"
  | d < 0.9   = "."
  | otherwise = " "
