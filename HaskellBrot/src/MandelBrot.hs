module MandelBrot where

import Data.Complex

-- Constants and Helper functions---------------------------------------------

iterations :: Int
iterations = 100

escapeDistance :: Double
escapeDistance = 2.0

i2d = fromIntegral

-------------------------------------------------------------------------------

-- Functions for a simple Mandelbrot set (only black or white pixels)


inMandel :: Complex Double -> Bool
inMandel a
    | (magnitude a) > 2 = False
    |  otherwise        = inMandel' a a iterations


inMandel' :: Complex Double -> Complex Double -> Int -> Bool
inMandel' c original 0 = True
inMandel' c original count
    | (magnitude c) > escapeDistance = False
    | otherwise                      = inMandel' c' original (count-1)
    where
        c' = (c * c) + original


-- Takes a two complex numbers and a number of increments between the two
-- and returns a list corresponding to whether the points on the line are
-- inMandel or not
mandelate :: Complex Double -> Complex Double -> Int -> [Bool]
mandelate start end pixels
    = map inMandel ys
        where
            ys  = take pixels (iterate (+inc) start)
            inc = (end - start)/((i2d pixels) :+0)



-- Functions for colouring the points from a mandelbrot set

-- Returns a double between 0 and 1, 0 meaning the number is still inside
-- escapedistance after inMandelC' and any other number indicates the ratio
-- of the number of iterations until a number escapes over "iterations"

--foreign export ccall inMandelCX :: Double -> Double -> Double
--inMandelCX a b = inMandelC (a:+b)

inMandelC :: Complex Double -> Double
inMandelC a
    | (magnitude a) > 2 = 1
    |  otherwise        = inMandelC' a a iterations


inMandelC' :: Complex Double -> Complex Double -> Int -> Double
inMandelC' c original 0 = 0
inMandelC' c original count
    | (magnitude c) > escapeDistance = i2d(count)/i2d(iterations)
    | otherwise                      = inMandelC' c' original (count-1)
    where
        c' = (c * c) + original


-- Takes a two complex numbers and a number of increments between the two
-- and returns a list corresponding to whether the points on the line are
-- inMandel or not
mandelateC :: Complex Double -> Complex Double -> Int -> [Double]
mandelateC start end pixels
    = map inMandelC ys
        where
            ys  = take pixels (iterate (+inc) start)
            inc = (end - start)/((i2d pixels) :+0)

-- Using the suffix X for the eXported functions
--foreign export ccall mandelateCX :: Double -> Double -> Double -> Double -> Int -> [Double]
mandelateCX :: Double -> Double -> Double -> Double -> Int -> [Double]
mandelateCX a b c d = mandelateC (a:+b) (c:+d)
