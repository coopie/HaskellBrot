module MandelBrot where

import Data.Complex

-- Constansts

iterations :: Int
iterations = 100

escapeDistance :: Double
escapeDistance = 2.0


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
			inc = (end - start)/((fromIntegral pixels) :+0)