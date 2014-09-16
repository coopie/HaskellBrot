import MandelBrot
import PrintGrid
import Data.Complex

main = do
	putStrLn "Enter the width and height of the mandelBrot Set you wish to make"
	printGrid (asciiGrid 200 80)


makeGrid :: Int -> Int -> [[Bool]]
makeGrid width height =  makeGrid' width height height []

makeGrid' :: Int -> Int -> Int -> [[Bool]] -> [[Bool]]
makeGrid' width height 0   grid  = grid
makeGrid' width height row grid
  = makeGrid' width height (row-1) grid' 
  	where
  		grid'      = (mandelate startPoint endPoint width) : grid
  		startPoint = (-2) :+ currHeight
  		endPoint   =   2  :+ currHeight
  		currHeight =  -2 + 4*(fromIntegral(row)/fromIntegral(height))
  		
asciiGrid :: Int -> Int -> [String]
asciiGrid width height 
	= map (concatMap f) (makeGrid width height)
	  where
	  	f = (\x -> if(x) then "+" else " ")
	


