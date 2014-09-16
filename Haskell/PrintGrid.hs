
import Control.Monad

-- Gets a 2d list and prints it out to standard out (at the moment)
printGrid :: Show a => [[a]] -> IO ()
printGrid xs = mapM_ printLine xs
  where
    printLine ys = putStrLn (concatMap show ys) 
