
module PrintGrid where

import Control.Monad

-- Gets a 2d list and prints it out to standard out (at the moment)
printGrid :: [String] -> IO ()
printGrid xs = mapM_ putStrLn xs

     
