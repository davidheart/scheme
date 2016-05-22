

module Main where
import System.Environment

main :: IO ()
-- "putStrLn cannot precede "do"
-- Exercise 1
-- main = do args <- getArgs
--           putStrLn ("Hello" ++ args !! 0 ++ args !! 1)


-- Exercise 2
-- main = do args <- getArgs
--           print((read $ args !! 0) + (read $ args !! 1))

-- Exercise 2
main = do args <- getArgs
          print((read $ args !! 0) + (read $ args !! 1))
