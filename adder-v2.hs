import Data.Char

newline :: IO ()
newline = putChar '\n'

adder :: IO ()
adder = do putStr "How many numbers? "
           nums <- getLine
           strs <- sequence [getLine | _ <- [1..((read nums)::Int)]]
           putStrLn ("The total is " ++ (show (sum [read x :: Int | x <- strs])))

main :: IO ()
main = adder
