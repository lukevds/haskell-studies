import Data.Char

newline :: IO ()
newline = putChar '\n'

getNums :: Int -> Int -> IO Int
getNums n 0 = return n
getNums n x = do input <- getLine
                 getNums (n + ((read input) :: Int)) (x - 1)

adder :: IO ()
adder = do putStr "How many numbers? "
           nums <- getLine
           total <- getNums 0 ((read nums) :: Int)
           putStr ("The total is " ++ (show total))
           newline

main :: IO ()
main = adder
