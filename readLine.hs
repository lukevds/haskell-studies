import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


readLine :: IO String
readLine = do x <- getCh
              case x of
                '\DEL' -> do putChar '\b'
                             y <- readLine
                             return y
                '\n'   -> do putChar x
                             return []
                _      -> do putChar x
                             y <- readLine
                             return (x:y)

main = do str <- readLine
          if str == "123" then
             putStrLn "Success!"
          else
             putStrLn "Error!"
