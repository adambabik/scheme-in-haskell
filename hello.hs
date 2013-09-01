module Main where
import System.Environment

main :: IO ()
main = do
	-- args <- getArgs

	-- (arg0:arg1:restArgs) <- getArgs
	-- putStrLn $ "sum: " ++ show (read arg0 + read arg1)

	name <- getLine
	putStrLn $ "Your name is: " ++ name
