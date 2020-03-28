--max.hs
--Laurin Fisher
--cs 331 
--Exercise B

module Main where


main = do 
	putStrLn "hello"
	interactive

interactive = do 
	getNum []
	putStrLn "Do again? [y/n] "
	hFlush
	line <- getLine 
	if line == "y"
		then
			--do again
			interactive
		else
			--stop
			putStrLn "Terminating..."

getNum (x:xs) = do 
	putStrLn "Number please (blank line to end): " 
	hFlush 
	line <- getLine
	if line == ""
		then 
			--end
			return maximum (x:xs)
		else
			line : getNum xs 

{-"I'm going to ask you for a number (Integer). Please
	print one on each line. I will give you the maximum
	of these values afterwards. "-}