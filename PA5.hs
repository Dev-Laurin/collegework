--PA5.hs
--Laurin Fisher
--CS331
--3/27/15

module PA5 where

{-***********************Collatz************************-}

--Collatz Function that returns how many times until the 
--	number reaches 1 
collatz 0 _ = 0
collatz k x
	| k == 1    = x 
	| even k    = collatz (k `div` 2) (x+1) 
	| otherwise = collatz (3*k + 1) (x +1) 
 
--mapCollatz
--	Helper function for collatzCounts. Enables the collatz 
--	function to be called on array with multiple parameters
mapCollatz f (x:xs) y = f x y : mapCollatz f xs y 

--collatzCounts
--	An array of numbers from collatz function based on 
--	index position in array
collatzCounts  = mapCollatz collatz [1..] 0 



{-******************findList**********************-}

--takes 2 lists
--keeps track of index with c 
--keeps track of list sizes and decriments it
--keeps track of if it has been equal before - e
findHelp (x:xs) (y:ys) c l1 l2 e 
	| (x /= y && e==False && l2 == 0) = Nothing --not in list
	| (l2 == 0 || l1 ==0)      = Just c --reached end of list1
	| x == y 	   		   	   = findHelp xs ys c (l1-1) (l2-1) True
	| (x/= y && e == True)	   = Nothing --sublist has extra not in list2
	| otherwise   = findHelp (x:xs) ys (c+1) l1 (l2-1) False--keep going 

--findList
--findList :: [x] -> [x] -> (Maybe Int)
findList [] _ = Just 0 
findList (x:xs) (y:ys) = 
	findHelp (x:xs) (y:ys) 0 (length (x:xs)) (length(y:ys)) False 


--operator ##
[] ## _ = 0
(x:xs) ## (y:ys) = 
	opHelp (x:xs) (y:ys) 0 (length (x:xs)) (length (y:ys))

--keeps track of lists sizes=knows when to end/return c
--c counts how many items are equal 
opHelp (x:xs) (y:ys) c l1 l2 
	| (l1 == 0 || l2 == 0)   = c
	| x == y   			   = opHelp xs ys (c+1) (l1-1) (l2-1) 
	| otherwise   		   = opHelp xs ys c (l1-1) (l2-1)


