-- main function to play Interactive King of Zed   
-- This function gives all the introduction, lets you provide it with a puzzle, and then shows you what the grid looks like, and how to play.            
main = do
    putStrLn $ "Welcome to the Kingdom of Zed Game!"
    putStrLn $ "Please input your puzzle in an array format. The system will crash if the wrong format is used"
    putStrLn $ "Please create a row for the North Side. Example [2,1]"
    north <- getLine
    let nl = (read north :: [Int])
    putStrLn $ "Please create a row for the East Side. Example [2,1]"
    east <- getLine
    let el = (read east :: [Int])
    putStrLn $ "Please create a row for the South Side. Example [2,1]"
    south <- getLine
    let sl = (read south :: [Int])
    putStrLn $ "Please create a row for the West Side. Example [2,1]"
    west <- getLine
    let wl = (read west :: [Int])
    if (length nl) /= (length el) || (length nl) /= (length sl) || (length nl) /= (length wl) || (invalidPuzzle (nl,el,sl,wl) (length nl))
    	then do 
			putStrLn $ "Invalid puzzle. Please press 1 to play again, or 0 to quit"
			key <- getLine
			let playAgain = (read key :: Int)
			if (playAgain == 1) then main else return()
		else do 
			let display = createGrid (nl,el,sl,wl) (length nl)
			let n = length nl
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $"This is your grid. The top row is your north merchants,"
			putStrLn $ "your leftmost and rightmost columns are west and east merchants respectively,"
			putStrLn $ "and your bottom row is your south merchants."
			mapM_ print display
			putStrLn $ "Ignoring the zeros on the four corners, the middle zeros is your grid!"
			putStrLn $ ""
			putStrLn $ "Please input the square you want to fill in and with which value in the following way:"
			putStrLn $ "[x,y,z] means that in the xth row on the yth column, you want to input z"
			putStrLn $ "x, y, and z range from 1 to the number of columns/rows"
			putStrLn $ "Good luck!"
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $ ""
			let init = [x | y <- [1..n], x <- [(replicate n 0)]]
			putStrLn $ "When ready, press any key, then Enter"
			getLine
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $ "Your grid is now:"
			putStrLn $ ""
			putStrLn $ ""
			putStrLn $ ""
			mapM_ print display
			playGame (nl,el,sl,wl) n display init
			return()

--playGame (nl,el,sl,wl) n display grid lets the user fill in a square, checks to see if you've reached a solution yet, and if not recurses until you do, or give up
--if [0,0,0] is typed, the game will use Brute Force to solve the puzzle and give the user the answer
--invalid types will crash the game, and invalid arrays will cause the game to recurse again
playGame (nl,el,sl,wl) n display grid = do
	putStrLn $ "Please input the square you want to fill in (ie. [row,column,value])."
	putStrLn $ "If you want to give up, input [0,0,0] for the solution"
	rep <- getLine
	let replace = (read rep :: [Int])
	if (length replace /= 3) then do
								putStrLn $ ""
								putStrLn $ ""
								putStrLn $ ""
								putStrLn $ "This was not the right format. Remember it's [row,column, value]. Please try again"
								playGame (nl,el,sl,wl) n display grid
							else do 
								if (replace == [0,0,0]) then do
									putStrLn $ "Good try!"
									putStrLn $ "The solution is ..."
									putStrLn $ ""
									putStrLn $ ""
									putStrLn $ ""
									let perms = filler n
									let soln = (checkGrids (nl,el,sl,wl) perms n)
									if (soln == []) then do
														putStrLn $ "Woops! This puzzle has no solution!"
														putStrLn $ "Thanks for playing!" 
														return() 
													else do 
														mapM_ print (createGridSol (nl,el,sl,wl) n soln)
														putStrLn $ "Please press 1 to play again, or 0 to quit"
														key <- getLine
														let playAgain = (read key :: Int)
														if (playAgain == 1) then main else return()
								else do 
									if (isRangeOk replace n) then do 
										print "yay"
										let newGrid = replaceValGrid replace grid
										let newDisplay = replaceValDis replace display
										putStrLn $ "Your grid is now:"
										putStrLn $ ""
										putStrLn $ ""
										putStrLn $ ""
										mapM_ print newDisplay
										let isCorrect = (correct newGrid (nl,el,sl,wl))
										if (isCorrect) then do
															putStrLn $ ""
															putStrLn $ ""
															putStrLn $ "Congratulations! You solved it!"
															putStrLn $ "Here is your final grid:"
															putStrLn $ ""
															putStrLn $ ""
															putStrLn $ ""
															mapM_ print newDisplay

														else do
															putStrLn $ "" 
															putStrLn $ "Keep going!"
															putStrLn $ ""
															putStrLn $ ""
															putStrLn $ ""
															playGame (nl,el,sl,wl) n newDisplay newGrid

										return()
									else do
										putStrLn $ ""
										putStrLn $ ""
										putStrLn $ ""
										putStrLn $ "Your numbers were invalid Please stay in range."
										playGame (nl,el,sl,wl) n display grid


-- checks to make sure all merchants have numbers less than the grid size (nxn)
invalidPuzzle (nl,el,sl,wl) n = (checkMerchants nl n) || (checkMerchants el n) || (checkMerchants sl n) || (checkMerchants wl n)

-- individual merchant check helper
checkMerchants lst n 
				| length lst == 0 = False
				| (head lst) > n = True
				| otherwise = checkMerchants (tail lst) n

-- checks to see if the range is valid for the user's input
isRangeOk [x,y,z] n = (x >= 0) && (x <= n) && (y > 0) && (y <= n) && (z > 0) && (z <= n)

--given the user input, replaces the square they selected at column x and row y with value z on the grid (used for brute solve purposes)
replaceValGrid [x,y,z] grid = replaceNth (x-1) (replaceNth (y-1) z (grid !! (x-1))) grid

--given the user input, replaces the square they selected at column x and row y with value z on the grid (used for display purposes)
replaceValDis [x,y,z] grid = replaceNth x (replaceNth y z (grid !! x)) grid

--replaces the nth index of an array (x:xs) with newVal
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

--creates the initial display graph will all 0s
createGrid (nl,el,sl,wl) n =         [[0] ++ nl ++ [0]] ++ (createSquare el (reverse wl) n) ++ [[0] ++ (reverse sl)++ [0]]

--creates the solution display graph given a solution, soln
createGridSol (nl,el,sl,wl) n soln = [[0] ++ nl ++ [0]] ++ (createSquareSol el (reverse wl) soln n) ++ [[0] ++ (reverse sl)++[0]]

--creates the west and east part of the initial grid
createSquare [] _ _ = []
createSquare _ [] _ = []
createSquare e1 w1 n= [[(head w1)] ++ (replicate n 0) ++ [(head e1)]] ++ (createSquare (tail e1) (tail w1) n)

--creates the west and east part of the solution grid
createSquareSol [] _ _ _= []
createSquareSol _ [] _ _= []
createSquareSol e1 w1 grid n = [[(head w1)] ++ (head grid) ++ [(head e1)]] ++ (createSquareSol (tail e1) (tail w1) (tail grid) n) 

--checks a list of grids to see if there's a correct solution given merchants, returns [] if not
checkGrids _ [] _ = []
checkGrids merchants grids n
		| (legalGrid (head grids) n) && (correct (head grids) merchants) = head grids
		| otherwise = checkGrids merchants (tail grids) n

--creates all brute force grids
filler n = permuteGrid n (perm [1..n]) (initializeGrid n)

--gives the first grid to pass to fn by creating a list of permutations, all separated into their own lists
initializeGrid n = [x | y <- (perm [1..n]), x <- [[y]]]

--creates the list of grid permutations, excluding all repeats 
permuteGrid n perms acc
			| length (head acc) == n = acc
			| otherwise = permuteGrid n perms (fn perms acc)

--adds one of the permutations to each set of permutations in the list ex. acc = [[1],[1]] and given perm == [2] this will return [[1,2],[1,2]]
fn _ [] = []
fn [] _ = []
fn perms acc 
		| (head perms) `elem` (head acc) = (fn (tail perms) [(head acc)]) ++ (fn perms (tail acc))
		| otherwise = [[(head perms)]++(head acc)] ++ (fn (tail perms) [(head acc)]) ++ (fn perms (tail acc))


-- checks to see if grid is valid by checking columns and rows
legalGrid grid n = (legalRows grid n 0) && (legalCols grid n 0)

-- checks columns to see if they are infact valid [1..n] only in each row
legalCols grid n acc 
		| n == acc = True
      	| otherwise = (legalCol grid acc [1..n]) && (legalCols grid n (acc+1))

-- checks that one column is valid
legalCol [] _ lst
		| (length lst) == 0 = True
      	| otherwise = False

legalCol grid acc lst = (legalCol (tail grid) acc (deleteX ((head grid) !! acc) lst))

-- checks rows to see if they are infact valid [1..n] only in each row
legalRows grid n acc 
		| n == acc = True
		| otherwise = (legalRow (head grid) [1..n]) && (legalRows (tail grid) n (acc+1))

-- checks that one row is valid
legalRow [] lst 
		| (length lst) == 0 = True
		| otherwise = False

legalRow row lst = legalRow (tail row) (deleteX (head row) lst)

-- delete helper function 
deleteX val [] = []
deleteX val (x:xs) 
	| val == x = xs
	|otherwise = x:(deleteX val xs)

--checks if a grid is the solution given the merchants (nl,el,sl,wl)
correct grid (nl, el, sl, wl) = (north grid nl) && (east grid el) && (south grid sl) && (west grid wl)


--creates permutations of a list
perm []     = return []
perm (x:xs) = (perm xs) >>= (ins x)
    where
    ins :: a -> [a] -> [[a]]
    ins x []     = [[x]]
    ins x (y:ys) = [x:y:ys] ++ ( map (y:) (ins x ys) )


-- validator check to see if north side traders all are true in terms of the grid given
-- creates a list of lists, where each individual list contains the columns going from top to bottom to be passed to the helper
north grid n1 = (northHelper [a | ind <- [0..((length n1)-1)], a <- [[x | y <- grid, x <- [(y !! ind)]]]] n1)

-- helper to check that every column is valid
northHelper [] [] = True
northHelper columns n1 = (northHelper2 (head columns) (head n1) 0 0) && (northHelper (tail columns) (tail n1))

-- checks that one column is valid
northHelper2 [] merchant rsf _ = merchant == rsf
northHelper2 column merchant rsf max 
	| merchant == 0 = True
	| (head column) > max = northHelper2 (tail column) merchant (rsf+1) (head column)
 	| otherwise = northHelper2 (tail column) merchant rsf max

-- validator check to see if south side traders all are true in terms of the grid given
-- creates a list of lists, where each individual list contains the columns going from bottom to top to be passed to the helper
south grid s1 = (southHelper [a | ind <- reverse [0..((length s1)-1)], a <- [[x | y <- grid, x <- [(y !! ind)]]]] s1)

--checks that every column is valid
southHelper [] [] = True
southHelper columns s1 = (southHelper2 (reverse (head columns)) (head s1) 0 0) && (southHelper (tail columns) (tail s1))

-- checks that one column is valid
southHelper2 [] merchant rsf _ = merchant == rsf
southHelper2 column merchant rsf max 
	| merchant == 0 = True
	| (head column) > max = southHelper2 (tail column) merchant (rsf+1) (head column)
 	| otherwise = southHelper2 (tail column) merchant rsf max

-- validator check to see if west side traders all are true in terms of the grid given
west grid w1 = westHelper grid (reverse w1)

-- checks that every row is valid
westHelper [] [] = True
westHelper columns w1 = (westHelper2 (head columns) (head w1) 0 0) && (westHelper (tail columns) (tail w1))

-- checks that one row is valid
westHelper2 [] merchant rsf _ = merchant == rsf
westHelper2 column merchant rsf max 
	| merchant == 0 = True
	| (head column) > max = westHelper2 (tail column) merchant (rsf+1) (head column)
 	| otherwise = westHelper2 (tail column) merchant rsf max

-- validator check to see if east side traders all are true in terms of the grid given
east grid e1 = eastHelper grid e1

-- checks that every row is valid
eastHelper [] [] = True
eastHelper columns e1 = (eastHelper2 (reverse (head columns)) (head e1) 0 0) && (eastHelper (tail columns) (tail e1))

-- checks that one row is valid
eastHelper2 [] merchant rsf _ = merchant == rsf
eastHelper2 column merchant rsf max 
	| merchant == 0 = True
	| (head column) > max = eastHelper2 (tail column) merchant (rsf+1) (head column)
 	| otherwise = eastHelper2 (tail column) merchant rsf max

{-
TEST CASES: 

For puzzle input: 

[2,1]
[1,2]
[2,1]
[1,2]

Press any key and enter, then

[1,1,1]
[1,2,2]
[2,1,2]
[2,1,3] --invalid
[2,1]   --invalid
[2,2,2] --wrong answer
[2,2,1] --finish

For puzzle input:

[0,0,0,0]
[0,0,0,2]
[3,2,0,0]
[0,0,4,0]

Press any key and enter, then

[0,0,0]

Answer: [[4,3,2,1],[1,2,3,4],[2,1,4,3],[3,4,1,2]] (will display in different format)

For puzzle input:
[1,1]
[1,1]
[1,1]
[1,1]

Press any key and enter, then

[0,0,0] 

Answer: No solution


-}