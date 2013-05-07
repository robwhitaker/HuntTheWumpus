module Map where 

	import Room
	import Data.List(findIndex,elemIndex)
	import Utility(randBetween,toRoom)

	data Map = Map {matrix :: [[Room]]}

	getNumRooms :: Map -> Int
	getNumRooms m = sum $ map roomsInRow (matrix m)
		where roomsInRow xs = length $ filter (/=buildEmptyRoom) xs

	getRoomIndex :: Int -> Map -> (Int, Int)
	getRoomIndex rid m = gri rs
		where
			rs = matrix m
			gri (x:xs) | null xs 								 = (toInt $ elemIndex x rs, toInt $ findIndex ((==rid) . r_id) x)
					   | findIndex ((==rid) . r_id) x == Nothing = gri xs
					   | otherwise							   	 = (toInt $ elemIndex x rs, toInt $ findIndex ((==rid) . r_id) x)
			toInt (Just a) = a
			toInt _		   = -1

	getRoomAtIndex :: (Int,Int) -> Map -> Maybe Room
	getRoomAtIndex (row,col) m
		| row < 0 || col < 0 || row >= (length $ matrix m) || col >= (length $ (!!) (matrix m) row) = Nothing
		| otherwise					 = Just $ (!!) ((!!) (matrix m) row) col

	getRoomById :: Int -> Map -> Room
	getRoomById  rid m = toRoom $ getRoomAtIndex (getRoomIndex rid m) m

	getRoomsAsList :: Map -> [Room]
	getRoomsAsList m = filter (/=buildEmptyRoom) $ concat $ matrix m

	addRoomAtIndex :: (Int,Int) -> Room -> Map -> Map
	addRoomAtIndex iii r m
		| length (matrix m) <= fst iii 					= addRoomAtIndex iii r $ addRows (matrix m)
		| length ((!!) (matrix m) (fst iii)) <= snd iii = addRoomAtIndex iii r $ addCols ((!!) (matrix m) (fst iii))
		| otherwise 									= Map $ take (fst iii) (matrix m) ++ [addToRow ((!!) (matrix m) (fst iii))] ++ drop (fst iii + 1) (matrix m)
		where
			addToRow rs = take (snd iii) rs ++ [toRoom $ mergeRoom (toRoom $ getRoomAtIndex iii m) r] ++ drop (snd iii + 1) rs
			addRows matr = Map $ matr ++ (replicate (fst iii - length matr + 1) [])
			addCols row = Map $ take (fst iii) (matrix m) ++ [row ++ (replicate (snd iii - length row + 1) buildEmptyRoom)] ++ drop (fst iii + 1) (matrix m)

	generateMap :: Int -> Int -> Map
	generateMap n diff
		| n < 2     = error "Game must contain two or more rooms."
		| otherwise = addTraps (genMap n (0,0) 'x' $ Map [[buildEmptyRoom]]) diff

	genMap :: Int ->(Int,Int) -> Char -> Map -> Map
	genMap n iii bpass m
		| getNumRooms m == n     = m -- nextIndex newRoomGoesBackTo    currentIndex                                       currentRoomGoesTo
		| getNumRooms m == 0 	 = genMap n (0,1) 'w' $ addRoomAtIndex (0,0) (buildRoom (getNumRooms m) (setSinglePassage 'e') []) m
		| otherwise			 	 = branch (fst getRandDir) (fst $ snd getRandDir) (snd $ snd getRandDir)
		where
			randNum = randBetween (1,100)
			maxBounds = ceiling $ sqrt $ fromIntegral n :: Int
			getRandDir 
				| randNum <= 100 && randNum > 75 = ((fst iii - 1, snd iii),('s','n')) --north
				| randNum <= 75 && randNum > 50  = ((fst iii, snd iii + 1),('w','e')) --east
				| randNum <= 50 && randNum > 25  = ((fst iii + 1, snd iii),('n','s')) --south
				| otherwise						 = ((fst iii, snd iii - 1),('e','w')) --west
			branch newIndex bp dir
				| fst newIndex < 0 || snd newIndex < 0 || fst newIndex >= maxBounds || snd newIndex >= maxBounds = genMap n iii bpass m
				| getNumRooms m == n-1 && (toRoom $ getRoomAtIndex iii m) == buildEmptyRoom = addRoomAtIndex iii (buildRoom (getNumRooms m) (setSinglePassage bpass) []) m
				| otherwise			   = genMap n newIndex bp $ addRoomAtIndex iii (buildRoom (getNumRooms m) (mergePassage (||) (setSinglePassage bpass) (setSinglePassage dir)) []) m

	addTraps :: Map -> Int -> Map
	addTraps gMap diff = Map $ map (map trap) (matrix gMap)
		where
			trap r 
				| r_id r <= 0 = r
				| otherwise	  = toRoom $ mergeRoom getBats getPit
				where
					randN = randBetween (1,100)
					getPit  = if (randN <= 10 + (diff * 12) && randN > 0) then (toRoom $ mergeRoom r (buildRoom (-1) (setSinglePassage 'x') [0])) else r
					getBats = if (randN <= 100 && randN > 95 - (diff * 5)) then (toRoom $ mergeRoom r (buildRoom (-1) (setSinglePassage 'x') [1])) else r

	getAdjRooms :: Room -> Map -> [Room]
	getAdjRooms r m = map toRoom $ filter (/=Nothing) $ map getRoomAtPassage [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
		where
			currentIndex = getRoomIndex (r_id r) m
			getRoomAtPassage iii
				| (!!) ((!!) (passage r) (fst iii)) (snd iii) && iii /= (1,1) = getRoomAtIndex (fst currentIndex + fst iii - 1, snd currentIndex + snd iii -1) m
				| otherwise = Nothing 

	printMap :: Map -> IO ()
	printMap a = putStr $ show a

	instance Show Map where
		show a = display (matrix a)
			where
				display (x:xs) = (show x) ++ "\n" ++ display xs
				display _	   = "" 