module Main (main,initGame) where

	import Room
	import Map
	import Player
	import Wumpus
	import State
	import Utility(randBetween,toRoom)
	import Data.List(nub)

	main :: IO ()
	main = do 
			  putStr "Enter a number of rooms and a difficulty (0-2) separated by a space> "
			  initData <- fmap words getLine
			  putStrLn $ show $ initGame ((read . head) initData :: Int) ((read . last) initData :: Int)

	--        #Rooms   Difficulty
	initGame :: Int -> Int -> State
	initGame nRooms diff = state newMap (player playerArrows playerLocation) (wumpus wumpusLocation)
		where
			newMap = generateMap nRooms
			playerArrows = (!!) [5,3,1] diff
			playerLocation | (length $ contents randPlayerRoom) > 0 = playerLocation
						   | otherwise				 		        = r_id randPlayerRoom
						    where 
								randPlayerRoom = getRoomById (randBetween (0,getNumRooms newMap - 1)) newMap
			wumpusLocation = r_id $ (!!) (filter ((/=playerLocation) . r_id) $ getRoomsAsList newMap) (randBetween (0,getNumRooms newMap - 2))

	contains :: Room -> Wumpus -> Bool
	contains r w = (length $ contents r) > 0 || r_id r == wLocation w

	sense :: Player -> Map -> String
	sense p m = getDesc $ nub $ concat $ map contents nextRooms
		where
			nextRooms = getAdjRooms (getRoomById (pLocation p) m) m
			getDesc (c:cs) = 
				case c of
					0 -> "You feel a soft breeze eminating from somewhere in the caverns. " ++ getDesc cs
					1 -> "A flapping sound echoes through the tunnels. " ++ getDesc cs
					2 -> "A malodorous stench lingers in the cave. " ++ getDesc cs
					_ -> "" ++ getDesc cs
			getDesc _ = ""

	--                                       can the player move into this room? True/False
	move :: Player -> Map -> Char -> (Player,Bool)
	move p m c = 
		case c of
			'n' -> moveTo (fst currentRoomIndex - 1, snd currentRoomIndex)
			's' -> moveTo (fst currentRoomIndex + 1, snd currentRoomIndex)
			'e' -> moveTo (fst currentRoomIndex, snd currentRoomIndex + 1)
			'w' -> moveTo (fst currentRoomIndex , snd currentRoomIndex - 1)
		where
			currentRoomIndex = getRoomIndex (r_id $ getRoomById (pLocation p) m) m
			moveTo (row,col) | not $ c `elem` (getPassAsStr $ getRoomById (pLocation p) m) = (p, False)
							 | otherwise = (player (arrows p) (r_id $ toRoom $ getRoomAtIndex (row,col) m), True)

	--								  (Player, hit Wumpus?, can shoot in this direction?)
	shoot :: Player -> Wumpus -> Map -> Char -> (Player,Bool,Bool)
	shoot p w m c = 
		case c of
			'n' -> shootInto (fst currentRoomIndex - 1, snd currentRoomIndex)
			's' -> shootInto (fst currentRoomIndex + 1, snd currentRoomIndex)
			'e' -> shootInto (fst currentRoomIndex, snd currentRoomIndex + 1)
			'w' -> shootInto (fst currentRoomIndex , snd currentRoomIndex - 1)
		where
			currentRoomIndex = getRoomIndex (r_id $ getRoomById (pLocation p) m) m
			shootInto iii | not $ c `elem` (getPassAsStr $ getRoomById (pLocation p) m) = (p, False, False)
						  | wLocation w == (r_id $ toRoom $ getRoomAtIndex iii m) = (player (arrows p - 1) (pLocation p), True, True)
						  | otherwise = (player (arrows p - 1) (pLocation p), False, True)