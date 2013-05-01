module State where
	
	import Map
	import Player
	import Room
	import Wumpus

	data State = State {gMap :: Map, gPlayer :: Player, gWumpus :: Wumpus}

	state :: Map -> Player -> Wumpus -> State
	state m p w = State m p w

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
