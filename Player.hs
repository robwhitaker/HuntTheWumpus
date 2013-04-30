module Player (arrows,location,sense) where
	
	import Map
	import Room
	import Data.List(nub)

	data Player = Player {arrows :: Int, location :: Int}

	sense :: Player -> Map -> String
	sense p m = getDesc $ nub $ concat $ map contents nextRooms
		where
			nextRooms = getAdjRooms (getRoomById (location p) m) m
			getDesc (c:cs) = 
				case c of
					0 -> "You feel a soft breeze eminating from somewhere in the caverns. " ++ getDesc cs
					1 -> "A flapping sound echoes through the tunnels. " ++ getDesc cs
					2 -> "A malodorous stench lingers in the cave. " ++ getDesc cs
					_ -> "" ++ getDesc cs
			getDesc _ = ""

	--move :: Player -> Map -> String -> Player

	--shoot :: Player -> Map -> String -> (Player,Map)
