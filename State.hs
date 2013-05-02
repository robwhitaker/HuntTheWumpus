module State (State,state,gMap,gPlayer,gWumpus) where
	
	import Map (Map)
	import Player (Player)
	import Wumpus (Wumpus)

	data State = State {gMap :: Map, gPlayer :: Player, gWumpus :: Wumpus}

	state :: Map -> Player -> Wumpus -> State
	state m p w = State m p w

	instance Show State where
		show a = "<S: \n" ++ (show $ gMap a) ++ "\n" ++ (show $ gPlayer a) ++ " " ++ (show $ gWumpus a) ++ "\n:>" 