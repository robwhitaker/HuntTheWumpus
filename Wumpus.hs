module Wumpus (Wumpus,wumpus,wLocation) where
	
	data Wumpus = Wumpus {wLocation :: Int}

	wumpus :: Int -> Wumpus
	wumpus loc = Wumpus loc

	instance Show Wumpus where
		show a = "<W location: " ++ (show $ wLocation a) ++ ">" 