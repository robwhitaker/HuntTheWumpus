module Wumpus where
	
	data Wumpus = Wumpus {wLocation :: Int}

	wumpus :: Int -> Wumpus
	wumpus loc = Wumpus loc