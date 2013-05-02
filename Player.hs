module Player (Player,player,arrows,pLocation) where

	data Player = Player {arrows :: Int, pLocation :: Int}

	player :: Int -> Int -> Player
	player arrs loc = Player arrs loc

	instance Show Player where
		show a = "<P arrows: " ++ (show $ arrows a) ++ ",location: " ++ (show $ pLocation a) ++ ">" 
