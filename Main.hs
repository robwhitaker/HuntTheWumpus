module Main (main) where

	import Room
	import Map
	import Player
	import Wumpus
	import State
	import Utility(randBetween,toRoom)
	import Data.List(nub)
	import System.IO

	main :: IO ()
	main = do
			putStr "Enter a number of rooms and a difficulty (0-2) separated by a space> "
			hFlush stdout
			initData <- fmap words getLine
			let numRooms   = ((read . head) initData :: Int)
			let	difficulty = ((read . last) initData :: Int)
			let gameState  = initGame numRooms difficulty 
			putStrLn "\nBuilding map..."
			if (numRooms == (getNumRooms $ gMap gameState)) then (putStrLn "Map successfully generated... \nBeginning game...\n") else putStrLn ""
			putStrLn $ initialDescription gameState ++ "\n"
			putStr $ sense gameState ++ (getPassageText $ getRoomById (pLocation $ gPlayer gameState) (gMap gameState))
			gameLoop gameState --begin game loop

	gameLoop :: State -> IO ()
	gameLoop gameState = do
					putStr "\n--> "
					hFlush stdout
					input <- getLine
					case input of
						"state" 					 -> do {putStrLn $ show gameState; gameLoop gameState}
						"help"  					 -> do {putStrLn showHelp; gameLoop gameState}
						'm':'o':'v':'e':' ':_:[] 	 -> handleMove gameState $ (head . last . words) input
						's':'h':'o':'o':'t':' ':_:[] -> do {putStrLn input; gameLoop gameState}
						"exit"  					 -> putStrLn "Game exited."
						_      						 -> do {putStr ("#Error: Invalid command - " ++ input); gameLoop gameState}

	handleMove :: State -> Char -> IO ()
	handleMove gameState dir 
		| (snd $ move (gPlayer gameState) (gMap gameState) dir) = do {putStr ("You walk into the next room. "); eventHandler newState}
		| otherwise			   = do 
									putStr "As you stumble in the dark, you carelessly bump into a wall. Perhaps you should try a different direction. " 
									gameLoop newState
		where 
			newState = state (gMap gameState) (fst $ move (gPlayer gameState) (gMap gameState) dir) (gWumpus gameState)

	eventHandler :: State -> IO ()
	eventHandler gameState
		| (pLocation $ gPlayer gameState) == (wLocation $ gWumpus gameState) = putStrLn "You barely have a moment to notice that the room reeks of decay when you feel the sharp teeth of the Wumpus dig into your flesh. \n\nGAME OVER.\n"
		| 0 `elem` (contents $ getRoomById (pLocation $ gPlayer gameState) (gMap gameState)) = putStrLn "As you step forward, you realize a moment too late that the ground has dropped off into a deep chasm.  Your foot slips, and you tumble into the pit.\n\nGAME OVER.\n"
		| 1 `elem` (contents $ getRoomById (pLocation $ gPlayer gameState) (gMap gameState)) = do 
																								 putStr batDescription
																								 eventHandler batRelocationState
		| otherwise = do
						putStr $ sense gameState ++ (getPassageText $ getRoomById (pLocation $ gPlayer gameState) (gMap gameState))
						gameLoop gameState
		where
			batRelocationState = state (gMap gameState) ( player (arrows $ gPlayer gameState) (r_id $ (!!) roomListNoBats (randBetween (0, length roomListNoBats - 1))) ) (gWumpus gameState)
			roomListNoBats = filter (not . elem 1 . contents) $ getRoomsAsList $ gMap gameState 

	--        #Rooms   Difficulty
	initGame :: Int -> Int -> State
	initGame nRooms diff = state newMap (player playerArrows playerLocation) (wumpus wumpusLocation)
		where
			newMap = generateMap nRooms
			playerArrows = (!!) [5,3,1] diff
			playerSafeRooms = filter ((<1) . length . contents) $ getRoomsAsList newMap
			playerLocation = r_id $ (!!) playerSafeRooms (randBetween (0, length playerSafeRooms - 1))
			wumpusLocation = r_id $ (!!) (filter ((/=playerLocation) . r_id) $ getRoomsAsList newMap) (randBetween (0,getNumRooms newMap - 2))

	contains :: Room -> Wumpus -> Bool
	contains r w = (length $ contents r) > 0 || r_id r == wLocation w

	sense :: State -> String
	sense gameState 
		| (length $ filter ((==wLocation w) . r_id) nextRooms) > 0 = (getDesc $ nub $ concat $ map contents nextRooms) ++ "A malodorous stench lingers in the cave. "
		| otherwise = (getDesc $ nub $ concat $ map contents nextRooms)
		where
			p = gPlayer gameState
			w = gWumpus gameState
			m = gMap gameState
			nextRooms = getAdjRooms (getRoomById (pLocation p) m) m
			getDesc (c:cs) = 
				case c of
					0 -> "You feel a soft breeze eminating from somewhere in the caverns. " ++ getDesc cs
					1 -> "A flapping sound echoes through the tunnels. " ++ getDesc cs
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
			_	-> (p,False)
		where
			currentRoomIndex = getRoomIndex (r_id $ getRoomById (pLocation p) m) m
			moveTo (row,col) | not $ c `elem` (getPassAsStr $ getRoomById (pLocation p) m) = (p, False)
							 | otherwise = (player (arrows p) (r_id $ toRoom $ getRoomAtIndex (row,col) m), True)

	--								  (Player, hit Wumpus?, can shoot in this direction?)
	shoot :: State -> Char -> ((Player,Bool),Bool)
	shoot gameState c = 
		case c of
			'n' -> shootInto (fst currentRoomIndex - 1, snd currentRoomIndex)
			's' -> shootInto (fst currentRoomIndex + 1, snd currentRoomIndex)
			'e' -> shootInto (fst currentRoomIndex, snd currentRoomIndex + 1)
			'w' -> shootInto (fst currentRoomIndex , snd currentRoomIndex - 1)
		where
			p = gPlayer gameState
			w = gWumpus gameState
			m = gMap gameState
			currentRoomIndex = getRoomIndex (r_id $ getRoomById (pLocation p) m) m
			shootInto iii | not $ c `elem` (getPassAsStr $ getRoomById (pLocation p) m) = ((p, False), False)
						  | wLocation w == (r_id $ toRoom $ getRoomAtIndex iii m) = ( (player (arrows p - 1) (pLocation p), True), True)
						  | otherwise = ( (player (arrows p - 1) (pLocation p), False), True)

	-- GAMEPLAY TEXT --

	showHelp :: String
	showHelp = "================================\n"
			++ "Hunt The Wumpus HELP MENU\n"
			++ "================================\n\n"
			++ "Gameplay Commands\n"
			++ "-----------------\n"
			++ "  move [n/e/s/w]\n"
			++ "  shoot [n/e/s/w]\n\n"
			++ "Other Commands\n"
			++ "--------------\n"
			++ "  help\n"
			++ "  exit"

	initialDescription :: State -> String
	initialDescription gameState =
						 "==================================================\n" ++
						 "HUNT THE WUMPUS (type 'help' for the help menu)\n" ++
						 "==================================================\n\n" ++
						 "You wake up to the lingering scent of sulfur and a dull throbbing in the back of your head. " ++
						 "Somewhere behind this veil of darkness, water drips onto a solid stone floor. " ++
						 "It sounds like you are in some sort of cavern.  As you prop yourself into sitting position, " ++
						 "your hand lands on a curved, wooden object.  It's a bow.  Suddenly you remember why you came down here. " ++
						 "You volunteered to hunt down the monster that has been terrorizing your village. The Wumpus. " ++
						 "Snatching up your quiver, which landed next to your bow when you fell into the cave, you count up " ++
						 "the arrows that are still usable after the fall. There are only " ++ (show $ arrows $ gPlayer gameState) ++ " " ++
						 "left. Better use them wisely. Feeling around the cave floor, you realize that your torch is missing. " ++
						 "Dread sinks in as you become aware that you can barely see.  You will have to use your other senses to survive " ++
						 "the caverns."

	batDescription :: String
	batDescription = "A horrible screeching rips through the cavern, forcing you to your knees as you clench your ears. " ++
			 	 	 "You suddenly feel large claws dig into your shoulders and tear you off the ground. Looking up, you can make out the silhouette " ++
			 	 	 "of a monstrous bat. Air rushes by your face as your struggle to free yourself, and finally the bat loses its grip, " ++
			 	 	 "sending you clattering into the ground. After a moment, you manage to get to your feet, only to realize you have no idea where you are. "