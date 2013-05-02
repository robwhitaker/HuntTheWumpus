module Room (Room,buildRoom,buildEmptyRoom,mergeRoom,r_id,passage,contents,setSinglePassage,mergePassage,getPassAsStr) where 

	import Data.List(nub)

	type Passage = [[Bool]]

	--Contents key: 0 - bottomless pit, 1 - bats
	data Room = Room {r_id :: Int, passage :: Passage, contents :: [Int]} deriving (Eq)

	buildRoom :: Int -> Passage -> [Int] -> Room
	buildRoom a b c = Room a (setPassage b) c 

	buildEmptyRoom :: Room
	buildEmptyRoom = Room (-1) (setPassage [[False,False,False],[False,False,False],[False,False,False]]) []

	setPassage :: Passage -> Passage
	setPassage p
		| length p /= 3          					= error "Passage must have 3 rows."
		| foldr (||) False (map ((/=3) . length) p) = error "Passage must have 3 columns in each row."
		| otherwise								    = p

	mergePassage :: (Bool->Bool->Bool) -> Passage -> Passage -> Passage
	mergePassage c (a:as) (b:bs)
		| null as || null bs = [mergeRow a b]
		| otherwise          = (mergeRow a b):(mergePassage c as bs)
		where
			mergeRow (x:xs) (y:ys) | null xs || null ys = [c x y]
								   | otherwise          = (c x y):(mergeRow xs ys)

	mergeRoom :: Room -> Room -> Maybe Room
	mergeRoom r1 r2 
		| r_id r1 < 0  && r_id r2 < 0 = Nothing 
		| r_id r1 < 0				  = Just $ buildRoom (r_id r2) (mergePassage (||) (passage r1) (passage r2)) (nub $ concat [contents r1, contents r2])
		| otherwise					  = Just $ buildRoom (r_id r1) (mergePassage (||) (passage r1) (passage r2)) (nub $ concat [contents r1, contents r2])

	setSinglePassage :: Char -> Passage
	setSinglePassage c =
		case c of
			'n' -> setPassage [[False,True,False],[False,False,False],[False,False,False]]
			'e' -> setPassage [[False,False,False],[False,False,True],[False,False,False]]
			's' -> setPassage [[False,False,False],[False,False,False],[False,True,False]]
			'w' -> setPassage [[False,False,False],[True,False,False],[False,False,False]]
			_   -> setPassage [[False,False,False],[False,False,False],[False,False,False]]

	unsetSinglePassage :: Char -> Passage
	unsetSinglePassage c = map (map not) $ setSinglePassage c

	getPassAsStr :: Room -> String
	getPassAsStr r = (concat $ map getPassAtIndex [(0,1),(1,0),(1,2),(2,1)])
		where 
			p = passage r
			getPassAtIndex iii | iii == (0,1) && ((!!) ((!!) p (fst iii)) (snd iii)) = "n"
							   | iii == (1,0) && ((!!) ((!!) p (fst iii)) (snd iii)) = "w"
							   | iii == (1,2) && ((!!) ((!!) p (fst iii)) (snd iii)) = "e"
							   | iii == (2,1) && ((!!) ((!!) p (fst iii)) (snd iii)) = "s"
							   | otherwise    = ""  

	instance Show Room where
		show a = "<id:" ++ (show $ r_id a) ++ "," ++ "pass:" ++ getPassAsStr a ++ ">"