module Utility (randBetween,toRoom,isNumericString) where

	import System.Random(randomRIO)
	import System.IO.Unsafe(unsafePerformIO)
	import Data.Char(isDigit)
	import Room (Room,buildEmptyRoom)

	randBetween :: (Int,Int) -> Int
	randBetween range = unsafePerformIO $ randomRIO (fst range, snd range) :: Int

	toRoom :: Maybe Room -> Room
	toRoom (Just a) = a
	toRoom _		= buildEmptyRoom

	isNumericString :: String -> Bool
	isNumericString ds 
		| length ds > 0 = isNumStr ds
		| otherwise 	= False 
		where
			isNumStr (x:xs) = isDigit x && isNumStr xs
			isNumStr _	   = True