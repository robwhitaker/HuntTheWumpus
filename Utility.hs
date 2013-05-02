module Utility (randBetween,toRoom) where

	import System.Random(randomRIO)
	import System.IO.Unsafe(unsafePerformIO)
	import Room (Room,buildEmptyRoom)

	randBetween :: (Int,Int) -> Int
	randBetween range = unsafePerformIO $ randomRIO (fst range, snd range) :: Int

	toRoom :: Maybe Room -> Room
	toRoom (Just a) = a
	toRoom _		= buildEmptyRoom