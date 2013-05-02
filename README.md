HuntTheWumpus
=============

Hunt The Wumpus text-based game in Haskell.

---------------------------------------------------------------------------

NOTES ON THE MAP MODULE:

Implementation: generateMap :: Int -> Map

Errors: 
* There is still one bug where the last room the map generates occasionally contains a stray passage that does not connect to any other room.
