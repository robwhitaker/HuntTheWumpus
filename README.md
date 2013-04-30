HuntTheWumpus
=============

Hunt The Wumpus text-based game in Haskell.

---------------------------------------------------------------------------

NOTES ON THE MAP MODULE:

Implementation: generateMap :: Int -> Map

Errors: 
* There is still one bug where the last room the map generates occasionally contains a stray passage that does not connect to any other room. 

Future additions: 
* Add bounds to the matrix so the Map doesn't spread out too far and will instead be more concentrated allowing for more room connections. 
* Add passability to the diagonals instead of just north, east, south and west. 
* Add a second pass over the map to add additional room connections (with the goal of 3).
