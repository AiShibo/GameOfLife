module GameOfLife where
import GHC.Base (IO())
-------------------------- types
type Cell = Bool
type Quadrant = [[Cell]]
type WorldCoord = (Quadrant, Quadrant, Quadrant, Quadrant)
type Coordinate = (Int, Int)
type AliveCells = [Coordinate]
type CandidateCells = [Coordinate]
data World = World WorldCoord AliveCells -- because in this game the world cannot be "dead", thus only one constructor :)
    deriving(Show)

{-

how the world was setup:

The world centers at (0,0), you can imagine it as a 2D Cartesian coordinate, 
which x and y goes from negative infinity to positive infinity

to represent such coordinate, we use four 2d lists (one each quadrant). 

for example a coordinate (1,2) will be passed to list representing quadrant one, 
and its value (the cell's status) can be retreved or modified by just getting lst!!1!!2

if a coordinate is negative, we will pass it to the corresponding quadrant list, but
accessing elements through the absolute value of the coordinate

for example (-1,-2) will be passed to list representing quadrant 3, and we will use (1,2) to access
the cell's status

this way we can assure fast cell status query/modification while using intuitive idexings


----------------------------

However, because the world is infinitely large, traversing the world to find all the alive cells is impossible.
thus, we also maintain a list of alive cells, to achieve the efficient world iteration/evolution.

-}

-- Given a coordinate of a cell, returns a list of coordinates of its neighbours

neighborCoordinates :: Coordinate -> [Coordinate]
neighborCoordinates (x,y) = [
            (x,y+1),
            (x,y-1),
            (x-1,y),
            (x+1,y),
            (x-1,y+1),
            (x+1,y+1),
            (x-1,y-1),
            (x+1,y-1)
        ]

-- create a infinite quadrant, each cell has an initial value False (dead)
infiniteQuadrant :: Quadrant
infiniteQuadrant = repeat (repeat False)

-- create a infinite world coordinate with four infinite quadrant
initialWorldCoord :: WorldCoord 
initialWorldCoord = (infiniteQuadrant, infiniteQuadrant, infiniteQuadrant, infiniteQuadrant)

-- a list keeps track of all the alive cells in the world
initialAliveCells :: AliveCells
initialAliveCells = []

-- A world is made up of an infinite coordinate and an alive list (as explained above)
initialWorld :: World
initialWorld = World initialWorldCoord initialAliveCells

-- this is a helper function to update the alive cell list.
-- it accepts an alive cell list and a coordinate, and the status of the cell, then it returns an updated alive cell list
-- example1: updateAliveCells [] (0,0) True -> [(0,0)]
-- example2: updateAliveCells [(0,0)] (0,0) False -> []
updateAliveCells :: AliveCells -> Coordinate -> Cell -> AliveCells
updateAliveCells originalCells coord isAlive
    | isAlive && not (coord `elem` originalCells) = coord : originalCells
    | not isAlive && coord `elem` originalCells = filter (/= coord) originalCells
    | otherwise = originalCells -- shouldn't reach here, but just in case :)

-- this is a helper function to update the status of the cell in each quadrant
-- it accetps a quadrant, and a coordinate (each x and y has to be bigger than or equals to zero), and a status of a cell, and returns an update quadrant
updateQuadrant :: Quadrant -> Coordinate -> Cell -> Quadrant
updateQuadrant oldQuadrant (x, y) newVal = 
    take x oldQuadrant ++ [updateVal] ++ drop (x + 1) oldQuadrant
    where updateVal = take y (oldQuadrant !! x) ++ [newVal] ++ drop (y + 1) (oldQuadrant !! x)

-- this is a helper function to update the world, it maintains both the world coordinate and alive cell list
-- it takes an old world, and a coordinate, and cell's status, and returns a new world
-- one of the key function for this game
updateWorld :: World -> Coordinate -> Cell -> World
updateWorld (World (one, two, three, four) oldAliveCells) (x, y) newVal
    | x >= 0 && y >= 0 = World (updateQuadrant one (abs x, abs y) newVal, two, three, four) newAlives
    | x < 0 && y >= 0 = World (one, updateQuadrant two (abs x, abs y) newVal, three, four) newAlives 
    | x < 0 && y < 0 = World (one, two, updateQuadrant three (abs x, abs y) newVal, four) newAlives 
    | x >= 0 && y < 0 = World (one, two, three, updateQuadrant four (abs x, abs y) newVal) newAlives 
    where newAlives = updateAliveCells oldAliveCells (x, y) newVal

-- this is a helper function to determine the status of a cell(alive or dead) at that location in a given world
getCellAtCoordinate :: World -> Coordinate -> Cell
getCellAtCoordinate (World (one, two, three, four) _) (x,y)
    | x >= 0 && y >= 0 = one!!absx!!absy
    | x < 0 && y >= 0 = two!!absx!!absy
    | x < 0 && y < 0 = three!!absx!!absy
    | x >= 0 && y < 0 = four!!absx!!absy
        where 
            absx = abs x
            absy = abs y

-- simple helper function to extract the alive list from a world data complex
getAliveList :: World -> AliveCells
getAliveList (World (_, _, _, _) aliveList) = aliveList

-- calculate the fate of a given cell
-- think of it as a question "in this given world, will this cell be alive or dead in the next iteration/evolution?"
-- this is the function contains 3 rules for the game
-- key function of the game
fate :: World -> Coordinate -> Cell
fate world (x, y)
    | not alive && count == 3 = True
    | alive && (count == 2 || count == 3) = True
    | otherwise = False
    where
        alive = getCellAtCoordinate world (x, y)
        count = length $ filter id [getCellAtCoordinate world (nx, ny) | (nx, ny) <- neighborCoordinates (x,y)] -- id is just \x -> x :)

{-
here i will further explain what is a candidate cell.

A candidate cell is a cell that could not be certain if it will be alive/dead in the next evolution/iteration

it can be computed as follows: all the alive cells, with their neighbours

because all the alive cells could be dead in the next evolution/iteration, 
and only the neighbour of alive cells could be new born cells
-}

-- this helper function compute the candidate cell list from an alive cell list (elements may duplicate)
mergeCandidate :: AliveCells -> CandidateCells
mergeCandidate [] = []
mergeCandidate (ele:aliveCells) = candidates ++ mergeCandidate aliveCells
    where
        candidates = ele:neighborCoordinates ele

-- function removes the duplicate element in a candidate cell list
removeDuplicateCandidates :: CandidateCells -> CandidateCells
removeDuplicateCandidates = foldr (\x y -> if x `elem` y then y else x:y) []

-- just another helper function, combines two previous functions, 
-- returning a list of candidate cells (without duplication), from a given alive cell list
getCandidate :: AliveCells -> CandidateCells
getCandidate candidate = removeDuplicateCandidates $ mergeCandidate candidate

-- this is the helper function computes each candidate cell's status in the new world recursively
evolutionHelper :: World -> CandidateCells -> World 
evolutionHelper world [] = world
evolutionHelper world ((x,y):lst) = updateWorld rest (x,y) (fate world (x,y))
    where
        rest = evolutionHelper world lst


-- the main function fore the game, because this is a zero player game, you pass in an old world, it gives you the next world's status
willToPowerEvolution :: World -> World
willToPowerEvolution oldWorld = evolutionHelper oldWorld (getCandidate (getAliveList oldWorld))
------------------------------------- main game logic ends here

printWorldHelper :: Int -> Int -> Int -> Int -> World -> IO ()
printWorldHelper x y target dim world
    | x == target + 1 && y == 0 = putChar '\n'
    | x == target + 1 = do
        putChar '\n'
        printWorldHelper 0 (y - 1) target dim world
    | otherwise = do
        -- putStr (show (x-dim, y-dim)) 
        let cell = getCellAtCoordinate world (x-dim, y-dim)
        if cell then do
            -- putStr (show (x, y))
            putStr "o  "

        else do
            -- putStr (show (x, y))
            putStr ".  "
        printWorldHelper (x + 1) y target dim world

-- function to print a world in the terminal, 'o' means alive abnd '.' means dead. world centers
-- at (0,0), the first parameter is diameter, if you input 10, then it will print a cube from (10,10) to (-10,-10) diagnal.
-- note because of the existance of x = 0 and y = 0, the actual length of the edge is diameter*2 + 1
printWorld :: Int -> World -> IO ()
printWorld diameter world = printWorldHelper 0 (diameter*2) (diameter*2) diameter world


-- initialize the first world, making all the cells at the specified coordinates true.
seedTheWorld :: [Coordinate] -> World
seedTheWorld [] = initialWorld
seedTheWorld (coordinate:lst) = updateWorld (seedTheWorld lst) coordinate True