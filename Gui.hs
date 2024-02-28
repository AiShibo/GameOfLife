import GameOfLife
import VariationOfLife
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Euterpea -- distinguish between gloss play
import System.Exit (exitSuccess)

{-
Data structure to keep track of changes to gloss interface and the world

WorldState with 
- World - contains world data structure
- Diameter - diameter of the world (e.g. 20 -> 40x40 grid) 
- Size - size of each cell on the window
- Auto - whether to automatically advance/evolve to next step (used in update)
- Paint - whether to automatically alive/unalive cells on mouse movement
- - random generator 
-}

type Diameter = Float
type Size = Float
type Auto = Bool
type Paint = Bool
data WorldState = WorldState World Diameter Size Auto Paint

-- diameter of grid (e.g. 20 means 40x40 grid)
diameter :: Diameter
diameter = 20

-- size of cell
size :: Size
size = 10

-- specify fps
fps :: Int
fps = 1

-- How much you want to increase/decrease the diameter by
diameterAdjustSize :: Diameter
diameterAdjustSize = 5

-- How much you want to zoom in/out by
zoomAdjustSize :: Size
zoomAdjustSize = 1

{-
This function takes our custom defined world state and transform it into a picture which Gloss can understand

Adapted from: https://stackoverflow.com/questions/42315653/draw-squares-for-a-game-board-in-haskell
4x4 grid with coordinates, we will look at quadrant 1 with diameter 2:
|0,1|1,1|
---------
|0,0|1,0|
for the following cells,
(0,0), we translate (0,0) pixels
(1,0), we translate (size,0) pixels
(0,1), we translate (0,size) pixels
(1,1), we translate (size,size) pixels

we can calculate this more generally,
(i,j), we translate (i * size, j * size) pixels
and
x_translate = i * size
y_translate = j * size

e.g. a 2x2 red-green checkerboard
(color green (translate 0 0 (rectangleSolid size size))),
(color red (translate size 0 (rectangleSolid size size))),
(color red (translate 0 size (rectangleSolid size size))),
(color green (translate size size (rectangleSolid size size)))
-}
drawWorldState :: WorldState -> IO Picture
drawWorldState (WorldState (World worldcoord alivecells) _ size _ _) = return (Pictures [
        color black (translate x_translate y_translate (rectangleSolid size size)) |
        (i, j) <- alivecells,
        let x_translate = fromIntegral i * size,
        let y_translate = fromIntegral j * size
    ])

{-
This function maps keyboard and mouse events to change the world state

Keybinds:
"Enter" to advance the world
"t" to reset the grid
"a" to toggle automatically advance to next world
"+/=" to increase diameter
"-/_" to decrease diameter
"]/}" to zoom out
"[/{" to zoom in
"p" to toggle paint mode
"r" to generate random grid
"m" to play music for current grid
"h" to show help message
"Esc" to exit program
-}
handleEvents :: Event -> WorldState -> IO WorldState
handleEvents (EventKey (MouseButton LeftButton) Down _ (x, y)) (WorldState (World worldcoord alivecells) diameter size auto paint) =
    return (if getCellAtCoordinate (seedTheWorld alivecells) (round (x / size), round (y / size))
                then WorldState (seedTheWorld $ updateAliveCells alivecells (round (x / size), round (y / size)) False) diameter size auto paint
                else WorldState (seedTheWorld $ updateAliveCells alivecells (round (x / size), round (y / size)) True) diameter size auto paint)
-- "Enter" to advance the world
handleEvents (EventKey (SpecialKey KeyEnter) Down _ _) (WorldState world diameter size auto paint) =
    return (WorldState (willToPowerEvolution world) diameter size auto paint)
-- "t" to reset the grid
handleEvents (EventKey (Char 't') Down _ _) (WorldState (World worldcoord alivecells) diameter size auto paint) =
    return (WorldState initialWorld diameter size auto paint)
-- toggle "a" to automatically advance to next world
handleEvents (EventKey (Char 'a') Down _ _) (WorldState world diameter size auto paint) =
    return (WorldState world diameter size (not auto) paint)
-- "+/=" to increase diameter, "-/_" to decrease diameter
handleEvents (EventKey (Char '=') Down _ _) (WorldState world diameter size auto paint) = do
    putStrLn ("Current diameter: " ++ show (min 100 (diameter + diameterAdjustSize)))
    return (WorldState world (min 100 (diameter + diameterAdjustSize)) size auto paint)
handleEvents (EventKey (Char '-') Down _ _) (WorldState world diameter size auto paint) = do
    putStrLn ("Current diameter: " ++ show (max 0 (diameter - diameterAdjustSize)))
    return (WorldState world (max 0 (diameter - diameterAdjustSize)) size auto paint)
-- "]/}" to zoom out, "[/{" to zoom in
handleEvents (EventKey (Char '[') Down _ _) (WorldState world diameter size auto paint) = do
    putStrLn ("Current size: " ++ show (min 50 (size + zoomAdjustSize)))
    return (WorldState world diameter (min 50 (size + zoomAdjustSize)) auto paint)
handleEvents (EventKey (Char ']') Down _ _) (WorldState world diameter size auto paint) = do
    putStrLn ("Current size: " ++ show (max 1 (size - zoomAdjustSize)))
    return (WorldState world diameter (max 1 (size - zoomAdjustSize)) auto paint)
-- "p" to toggle paint mode
handleEvents (EventKey (Char 'p') Down _ _) (WorldState world diameter size auto paint) =
    return (WorldState world diameter size auto (not paint))
-- if paint mode is active, paint with mouse cursor
handleEvents (EventMotion (x, y)) (WorldState (World worldcoord alivecells ) diameter size auto True) =
    return (WorldState (seedTheWorld $ updateAliveCells alivecells (round (x / size), round (y / size)) True) diameter size auto True)
-- "r" to generate random grid
handleEvents (EventKey (Char 'r') Down _ _) (WorldState (World worldcoord alivecells) diameter size auto paint) = do
    new_gen <- newStdGen
    return (WorldState (seedTheWorld $ generateRandomPairs (round diameter * 20) new_gen (round diameter)) diameter size auto paint)
-- "m" to play music
handleEvents (EventKey (Char 'm') Down _ _) (WorldState world diameter size auto paint) = do
    Euterpea.play $ lifePlayer (round diameter * 2) world
    return (WorldState (willToPowerEvolution world) diameter size auto paint)
-- "h" to print help message
handleEvents (EventKey (Char 'h') Down _ _) worldstate = do
    showHelp
    return worldstate
-- "Esc" to quit
handleEvents (EventKey (SpecialKey KeyEsc) Down _ _) worldstate = exitSuccess
handleEvents _ gw = return gw

-- This function to automatically update the grid by the specified number of frames per second
updateWorldState :: Float -> WorldState -> IO WorldState
updateWorldState time (WorldState world diameter size True paint) =
    return (WorldState (willToPowerEvolution world) diameter size True paint)
updateWorldState time (WorldState world diameter size False paint) =
    return (WorldState world diameter size False paint)

-- Show help message
showHelp :: IO ()
showHelp = do
    putStrLn "Help Menu"
    putStrLn "Keybinds:"
    putStrLn "\"Enter\"\t - advance the world"
    putStrLn "\"t\"\t - reset the grid"
    putStrLn "\"a\"\t - toggle automatically advance to next world"
    putStrLn "\"+\"/\"=\"\t - increase diameter"
    putStrLn "\"-\"/\"_\"\t - decrease diameter"
    putStrLn "\"]\"/\"}\"\t - zoom out"
    putStrLn "\"[\"/\"{\"\t - zoom in"
    putStrLn "\"p\"\t - toggle paint mode"
    putStrLn "\"r\"\t - generate random grid"
    putStrLn "\"m\"\t - music for current grid"
    putStrLn "\"h\"\t - show this menu"
    putStrLn "\"Esc\"\t - quit program"

-- Main entry of the gui program
-- To launch the program, do
-- ghci> main
main :: IO ()
main = do
    putStrLn "Welcome to Game of Life!"
    showHelp
    playIO
        (InWindow "Game of Life" (600, 600) (10, 10)) -- display
        white -- background color
        fps -- number of frames per second
        (WorldState initialWorld diameter size False False) -- initial WorldState
        drawWorldState -- draw grid world
        handleEvents -- event handler
        updateWorldState -- update grid by fps
