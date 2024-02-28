module VariationOfLife where
import Euterpea
import System.Random
import GameOfLife

-- a bank of notes, only notes avaliable to compose the music
-- their index represents their location on y axis.
-- for example, note qn (C, 5) is the number zero element in this list
-- it will play when there's a cell alive on on line y = 0
middleOctaves :: [Music Pitch]
middleOctaves = [
    note qn (C, 5), note qn (D, 5), note qn (E, 5), note qn (F, 5), note qn (G, 5), note qn (A, 5), note qn (B, 5),
    note qn (C, 6), note qn (D, 6), note qn (E, 6), note qn (F, 6), note qn (G, 6), note qn (A, 6), note qn (B, 6),
    note qn (C, 7)]

-- helper function, given a list of alive cells, and a column number, output a chord of that column
getChord :: AliveCells -> Int -> Music Pitch
getChord aliveList column = foldr (:=:) (rest qn) (getNoteFromCoordinate getAliveAtColumn)
    where
        -- fixed value for y, due to the limitation of number of notes we have
        getAliveAtColumn = [x | x <- aliveList, fst x == column, snd x < 15, snd x >= 0]
        getNoteFromCoordinate coord = [middleOctaves!!snd x | x <- coord]

-- this helper function returns a segment of music by calling getChord on every column specified by the user
-- the first int represents the number of columns each segment will consist of
lifePlayer :: Int -> World -> Music Pitch
lifePlayer width (World _ aliveCells) = foldr (:+:) (rest 0) chords
    where chords = [getChord aliveCells x | x <- [0..(width - 1)]]


generateRandomPairs :: Int -> StdGen -> Int -> [(Int, Int)]
generateRandomPairs n gen diameter = 
    let 
        (gen1, gen2) = split gen
        xs = randomRs (-diameter, diameter) gen1
        ys = randomRs (-diameter, diameter) gen2
    in take n (zip xs ys)