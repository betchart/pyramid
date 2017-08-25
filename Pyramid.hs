module Pyramid where

import qualified Data.Map as Map
import qualified Data.List as List
    
data Pyramid = Pyramid Int (Map.Map Point Char) deriving (Show)
data Point = Point Int Int Int deriving (Show, Eq, Ord)
data Shape = PyrI | PyrJ | PyrL | PyrP | PyrU | PyrZ

data Direction = PRY | PCG | PBM |
                 PRed | PYellow | PGreen | PCyan | PBlue | PMagenta |
                 PYG | PCB | PMR

-- (-1 -1 -1) (-1 -1 0) (-1 0 0)
-- (0 -1 -1) (0 -1 0) (0 0 -1)
-- (0 0 1) (0 1 0)  (0 1 1)
-- (1 0 0) (1 1 0)  (1 1 1)

emptyPyramid :: Int -> Pyramid
emptyPyramid size = Pyramid size $ Map.fromList []

add :: Pyramid -> [Point] -> Char -> Pyramid
add (Pyramid size mp) points c = Pyramid size $
                                foldl (\m p -> Map.insert p c m) mp points


displayPoint :: Point -> Pyramid -> String
displayPoint point (Pyramid size map) = case Map.lookup point map of
                                          Just c -> c:""
                                          Nothing -> "."

displayLayer :: Int -> Pyramid -> String
displayLayer iLayer pyr@(Pyramid size map) = unlines ([(replicate (2*(iLayer-j)) ' ') ++
                                                       (unwords [displayPoint (Point iLayer j k) pyr | k<-[0..j]]) ++
                                                       (replicate (2*(size-iLayer)) ' ') ++ "|"
                                                       | j<-[0..iLayer]
                                                      ]++
                                                     (replicate (size-iLayer-1) ((replicate (2*size) ' ')++"|")))
                                                     
display :: Pyramid -> String
display pyr@(Pyramid size _) = unlines $ map unwords ( List.transpose [lines (displayLayer i pyr)
                                                                       | i<-[0..(size-1)]])

        
-- rotations
-- 6 rotations on one plane
-- rotations fail with invalid PyrL, but reflections work
-- reflections fail to get all PyrU, but rotations work
-- translations
-- 
-- (!)
        
canonical :: Shape -> [Point]
canonical PyrI = map toPoint [(0,0,0), (1,0,0), (2,0,0)]
canonical PyrJ = map toPoint [(0,0,0), (1,0,0), (2,0,0), (3,1,0)]
canonical PyrP = map toPoint [(0,0,0), (4,1,0), (4,2,0), (4,2,1)]
canonical PyrU = map toPoint [(0,0,0), (4,1,0), (4,2,1), (4,2,2)]
canonical PyrZ = map toPoint [(0,0,0), (4,1,0), (4,2,1), (4,3,2)]
canonical PyrL = map toPoint [(1,0,0), (4,1,1), (4,2,1), (4,3,1)]

-- --generate all orientations from canonical
-- positions :: Shape -> 
-- PyrI 

           
toPoint :: (Int, Int, Int) -> Point
toPoint (i, j, k) = Point i j k
           
-- secondPoints :: Shape -> Point -> [Point]
-- secondPoints PyrI (Point i j k) = map toPoint [(i-1, j, k), (i+1, j, k),
--                                                (i, j-1, k), (i, j+1, k),
--                                                (i, j, k-1), (i, j, k+1),
--                                               (i-1, j-1, k), (i+1, j+1, k),
--                                               (i-1, j-1, k-1), (i+1, j+1, k+1)] --12
-- secondPoints PyrJ (Point i j k) = map toPoint [()]
                
-- points:: Shape
-- 
-- 
--          (0 0 0)
-- 
--          (1 0 0)
--      (1 1 0)  (1 1 1)
-- 
--          (2 0 0)
--      (2 1 0) (2 1 1)
--  (2 2 0) (2 2 1) (2 2 2)
-- 
--          (3 0 0)
--      (3 1 0) (3 1 1)
--  (3 2 0) (3 2 1) (3 2 2)
-- ( 330) ( 331) ( 332) ( 333)
-- 
--          (4 0 0)
--          ...
         
-- Each piece has a position and orientation specified by two points
-- Each piece additionally has a shape
-- Given an initial point of a piece (location, the possible orientations can be calculated.

--Given a piece and two points, the remaining points can be calculated
