module Pyramid where

import qualified Data.Map as Map
import qualified Data.List as List
import StrUtils
    
data Pyramid = Pyramid Int (Map.Map Point Char) deriving (Show)
data Point = Point Int Int Int deriving (Show, Eq, Ord)
data Shape = PyrI | PyrJ | PyrL | PyrP | PyrU | PyrZ

toPoint :: (Int, Int, Int) -> Point
toPoint (i, j, k) = Point i j k

canonical :: Shape -> [Point]
canonical PyrI = map toPoint [(0,0,0), (1,0,0), (2,0,0)] -- 6 : 1 for each edge of a tetrahedron
canonical PyrJ = map toPoint [(0,0,0), (1,0,0), (2,0,0), (3,1,0)] -- 48 : 8 for each of 6 edges
canonical PyrP = map toPoint [(0,0,0), (1,0,0), (2,0,0), (2,1,0)] -- 48 : 8 for each of 6 edges
canonical PyrU = map toPoint [(0,0,0), (1,1,0), (2,1,0), (2,0,0)] -- 24 : 4 for each of 6 edges
canonical PyrZ = map toPoint [(0,0,0), (1,0,0), (2,1,0), (3,1,0)] -- 24 : 4 for each of 6 edges
canonical PyrL = map toPoint [(1,1,0), (1,1,1), (2,1,1), (3,1,1)] -- 24 : 4 for each of 6 edges

-- generate all orientations from canonical
orientations :: Shape -> [[Point]]
orientations PyrI =
    let plane = take 3 $ iterate (map rotateZ) $ map toPoint [(0,0,0),(0,1,1),(0,2,2)]
        slant = take 3 $ iterate (map rotateZ) (canonical PyrI)
    in plane ++ slant

translate :: Point -> [Point] -> [Point]
translate _ [] = []
translate (Point a b c) ((Point i j k):ps) =
    (Point a b c):[Point (e-i+a) (f-j+b) (g-k+c) | (Point e f g) <- ps]
                    
rotateZ :: Point -> Point
rotateZ (Point i j k) = Point i (i-k) (j-k)
                        
emptyPyramid :: Int -> Pyramid
emptyPyramid size = Pyramid size $ Map.fromList []

add :: Pyramid -> [Point] -> Char -> Pyramid
add (Pyramid size mp) points c = Pyramid size $
                                foldl (\m p -> Map.insert p c m) mp points

displayPoint :: Point -> Pyramid -> String
displayPoint point (Pyramid size mp) = case Map.lookup point mp of
                                         Just c -> c:""
                                         Nothing -> "."

displayLayer :: Int -> Pyramid -> String
displayLayer iLayer pyr@(Pyramid size _) =
    let blank = replicate (2*size) ' '
        prepend = (:) '|' . (:) ' '
        padding = replicate (size-iLayer-1) blank
        payload = [padCenter (length blank)
                   (unwords [displayPoint (Point iLayer j k) pyr
                             | k<-[0..j]])
                   | j<-[0..iLayer]
                  ]
    in unlines $ map prepend $ payload ++ padding
                                                     
display :: Pyramid -> String
display pyr@(Pyramid size _) =
    let layers = [lines (displayLayer i pyr) | i<-[0..(size-1)]]
    in unlines $ map unwords $ List.transpose layers

        

