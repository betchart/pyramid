module Pyramid where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import StrUtils
    
data Pyramid = Pyramid Int (Map.Map Point Char) deriving (Show)
data Point = Point Int Int Int deriving (Show, Eq, Ord)
data Shape = PyrI | PyrJ | PyrL | PyrP | PyrU | PyrZ deriving (Eq,Show)

pieces = zip "1234" (repeat PyrL) ++
         [('I',PyrI), ('J',PyrJ),('P',PyrP),('Z',PyrZ), ('U',PyrU)]
         

pyrSolutions :: Pyramid -> [(Char, Shape)] -> [Pyramid]
pyrSolutions pyr [] = [pyr]
pyrSolutions (pyr@(Pyramid _ m)) ((c, shape):xs) =
    concat [pyrSolutions (add pyr c ps) xs
            | ps <- positions shape,
              all (flip Map.notMember m) ps
           ]
    
toPoint :: (Int, Int, Int) -> Point
toPoint (i, j, k) = Point i j k

canonical :: Shape -> [Point]
canonical shape = map toPoint $ which shape
    where which PyrI = [(0,0,0), (0,0,1), (0,0,2)] -- 6 : one for each edge of tetrahedron
          which PyrJ = [(0,0,0), (0,0,1), (0,0,2), (0,1,3)] -- 48 : 8 for each of 6 edges
          which PyrP = [(0,0,0), (0,0,1), (0,0,2), (0,1,2)] -- 48 : 8 for each of 6 edges
          which PyrU = [(0,0,0), (0,1,1), (0,1,2), (0,0,2)] -- 24 : 4 for each of 6 edges
          which PyrZ = [(0,0,0), (0,0,1), (0,1,2), (0,1,3)] -- 24 : 4 for each of 6 edges
          which PyrL = [(0,0,0), (0,0,1), (0,0,2), (-1,0,2)] -- 24 : 4 for each of 6 edges

positions :: Shape -> [[Point]]
positions shape =
    let pp5 = pyramidPoints 5
        everyPos = [translate p o | p <- Set.toList pp5, o <- orientations shape]
    in filter (all (flip Set.member pp5)) everyPos
       

orientations :: Shape -> [[Point]]
orientations shape
    | shape == PyrI =
        let two = take 2 $ iterate (map rotateW) $ canonical shape
        in concat $ map (take 3 . iterate (map rotateZ)) two
    | otherwise =
    let two = if (elem shape [PyrJ,PyrP])
              then take 2 $ iterate (map reflectY) $ canonical shape
              else [canonical shape]
        four = concat $ map (take 2 . iterate (map rotateXpi)) two
        bottom = concat $ map (take 3 . iterate (map rotateZ)) four
        sides = map (take 3 . iterate (map rotateZ)) $ map (map rotateW) $ bottom
    in concat (bottom:sides)

translate :: Point -> [Point] -> [Point]
translate _ [] = []
translate (Point a b c) ((Point i j k):ps) =
    (Point a b c):[Point (e-i+a) (f-j+b) (g-k+c) | (Point e f g) <- ps]
                    
rotateZ :: Point -> Point -- by 2pi/3
rotateZ (Point i j k) = Point i (i-k) (j-k)

rotateW :: Point -> Point -- by 2pi/3
rotateW (Point i j k) = Point (-k) (i-k) (i-j)

reflectY :: Point -> Point
reflectY (Point i j k) = Point i j (j-k)

rotateXpi :: Point -> Point -- by pi
rotateXpi (Point i j k) = Point (-i) (-j) (k-j)
       


emptyPyramid :: Int -> Pyramid
emptyPyramid size = Pyramid size $ Map.fromList []

add :: Pyramid -> Char -> [Point]  -> Pyramid
add (Pyramid size mp) c points = Pyramid size $
                                 foldl (\m p -> Map.insert p c m) mp points

pyramidPoints :: Int -> (Set.Set Point)
pyramidPoints size = Set.fromList [Point i j k
                                   | i<-[0..(size-1)], j<-[0..i], k<-[0..j]]

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
