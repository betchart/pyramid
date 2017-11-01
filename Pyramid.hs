module Pyramid where

import qualified Data.Map as Map
import qualified Data.Set as Set
import PPoint
import Shape
    
data Pyramid = Pyramid Int (Map.Map Point Char) deriving (Show)

emptyPyramid :: Int -> Pyramid
emptyPyramid size = Pyramid size $ Map.fromList []

insert :: Pyramid -> Char -> [Point]  -> Pyramid
insert (Pyramid size mp) c points =
    Pyramid size $ foldl (\m p -> Map.insert p c m) mp points

pieces = zip "1234" (repeat PyrL) ++
         [('I',PyrI), ('U',PyrU), ('J',PyrJ), ('P',PyrP), ('Z',PyrZ)]

pyrSolutions :: Pyramid -> [(Char, Shape)] -> [Pyramid]
pyrSolutions pyr [] = [pyr]
pyrSolutions pyr ((c, shape):xs) =
    case null (isoPoints pyr) of
      False -> []
      otherwise -> concat [pyrSolutions (insert pyr c ps) xs
                           | ps <- positions pyr shape]

positions :: Pyramid -> Shape -> [[Point]]
positions pyr shape =
    let open = Set.fromList $ openPoints pyr
    in [points | p <- Set.toList open,
                 o <- orientations shape,
                 let points = translate p o,
                 all (flip Set.member open) points ]

openPoints :: Pyramid -> [Point]
openPoints (Pyramid size m) = [point | i<-[0..(size-1)],
                                       j<-[0..i],
                                       k<-[0..j],
                                       let point = Point i j k,
                                       Map.notMember point m ]

isoPoints :: Pyramid -> [Point]
isoPoints pyr = let open = Set.fromList $ openPoints pyr
                in [o | o <- Set.toList open,
                        not . any (flip elem open) $ pyrNeighbors o]
