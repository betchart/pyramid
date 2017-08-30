module Shape where

import PPoint

data Shape = PyrI | PyrJ | PyrL | PyrP | PyrU | PyrZ deriving (Eq,Show)

canonical :: Shape -> [Point]
canonical shape = map toPoint $ which shape
    where which PyrI = [(0,0,0), (0,0,1), (0,0,2)] -- 6 : one for each edge of tetrahedron
          which PyrJ = [(0,0,0), (0,0,1), (0,0,2), (0,1,3)] -- 48 : 8 for each of 6 edges
          which PyrP = [(0,0,0), (0,0,1), (0,0,2), (0,1,2)] -- 48 : 8 for each of 6 edges
          which PyrU = [(0,0,0), (0,1,1), (0,1,2), (0,0,2)] -- 24 : 4 for each of 6 edges
          which PyrZ = [(0,0,0), (0,0,1), (0,1,2), (0,1,3)] -- 24 : 4 for each of 6 edges
          which PyrL = [(0,0,0), (0,0,1), (0,0,2), (-1,0,2)] -- 24 : 4 for each of 6 edges

orientations :: Shape -> [[Point]]
orientations shape
    | shape == PyrI = [canonical shape]
        --let two = take 2 $ iterate (map rotateW) $ canonical shape
        --in z3 two
    | otherwise =  face4 . z3 . rxpi $ init shape
    where z3 = concat . map (take 3 . iterate (map rotateZ))
          rxpi = concat . map (take 2 . iterate (map rotateXpi))
          face4 = (\base -> base ++ (z3 $ map (map rotateW) base))
          refY = take 2 . iterate (map reflectY)
          init PyrJ = refY $ canonical shape
          init PyrP = refY $ canonical shape
          init shape = [canonical shape]
