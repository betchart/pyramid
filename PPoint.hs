module PPoint where

data Point = Point Int Int Int deriving (Show, Eq, Ord)

toPoint :: (Int, Int, Int) -> Point
toPoint (i, j, k) = Point i j k

translate :: Point -> [Point] -> [Point]
translate _ [] = []
translate (Point a b c) ((Point i j k):ps) =
    (Point a b c):[Point (e-i+a) (f-j+b) (g-k+c) | (Point e f g) <- ps]

displace :: Point -> Point -> Point
displace (Point i j k) (Point a b c) = Point (i+a) (j+b) (k+c)

rotateZ :: Point -> Point -- by 2pi/3
rotateZ (Point i j k) = Point i (i-k) (j-k)

rotateW :: Point -> Point -- by 2pi/3
rotateW (Point i j k) = Point (-k) (i-k) (i-j)

reflectY :: Point -> Point
reflectY (Point i j k) = Point i j (j-k)

rotateXpi :: Point -> Point -- by pi
rotateXpi (Point i j k) = Point (-i) (-j) (k-j)

pyrNeighbors :: Point -> [Point]
pyrNeighbors point@(Point i j k)
    | (i,j,k) == (0,0,0) = below
    | (j,k) == (0,0) = mdis $ concat [below, south, take 1 above]
    | (j,k) == (i,0) = mdis $ concat [below, ene, drop 2 above]
    | (j,k) == (i,i) = mdis $ concat [below, wnw, take 1 $ drop 1 above]
    | otherwise = mdis todos
    where mdis = map (displace point)
          below = take 3 $ iterate rotateZ (Point 1 0 0)
          above = map rotateXpi below
          south = take 2 $ iterate reflectY (Point 0 1 0)
          ene = map rotateZ south
          wnw = map rotateZ ene
          todos = concat [above, below, south, ene, wnw]

