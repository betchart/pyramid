module Display where

import qualified Data.Map as Map
import qualified Data.List as List
import Pyramid
import PPoint
import Shape
import StrUtils
    
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
