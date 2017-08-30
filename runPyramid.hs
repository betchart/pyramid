import Pyramid
import Display

main = do
  putStrLn $ display $ head $ pyrSolutions (emptyPyramid 5) pieces
     
