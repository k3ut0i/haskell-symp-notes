module Main where
import Backtracking
import Data.Foldable (fold)

partialBoard :: Int -> Int -> Backtr [Int]
partialBoard size nrows
  | nrows == 1 =  fmap (:[]) $ fold (map Return [1 .. size])
  | nrows >  1 = do
      colNums <- partialBoard size (nrows - 1)
      let diagonalCols = foldr (\(x, y) acc -> [x+y, x-y] ++ acc) [] $ zip colNums [1 .. (nrows - 1)]
      let nextColNums =
            filter (\x ->  (notElem x colNums) &&
                           (notElem x diagonalCols))
            [1 .. size]
      fold (map (\x -> Return (x : colNums)) nextColNums)
  
main :: IO()
main = let eightsols = partialBoard 8 8
       in print $ foldr (:) [] eightsols
