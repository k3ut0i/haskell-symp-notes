module Main where
import Backtracking
import Data.Foldable (fold)

partialBoard :: Int -> Int -> Backtr [Int]
partialBoard size nrows
  | nrows == 1 =  fmap (:[]) $ fold (map Return [1 .. size])
  | nrows >  1 = do
      colNums <- partialBoard size (nrows - 1)
      let verticalFilter = \x -> notElem x colNums
      let diagonalFilter = undefined
      let nextColNums =
            filter (\x -> notElem x colNums) [1 .. size]
      -- Vertical filtering done
      -- TODO: Diagonal filtering remaining
      fold (map (\x -> Return (x : colNums)) nextColNums)
  
main :: IO()
main = let eightsols = partialBoard 8 8
       in print . length $ foldr (:) [] eightsols
