module Main where
import Backtracking
import Data.Foldable (fold)

{-
In this implementation I'm not doing any backtracking.
I'm Just building partial solutions.
-}
partialBoard' :: Int -> Int -> Backtr [Int]
partialBoard' size nrows
  | nrows == 1 =  fmap (:[]) $ fold (map Return [1 .. size])
  | nrows >  1 = do
      colNums <- partialBoard' size (nrows - 1)
      let diagonalCols = foldr (\(x, y) acc -> [x+y, x-y] ++ acc) [] $ zip colNums [1 .. (nrows - 1)]
      let nextColNums =
            filter (\x ->  (notElem x colNums) &&
                           (notElem x diagonalCols))
            [1 .. size]
      fold (map (\x -> Return (x : colNums)) nextColNums)

-- Backtracking Implementation
partialBoard :: Int -> Int -> Backtr [Int]
partialBoard s n | n == 1 = fmap (:[]) . fold $ map Return [1 .. s]
                  | n > 1 = do
                      colNums <- partialBoard s (n - 1)
                      fold (map (\x -> validate x colNums) [1 .. s])
  where
    validate :: Int -> [Int] -> Backtr [Int]
    validate n p = if elem n (attackPos p)
                   then Fail
                   else Return (n : p)
    attackPos :: [Int] -> [Int]
    attackPos cols = foldr (\(x, y) acc -> [x+y, x, x-y] ++ acc) []
                     (zip cols [1 .. (length cols)])
                      

main :: IO()
main = let eightsols = partialBoard 8 8
       in print $ foldr (:) [] eightsols
