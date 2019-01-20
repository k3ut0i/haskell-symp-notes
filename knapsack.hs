module Main where
import Backtracking
import Data.Foldable(fold)

knapsack :: Int -> [Int] -> Backtr [Int]
knapsack w vs | w < 0 = Fail
              | w == 0 = return []
              | w > 0 = do v <- select vs
                           vs' <- knapsack (w - v) vs
                           return (v : vs')

-- Backtr monoid instance
select :: [a] -> Backtr a
select = fold . map Return

-- Backtr foldable instance
allsols :: Backtr a -> [a]
allsols = foldr (:) []

-- Does not allow repeats
knapsack10 :: Int -> [Int] -> Backtr [Int]
knapsack10 w vs | w < 0 = Fail
                | w == 0 = return []
                | w > 0 = do v <- select vs
                             vs' <- knapsack (w - v) $ filter (v /=) vs
                             return (v : vs')

-- FIXME: generalized 0-1 knapsack problem
{-
knapsack10G :: Int -> [(Int, Int)] -> Backtr [Int]
knapsack10G w vs | w < 0 = Fail
                 | w >= 0 && w < minW = return []
                 | w > minW =
                   do v <- select vs
                      vs' <- knapsack (w - snd v) $ filter (v /=) vs
                      return (v : vs')
  where minW = foldr1 min $ map snd vs
-}
-- Unbounded knapsack problem

main :: IO()
main = print $ allsols $ (knapsack 3 [3,2,1])
