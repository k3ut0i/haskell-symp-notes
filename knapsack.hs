module Main where
import Backtracking

knapsack :: Int -> [Int] -> Backtr [Int]
knapsack w vs | w < 0 = Fail
              | w == 0 = return []
              | w > 0 = do v <- select vs
                           vs' <- knapsack (w - v) vs
                           return (v : vs')

select :: [a] -> Backtr a
select = foldr (:|) Fail . map Return

allsols :: Backtr a -> [a]
allsols (Return x) = [x]
allsols Fail = []
allsols (p :| q) = allsols p ++ allsols q

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
knapsack10G w vs | w < minW = Fail
                 | w == 0 = return []
                 | w > minW =
                   do v <- select vs
                      vs' <- knapsack (w - snd v) $ filter (v /=) vs
                      return (v : vs')
  where minW = foldr1 min $ map snd vs
-}
main :: IO()
main = print $ allsols $ (knapsack 3 [3,2,1])
