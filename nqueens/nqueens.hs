--
-- NQueens in Haskell using the list monad for backtracking search
-- 
-- Andrei de A. Formiga - 2014-02-17
--

type Solution = [(Int, Int)]

-- determine if queen in position (x1, y1) attacks queen at (x2, y2)
-- assumes that x1 /= x2
attacks :: (Int, Int) -> (Int, Int) -> Bool
attacks (x1, y1) (x2, y2) = (y1 == y2) || (abs (x1 - x2) == abs (y1 - y2))

-- check to see if a (partial) solution is valid
checkSolution :: Solution -> Bool 
checkSolution []      = True
checkSolution (q:qs)  = not $ any (attacks q) qs && checkSolution qs

buildSolution :: Int -> Int -> [Solution] -> [Solution]
buildSolution n i pss 
    | i > n     = pss
    | otherwise = buildSolution n (i+1) newpss
        where newpss = do
                ps <- pss
                row <- [1..n]
                let newps = (i, row) : ps 
                if checkSolution newps then return newps else fail "not a solution" -- []

nQueens :: Int -> [Solution]
nQueens n = buildSolution n 1 [[]]
