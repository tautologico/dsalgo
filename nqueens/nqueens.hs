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

failure :: [a]
failure = fail "not a solution"  -- failure == []

buildSolution :: Int -> Int -> [Solution] -> [Solution]
buildSolution n i pss 
    | i > n     = pss
    | otherwise = buildSolution n (i+1) newpss
        where newpss = do
                ps <- pss
                row <- [1..n]
                let newps = (i, row) : ps 
                if checkSolution newps then return newps else failure

nQueens :: Int -> [Solution]
nQueens n = buildSolution n 1 [[]]

queens4Sols :: [Solution]
queens4Sols = do
    row1 <- [1..4]
    let s1 = (1, row1) : []
    if not $ checkSolution s1 then failure
        else do
            row2 <- [1..4]
            let s2 = (2, row2) : s1
            if not $ checkSolution s2 then failure
                else do
                    row3 <- [1..4]
                    let s3 = (3, row3) : s2
                    if not $ checkSolution s3 then failure
                        else do
                            row4 <- [1..4]
                            let s4 = (4, row4) : s3
                            if not $ checkSolution s4 then failure
                                else return s4

lazyBuild :: Int -> Int -> Solution -> [Solution]
lazyBuild n i sol 
    | i > n     = return sol
    | otherwise = do
        row <- [1..n]
        let isol = (i, row) : sol
        if not $ checkSolution isol then failure 
            else lazyBuild n (i+1) isol

lazyNQueens :: Int -> [Solution]
lazyNQueens n = lazyBuild n 1 []

