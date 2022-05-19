module SLE (getRootsVector) where

import Data.Matrix

import DataTable

type Roots = [Double]

getCramersRoots :: Matrix Double -> Double -> Int -> Roots
getCramersRoots mtrx det ind
    | ind >= (ncols cols - 1) = []
    | otherwise = [curDet / det] ++ getCramerRoots mtrx det (ind + 1)
    where
    curDet = detLU mtrxWithBs
    mtrxWithBs = leftPart <|> Bs <|> rightPart
    leftPart = submatrix 1 rows 1 (ind + 1) mtrx
    rightPart = submatrix 1 rows (ind + 3) (cols - 1) mtrx
    Bs = colVector $ getCol cols mtrx

    cols = ncols mtrx
    rows = nrows mtrx

getRoots :: Matrix Double -> Roots
getRoots mtrx
    | det == 0 = []
    | otherwise = getCramersRoots mtrx det 0
    where
    det = detLU $ submatrix 1 rows 1 (cols - 1) mtrx
    rows = nrows mtrx
    cols = ncols mtrx

