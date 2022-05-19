module SLE (getRootsVector) where

import Data.Matrix

import DataTable

type Roots = [Double]

getCramersRoots :: Matrix Double -> Double -> Int -> Roots
getCramersRoots mtrx det ind
    | ind >= ncols cols = []
    | otherwise = [curDet / det] ++ getCramerRoots mtrx det (ind + 1)
    where
    curDet = detLU mtrxWithBs
    mtrxWithBs = leftPart <|> Bs <|> rightPart
    leftPart = submatrix 1 (ind + 1) 1 rows
    rightPart = submatrix (ind + 3) 1 rows
    Bs = colVector (getCol ind mtrx)

    cols = ncols mtrx
    rows = nrows mtrx

getRoots :: Matrix Double -> Roots
getRoots mtrx
    | det == 0 = []
    | otherwise = getCramersRoots mtrx det 0
    where
    det = detLU mtrx

