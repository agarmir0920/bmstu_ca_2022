module DataTable
    ( FuncTable
    , Filename
    , printTable
    , getTableFromFile)
    where

import System.IO

type FuncTable = [[Double]]
type Filename = String

printArr :: [Double] -> [Double]
printArr [] = do
    putStr "\n"
    return []
printArr arr = do
    putStr $ show (head arr)
    putStr " "
    return $ tail arr

printTable :: FuncTable -> FuncTable
printTable [] = []
printTable funcTable = do
    printArr $ head funcTable
    return $ tail funcTable

getTableFromFile :: Filename -> FuncTable
getTableFromFile fileName =
    map toCoors (lines text)
    where
    text = hGetContents fileName
    toCoors = \str -> map toDouble (words str)
    toDouble = \str -> read str :: Double

