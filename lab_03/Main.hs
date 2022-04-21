module Main where

import System.IO
import Table
import Interpol
import Spline

main :: IO()
main = do
    file <- openFile "data.txt" ReadMode
    text <- hGetContents file

    let table = parseDataTable text

    putStrLn "Введите x: "
    x <- getLine

    let splineRes = splineInterpolation table x 0 0

    putStrLn "Интерполяция сплайном: "
    print splineRes

    let newtonsRes = multVarInterpolation table [x] [3]

    putStrLn "Интерполяция полиномом Ньютона: "
    if newtonsRes == newtonsRes then print newtonsRes else print "-"

