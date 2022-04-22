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
    xstr <- getLine

    let x = read xstr::Double

    let splineRes = splineInterpolation table x 0 0

    putStr "Интерполяция сплайном: "
    print splineRes

    let newtonsRes = multVarInterpol table [x] [3]

    putStr "Интерполяция полиномом Ньютона: "
    if newtonsRes == newtonsRes then print newtonsRes else putStrLn "-"

