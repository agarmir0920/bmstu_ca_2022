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

    putStr "Интерполяция сплайном:\n\n"

    putStr "φ``(x0) = 0; φ``(xn) = 0:\t\t"
    print $ splineInterpolation table x 0 0

    let left = get3DPolSndDiffValue table x ((head . head) table) 3
    let right = get3DPolSndDiffValue table x ((last . head) table) 3

    putStr "φ``(x0) = P3``(x0); φ``(xn) = 0:\t"
    if left == left then print $ splineInterpolation table x left 0 else putStrLn "-"

    putStr "φ``(x0) = P3``(x0); φ``(xn) = P3``(xn):\t"
    if right == right then print $ splineInterpolation table x left right else putStrLn "-"

    let newtonsRes = multVarInterpol table [x] [3]

    putStr "\nИнтерполяция полиномом Ньютона:\t\t"
    if newtonsRes == newtonsRes then print newtonsRes else putStrLn "-"

