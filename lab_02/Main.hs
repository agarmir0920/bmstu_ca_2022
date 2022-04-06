module Main where

import System.IO
import CoorsTable
import Interpol

main :: IO()
main = do
    file <- openFile "data.txt" ReadMode
    text <- hGetContents file

    let table = parseDataTable text

    putStrLn "Введите x: "
    x <- getLine
    putStrLn "Введите y: "
    y <- getLine
    putStrLn "Введите z: "
    z <- getLine

    putStrLn "Введите nx: "
    nx <- getLine
    putStrLn "Введите ny: "
    ny <- getLine
    putStrLn "Введите nz: "
    nz <- getLine

    let coors = map (read :: String -> Double) [x, y, z]
    let degrees = map (read :: String -> Int) [nx, ny, nz]

    let result = multVarInterpol table coors degrees

    putStr "Результат: "
    if result == result then print result else putStr "Невозможно выполнить интерполяцию\n"

