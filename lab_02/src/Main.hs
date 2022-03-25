module Main where

import CoorsTable (parseDataTable)
import Interpol (multVarInterpol)

main :: IO()
main = do
    file = openFile "data.txt" ReadMode
    text = hGetContents file

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

    let coors = [x, y, z]
    let degrees = [nx, ny, nz]

    putStr "Результат: "
    print (multVarInterpol table coors degrees)

