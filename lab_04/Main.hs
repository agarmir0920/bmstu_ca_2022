module Main where

import System.IO

main :: IO()
main = do
    mainLoop []

-- mainLoop :: NodesTable -> IO()
mainLoop :: [Double] -> IO()
mainLoop table = do
    putStrLn "\nМеню:\n"

    putStrLn "1. Загрузить таблицу из файла"
    putStrLn "2. Добавить апроксимирующий полином"
    putStrLn "3. Очистить список апроксимирующих полиномов"
    putStrLn "4. Вывести полную таблицу"
    putStrLn "5. Вывести результат\n"

    putStrLn "0. Выход\n\n"

    putStr "Введите команду: "

    cmdStr <- getLine
    let cmd = read cmdStr::Int

    mainLoop table
