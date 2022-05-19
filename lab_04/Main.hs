module Main where

import System.IO
import System.Directory

type Command = Int
type Polynom = [Polynoms]

data MainData = MainData {
    funcTable :: FuncTable,
    polynoms :: Polynoms
}

executeLoad :: MainData -> MainData
executeLoad data = do
    putStr "\nВведите имя файла: "
    
    fileName <- getLine

    if not (doesFileExist fileName)
    then
        putStrLn "Файл не существует"
    else
        return $ MainData (getTableFromFile fileName) []

    return data

executeOneWeightSet :: MainData -> MainData
executeOneWeightSet data = do
    putStr "Введите значение весов: "

    pStr <- getLine
    let p = read pStr::Double

    return $ MainData newTable (polynoms data)

    where
    newTable = (init (funcTable data)) ++ (take (length ((funcTable data) !! 0)) (repeat p))

executeAllWeightSet :: MainData -> MainData
executeAllWeightSet data = do
    putStr "Введите значения весов через пробел: "

    psStr <- getLine
    let ps = map (\pStr -> read pStr::Double) (words psStr)

    return $ MainData newTable (polynoms data)

    where
    newTable = (init (funcTable data)) ++ ps

executeWeightsChanging :: MainData -> MainData
executeWeightsChanging data = do
    putStr "\nХотите ввести одно значение для всех узлов? (y\n): "

    per <- getLine

    if per == 'y'
    then
        executeOneWeightSet mainData
    else
        executeAllWeightSet mainData

executePolynomAdding :: MainData -> MainData
executePolynomAdding data = MainData table newPolynoms
    where
    newPolynoms = (polynoms data) ++ (getAproxPolynom table)
    table = funcTable data

executePolynomClearing :: MainData -> MainData
executePolynomClearing data = MainData (funcTable data) []

executeTablePrinting :: MainData -> MainData
executeTablePrinting data = do
    printTable $ funcTable data
    return data

executeGraphPrinting :: MainData -> MainData
executeGraphPrinting data = do
    return data

executeCommand :: MainData -> Command -> MainData
executeCommand data cmd
    | cmd == 1 = executeLoad mainData
    | cmd == 2 = executeWeightsChanging mainData
    | cmd == 3 = executePolynomAdding mainData
    | cmd == 4 = executePolynomsClearing mainData
    | cmd == 5 = executeTablePrinting mainData
    | cmd == 6 = executeGraphPrinting mainData
    | otherwise = mainData

main :: IO()
main = do
    mainLoop (MainData [] [])

mainLoop :: MainData -> IO()
mainLoop data = do
    putStrLn "\nМеню:\n"

    putStrLn "1. Загрузить таблицу из файла"
    putStrLn "2. Изменить веса"
    putStrLn "3. Добавить апроксимирующий полином"
    putStrLn "4. Очистить список апроксимирующих полиномов"
    putStrLn "5. Вывести полную таблицу"
    putStrLn "6. Вывести результат\n"

    putStrLn "0. Выход\n\n"

    putStr "Введите команду: "

    cmdStr <- getLine
    let cmd = read cmdStr::Int

    if (cmd == 0)
    then
        return 0
    else
        mainLoop $ executeCommand data cmd

