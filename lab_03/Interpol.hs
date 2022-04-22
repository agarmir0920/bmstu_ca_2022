module Interpol (multVarInterpol) where

import Debug.Trace

import Data.List
import Data.Maybe
import Table

type Degree = Int
type Degrees = [Degree]

type VarsList = [Var]
type VarsLists = [VarsList]

type CoefsList = [Double]

type DiffsList = [Double]

type ValuesList = [Double]

-- Вычисление значения полинома Ньютона
getNewtonsPolValue :: CoefsList -> VarsList -> Degree -> Var -> Double
getNewtonsPolValue coefs args degree var
    | degree <= 0 = last coefs
    | otherwise = m + getNewtonsPolValue (tail coefs) args (degree - 1) var
    where
    m = (head coefs) * (getArgsMult var degree args)

    getArgsMult _ 0 [_] = 1.0
    getArgsMult _ _ [] = 1.0
    getArgsMult var degree args = (var - (head args)) * getArgsMult var (degree - 1) (tail args)

-- Расчет списка разделенных разностей
getSpDiffLst :: VarsLists -> DiffsList
getSpDiffLst varsLsts
    | length ys <= 1 = ys
    | otherwise = diff : (getSpDiffLst $ map tail varsLsts)
    where
    xs = head varsLsts
    ys = last varsLsts
    diff = (y2 - y1) / (x2 - x1)
    x1 = xs !! 0
    x2 = xs !! 1
    y1 = ys !! 0
    y2 = ys !! 1

-- Расчет коэффициентов полинома Ньютона
getNewtonsPolCoefs :: VarsLists -> Degree -> CoefsList
getNewtonsPolCoefs varsLsts degree
    | degree <= 0 = []
    | otherwise = (getNewtonsPolCoefs perfVarsLsts (degree - 1)) ++ [coef]
    where
    perfVarsLsts = [xs, diffLst]
    xs = head varsLsts
    coef = head diffLst
    diffLst = getSpDiffLst varsLsts

-- Интерполяция полиномом Ньютона
newtonsInterpol :: VarsLists -> Var -> Degree -> Double
newtonsInterpol varsLsts var degree = getNewtonsPolValue coefs args degree var
    where
    ys = last varsLsts
    coefs = (getNewtonsPolCoefs varsLsts degree) ++ [ys !! nearArgInd]
    args = head varsLsts

    nearArgInd = fromMaybe 0 (elemIndex minArgDiff argDiffsLst)
    minArgDiff = minimum argDiffsLst
    argDiffsLst = map (\x -> abs (x - var)) args

-- Поиск значений по заданным переменным
getInterpolValuesLst :: CoorsTable -> VarsLists -> Coors -> Degrees -> Coors -> VarsList
getInterpolValuesLst coorsTable varsLsts coors degrees curCoors
    | length varsLsts == 2 = map lastVarInterpolFunc fstVarLst
    | otherwise = map varInterpolFunc fstVarLst
    where
    fstVarLst = head varsLsts
    sndVarLst = varsLsts !! 1
    coor = head coors
    degree = head degrees
    othVarsLsts = tail varsLsts
    othCoors = tail coors
    othDegrees = tail degrees

    varInterpolFunc v = newtonsInterpol [sndVarLst, getOthInterpolValues v] coor degree
    getOthInterpolValues v = getInterpolValuesLst coorsTable othVarsLsts othCoors othDegrees (curCoors ++ [v])

    lastVarInterpolFunc v = newtonsInterpol [sndVarLst, getValuesLst v] coor degree
    getValuesLst v = map (getValue v) sndVarLst
    getValue v w = last $ fromMaybe [] $ find (isPrefixOf (curCoors ++ [v, w])) coorsTable

-- Поиск симметричного списка значений переменной
getSymmLst :: VarsList -> Var -> Degree -> VarsList
getSymmLst varsLst var degree
    | varsLstLen < symmLstLen = []
    | varsLstLen == symmLstLen = if abs (mid - nearInd) > 1 then [] else varsLst
    | otherwise = if mid > nearInd then getSymmLst (init varsLst) var degree else getSymmLst (tail varsLst) var degree
    where
    symmLstLen = degree + 1
    varsLstLen = length varsLst
    mid = varsLstLen `div` 2

    nearInd = fromMaybe 0 (elemIndex minDiff diffsLst)
    minDiff = minimum diffsLst
    diffsLst = map (\x -> abs (x - var)) varsLst

-- Поиск симметричных узлов
getSymmVarsLsts :: CoorsTable -> Coors -> Degrees -> VarsLists
getSymmVarsLsts coorsTable coors degrees
    | null coors = []
    | null symmLst = []
    | otherwise = [symmLst] ++ othSymmLsts
    where
    symmLst = getSymmLst fstVarLst coor degree
    othSymmLsts = getSymmVarsLsts tableWithoutFst othCoors othDegrees
    fstVarLst = (sort . nub) $ map head coorsTable
    tableWithoutFst = map tail coorsTable

    coor = head coors
    othCoors = tail coors
    degree = head degrees
    othDegrees = tail degrees

-- Многомерная интерполяция
multVarInterpol :: CoorsTable -> Coors -> Degrees -> Double
multVarInterpol [] [_] [_] = 0 / 0
multVarInterpol [_] [] [_] = 0 / 0
multVarInterpol [_] [_] [] = 0 / 0
multVarInterpol coorsTable coors degrees
    | length coors /= length degrees = 0 / 0
    | length (head coorsTable) < 2 = 0 / 0
    | length symmVarsLsts /= length coors = 0 / 0
    | length (head coorsTable) == 2 = newtonsInterpol [fstVarsLst, valuesLst] coor degree
    | otherwise = newtonsInterpol [fstVarsLst, interpolValuesLst] coor degree
    where
    symmVarsLsts = getSymmVarsLsts tableWithoutValues coors degrees
    coor = head coors
    degree = head degrees
    othCoors = tail coors
    othDegrees = tail degrees
    tableWithoutValues = map init coorsTable
    fstVarsLst = head symmVarsLsts
    valuesLst = map last (filter (\c -> (head c) `elem` fstVarsLst) coorsTable)
    interpolValuesLst = getInterpolValuesLst coorsTable symmVarsLsts othCoors othDegrees []

