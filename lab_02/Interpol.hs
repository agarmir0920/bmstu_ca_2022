module Interpol (multVarInterpol) where

import Debug.Trace

import Data.List
import Data.Maybe
import CoorsTable

type Degree = Int
type Degrees = [Degree]

type VarsList = [Var]
type VarsLists = [VarsList]

type CoefsList = [Double]

type DiffsList = [Double]

type ValuesList = [Double]

getNewtonsPolValue :: CoefsList -> VarsList -> Degree -> Var -> Double
getNewtonsPolValue coefs args degree var
    | degree < 0 = 0.0
    | otherwise = m + getNewtonsPolValue (tail coefs) args (degree - 1) var
    where
    m = (head coefs) * (getArgsMult var degree args)
    getArgsMult _ 0 [_] = 1.0
    getArgsMult var degree args = (var - (head args)) * getArgsMult var (degree - 1) (tail args)

getSpDiffLst :: VarsLists -> DiffsList
getSpDiffLst varsLsts
    | length ys <= 1 = ys
    | otherwise = diff : getSpDiffLst [tail xs, tail ys]
    where
    xs = head varsLsts
    ys = last varsLsts
    diff = (y2 - y1) / (x2 - x1)
    x1 = xs !! 0
    x2 = xs !! 0
    y1 = ys !! 0
    y2 = ys !! 1

getNewtonsPolCoefs :: VarsLists -> Degree -> CoefsList
getNewtonsPolCoefs varsLsts degree
    | degree <= 0 = []
    | otherwise = getNewtonsPolCoefs perfVarsLsts (degree - 1) ++ [coef]
    where
    coef = head diffLst
    xs = head varsLsts
    perfVarsLsts = [xs, diffLst]
    diffLst = getSpDiffLst varsLsts

newtonsInterpol :: VarsLists -> Var -> Degree -> Double
newtonsInterpol varsLsts var degree = getNewtonsPolValue coefs args degree var
    where
    ys = last varsLsts
    coefs = (getNewtonsPolCoefs varsLsts degree) ++ [ys !! nearArgInd]
    args = head varsLsts

    nearArgInd = fromMaybe 0 (elemIndex minArgDiff argDiffsLst)
    minArgDiff = minimum argDiffsLst
    argDiffsLst = map (\x -> abs (x - var)) args

getInterpolValuesLst :: CoorsTable -> VarsLists -> Coors -> Degrees -> Coors -> VarsList
getInterpolValuesLst coorsTable varsLsts coors degrees curCoors
    | length varsLsts == 1 = map valuesInterpolFunc fstVarLst
    | otherwise = map recInterpolFunc fstVarLst
    where
    fstVarLst = head varsLsts
    coor = head coors
    degree = head degrees
    othCoors = tail coors
    othDegrees = tail degrees
    othVarsLsts = tail varsLsts

    valuesInterpolFunc = \v -> newtonsInterpol [fstVarLst, getValuesLst v] coor degree
    getValuesLst = \v -> map last (filter (\cs -> (init cs) == (curCoors ++ [v])) coorsTable)

    recInterpolFunc = \v -> newtonsInterpol [fstVarLst, getOthInterpolValuesLst v] coor degree
    getOthInterpolValuesLst v = getInterpolValuesLst coorsTable othVarsLsts othCoors othDegrees (curCoors ++ [v])

twoVarsInterpol :: CoorsTable -> VarsLists -> Coors -> Degrees -> Double
twoVarsInterpol coorsTable varsLsts coors degrees
    | (length . head) coorsTable == 2 = newtonsInterpol coorsTable coor degree
    | otherwise = newtonsInterpol valuesInterpolTable coor degree
    where
    coor = head coors
    degree = head degrees
    othCoors = tail coors
    othDegrees = tail degrees
    fstVarsLst = head varsLsts
    valuesInterpolTable = [fstVarsLst, getInterpolValuesLst coorsTable (tail varsLsts) othCoors othDegrees [coor]]

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
multVarInterpol [] [_] [_] = 0.0
multVarInterpol [_] [] [_] = 0.0
multVarInterpol [_] [_] [] = 0.0
multVarInterpol coorsTable coors degrees
    | length coors /= length degrees = 0.0
    | length coorsTable < 2 = 0.0
    | length symmVarsLsts /= length coors = 0.0
    | otherwise = twoVarsInterpol coorsTable symmVarsLsts coors degrees
    where
    symmVarsLsts = getSymmVarsLsts tableWithoutValues coors degrees
    tableWithoutValues = map init coorsTable

