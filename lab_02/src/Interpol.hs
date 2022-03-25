module Interpol (interpolation) where

import CoorsTable (Var, Coors, CoorsTable)

type Degree = Int
type Degrees = [Degrees]

type VarsList = [Var]
type VarsLists = [VarsList]

type CoefsList = [Double]

type ValuesList = [Double]

-- Создание списка разделенных разностей функции
getVarsDiffsLst :: VarsLists -> VarsLists
getVarsDiffsLst [] = []
getVarsDiffsLst varsLsts = diff : getVarsDiffsLst perfVarsLsts
    where diff = (y1 - y2) / (x1 - x2)
          y1 = head ys
          y2 = ys !! 1
          x1 = head xs
          x2 = xs !! 1
          perfVarsLsts = [xs, tail ys]
          xs = head varsLsts
          ys = last varsLsts

-- Поиск коэффициентов полинома Ньютона
getNewtonsPolCoefs :: VarsLists -> Var -> Degree -> CoefsList
getNewtonsPolCoefs varsLsts var degree
    | degree < 0 = []
    | length ys == lenght xs = (getNewtonsPolCoefs varsDiffsLsts var degree - 1) : y0
    | otherwise = (getNewtonsPolCoefs varsDiffsLsts var degree - 1) : coef
    where
    coef = head ys
    ys = last varsLsts
    xs = head varsLsts
    y0 = if length ys `mod` 2 == 0 then ys !! (ys `div` 2) else ynear
    ynear = if abs (yl - y0) < abs (yr - y0) then yl else yr
    yl = ys !! (length y `div` 2)
    yr = ys !! (length y `div` 2 + 1)
    varsDiffsLsts = getVarsDiffsLst varsLsts

-- Значение полинома Ньютона при данном значении
getNewtonsPolValue :: CoefsList -> VarsList -> Var -> Double
getNewtonsPolValue coefs args arg degree
    | length coefs == 1 = curCoef
    | otherwise = curCoef * mult args arg + getNewtonsPolValue (tail coefs, init args, arg)
    where
    curCoef = head coefs
    mult [] _ = 1
    mult args arg = (arg - argi) * mult (tail args, arg)
    argi = head args

-- Интерполяция с помощью полинома Ньютона
newtonsInterpol :: VarsLists -> Var -> Degree -> Double
newtonsInterpol [] _ _ = 0.0
newtonsInterpol varsLsts var degree = getNewtonsPolValue coefs xs var
    where
    coefs = getNewtonsPolCoefs varsLsts var degree
    xs = head varsLsts

-- Значение функции при данных значениях переменных
getFuncValue :: CoorsTable -> Coors -> Double
getFuncValue [] [_] = 0.0
getFuncValue table coors
    | coors == curCoors = last curCoors
    | otherwise = getFuncValue (tail table, coors)

-- Получение списка значений интерполяций
getInterValuesLst :: CoorsTable -> VarsLsts -> Coors -> Degrees -> Coors -> ValuesList
getInterValuesLst table (varLst:othVarsLsts) (var:othVars) (degree:othDegrees) curCoors
    | null othVarsLsts = map interpolateValues varLst
    | otherwise = map interpolateInterpolated varLst
    where
    interpolateValues = \v -> newtonsInterpol [varLst, valuesLst] var degree
    interpolateInterpolated = \v -> newtonsInterpol [varLst, getInterpolLst v] var degree
    valuesLst = map getValue varLst
    getValue = \v -> getFuncValue (table, curCoors : v)
    getInterpolLst = \v -> getInterValuesLst (table, othVarsLsts, othVars, othDegrees, curCoors : v)

-- Интерполяция функции от двух пременных
twoVarsInterpol :: CoorsTable -> VarsLists -> Coors -> Degrees -> Double
twoVarsInterpol table (varLst:othVarsLsts) (var:othVars) (degree:othDegrees) = interpolResult
    where
    interpolResult = newtonsInterpol (interpolTable, var, degree)
    interpolTable = [varLsts, valuesLst]
    valuesLst = if length othVarsLsts == 1 then interValuesLst else othVarsLsts
    interValuesLst = getInterValuesLst table othVarsLsts othVars othDegrees (var)

getSymmLst :: VarsList -> Var -> Degree -> VarsList
getSymmLst vars var degree
    | length vars == degree + 1 = vars
    | length vars <= degree = []
    | vars !! mid < var = getSymmLst (tail vars, var, degree)
    | otherwise = getSymmLst (init vars, var, degree)
    where mid = length vars / 2

-- Поиск симметричных узлов
getSymmVarsLsts :: CoorsTable -> Coors -> Degrees -> VarsLists
getSymmVarsLsts table (coor:coors) (degree:degrees)
    | length (head table) == 1 = []
    | otherwise = symmLst ++ othSymmLsts
    where
    symmLst = getSymmLst fstVarLst var degree
    othSymmLsts = getSymmVarsLsts tableWithoutFstVar coors degrees
    fstVarLst = sort (nub (map head table))
    tableWithoutFstVar = map tail table

-- Многомерная интерполяция
multVarInterpol :: CoorsTable -> Coors -> Degrees -> Double
multVarInterpol [] [_] [_] = 0.0
multVarInterpol [_] [] [_] = 0.0
multVarInterpol [_] [_] [] = 0.0
multVarInterpol table coors degrees
    | length coors /= length degrees = 0.0
    | length table < 2 = 0.0
    | otherwise = twoVarsInterpol table symmVarsLsts coors degrees
    where
    symmVarsLsts = getSymmVarsLsts table coors degrees

