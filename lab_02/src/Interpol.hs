module Interpol (interpolation) where

import Table (Coor, Coors, CoorsTable)

type Degree = Int
type Degrees = [Degrees]

type CoorsList = [Coor]
type SymmCoorsLists = [CoorsList]

type Index = Int

getSymmCoorsList :: CoorsList -> Coor -> Degree -> CoorsList
getSymmCoorsList [] _ _ = []
getSymmCoorsList lst coor degree
    | degree < 0 = if len > 1 && diff > lstStep then [] else lst
    | midElem > coor = getSymm(init lst, coor, degree - 1)
    | otherwise = getSymm(tail lst, coor, degree - 1)
    where diff = abs $ midElem - coor
          len = length lst
          midElem = lst!!midInd
          midInd = div $ len 2

getSymmCoorsLists :: CoorsTable -> Coors -> Degrees -> SymmCoorsLists
getSymmCoorsLists [_] [] [_] = []
getSymmCoorsLists table coors degrees
    | null symmLst = []
    | otherwise = symmLst : nextLsts
    where symmLst = getSymmCoorsList $ coorsLst coors!!0 degrees!!0
          nextLsts = getSymmCoorsLists (table, tail coors, tail degrees)
          coorsLst = nub $ table!!ind
          ind = length (table - length (coors) - 1)

coorsAreEqual :: Coors -> Coors -> Bool
coorsAreEqual [] [] = True
coorsAreEqual coors1 coors2
    | len1 /= len2 = False
    | coors1!!0 /= coors2!!0 = False
    | otherwise = coorsAreEqual (tail coors1, tail coors2)
    where len1 = length coors1
          len2 = length coors2

getValue :: CoorsTable -> Coors -> Double
getValue [] [_] = 0.0
getValue table coors
    | coorsAreEqual $ curCoors coors = curCoorsValue
    | otherwise = getValue (tail (table), coors)
    where curCoors = init firstCoors
          curCoorsValue = last firstCoors
          firstCoors = table!!0

getFirstCoors :: SymmCoorsLists -> Coors
getFirstCoors [] = []
getFirstCoors lsts = coor : getFirstCoors $ tail lsts
    where coor = (lsts!!0)!!0

getLstsWithoutFstCoors :: SymmCoorsLists -> SymmCoorsLists
getLstsWithoutFstCoors [] = []
getLstsWithoutFstCoors lsts = (tail (lsts!!0)) : nextCoors
    where nextCoors = getLstsWithoutFstCoors $ tail lsts

getSymmValuesList :: CoorsTable -> SymmCoorsLists -> CoorsList
getSymmValuesList table lsts
    | null lsts!!0 = []
    | otherwise = curCoorsValue : nextValuesLst
    where curCoorsValue = getValue $ table curCoors
          curCoors = getFirstCoors lsts
          nextValuesLst = getSymmValuesList (table, newLsts)
          newLsts = getLstsWithoutFstCoors lsts

getSymmNodes :: CoorsTable -> Coors -> Degrees -> SymmCoorsLists
getSymmNodes table coors degrees = symmCoors ++ valuesList
    where symmCoors = getSymmCoorsLists $ table coors degrees
          valuesList = getSymmValuesList $ table symmCoors

variableInterpolation :: CoorsLists -> Coors -> Degrees -> Double

interpolation :: CoorsTable -> Coors -> Degrees -> Double
interpolation [] [_] [_] = 0.0
interpolation [_] [] [_] = 0.0
interpolation [_] [_] [] = 0.0
interpolation table coors degrees
    | length degrees /= length coors = 0.0
    | otherwise = variableInterpolation $ symmNodes coors degrees
    where symmNodes = getSymmCoorsLists $ table coors
