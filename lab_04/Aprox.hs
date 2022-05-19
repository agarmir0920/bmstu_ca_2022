module Aprox where

type Polynom = [Double]

type PolDegree = Double
type Degree = Double

oneDimAprox :: FuncTable -> PolDegree -> Polynom

twoDimAprox :: FuncTable -> PolDegree -> Polynom

getAproxPolynom :: FuncTable -> PolDegree -> Polynom
getAproxPolynom [] _ = []
getAproxPolynom table polDegree
    | length (table !! 0) == 3 = oneDimAprox table polDegree
    | length (table !! 0) == 4 = twoDimAprox table polDegree
    | otherwise = []

