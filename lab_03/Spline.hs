module Spline (splineInterpolation) where

import Debug.Trace

import Interpol
import Table

type Left = Double
type Right = Double

type Coef = Double
type CoefsList = [Coef]

data Polynom = Polynom {
    a :: Coef,
    b :: Coef,
    c :: Coef,
    d :: Coef,
    xf :: Double
} deriving (Show)

data SplinePart = SplinePart {
    xb :: Var,
    xe :: Var,
    polynom :: Polynom
} deriving (Show)

type Spline = [SplinePart]

data RunCoefs = RunCoefs {
    ksi :: Coef,
    eta :: Coef
} deriving (Show)

type RunCoefsList = [RunCoefs]

-- Вычисление коэффициентов a
getALst :: CoorsTable -> CoefsList
getALst pairsLst
    | length pairsLst <= 1 = []
    | otherwise = y : ((getALst . tail) pairsLst)
    where
    y = (last . head) pairsLst

-- Вычисление коэффициентов b
getBLst :: CoorsTable -> CoefsList -> CoefsList
getBLst pairsLst cLst
    | length pairsLst <= 2 = [bn]
    | otherwise = bi : (getBLst (tail pairsLst) (tail cLst))
    where
    bi = (y - yprev) / h - h * (cnext + 2 * c) / 3
    bn = (y - yprev) / h - 2 / 3 * h * c

    h = x - xprev

    x = head curPair
    xprev = head prevPair

    y = last curPair
    yprev = last prevPair

    prevPair = pairsLst !! 0
    curPair = pairsLst !! 1

    c = cLst !! 0
    cnext = cLst !! 1

-- Вычисление прогоночных коэффициентов
getRunCoefsLst :: CoorsTable -> Left -> RunCoefsList -> RunCoefsList
getRunCoefsLst pairsLst left runCoefsLst
    | null runCoefsLst = getRunCoefsLst pairsLst left [fstRunCoefs]
    | length pairsLst <= 2 = runCoefsLst
    | otherwise = getRunCoefsLst (tail pairsLst) left (runCoefsLst ++ [newRunCoefs]) 
    where
    fstRunCoefs = RunCoefs 0 (left / 2)
    newRunCoefs = RunCoefs newksi neweta

    newksi = negate $ h / (prevh * lastksi + 2 * (h + prevh))
    neweta = (f - prevh * lasteta) / (prevh * lastksi + 2 * (h + prevh))
    f = 3 * ((y - yp) / h - (yp - ypp) / prevh)

    h = x - xp
    prevh = xp - xpp

    pair = pairsLst !! 2
    pairp = pairsLst !! 1
    pairpp = pairsLst !! 0

    x = head pair
    y = last pair

    xp = head pairp
    yp = last pairp

    xpp = head pairpp
    ypp = last pairpp

    lastcoefs = last runCoefsLst
    lasteta = eta lastcoefs
    lastksi = ksi lastcoefs

-- Вычисление коэффициентов c
getCLst :: RunCoefsList -> Left -> Right -> CoefsList -> CoefsList
getCLst runCoefsLst left right cLst
    | null cLst = getCLst runCoefsLst left right [right / 2]
    | null runCoefsLst = cLst
    | otherwise = getCLst (init runCoefsLst) left right (ci : cLst)
    where
    ci = lastksi * cnext + lasteta
    cnext = head cLst
    
    lastRunCoefs = last runCoefsLst
    lastksi = ksi lastRunCoefs
    lasteta = eta lastRunCoefs

-- Вычисление коэффициентов d
getDLst :: CoorsTable -> CoefsList -> CoefsList
getDLst pairsLst cLst
    | length pairsLst <= 2 = [negate (c / 3 / h)]
    | otherwise = [(cnext - c) / 3 / h] ++ (getDLst (tail pairsLst) (tail cLst))
    where
    c = cLst !! 0
    cnext = cLst !! 1

    h = x - xp

    pairp = pairsLst !! 0
    pair = pairsLst !! 1

    x = head pair
    xp = head pairp

-- Образование сплайна с помощью списков соответствующих коэффициентов
getSplineWithCoefs :: CoorsTable -> CoefsList -> CoefsList -> CoefsList -> CoefsList -> Spline
getSplineWithCoefs pairsLst aLst bLst cLst dLst
    | length pairsLst <= 1 = []
    | otherwise = splinePart : (getSplineWithCoefs (tail pairsLst) (tail aLst) (tail bLst) (tail cLst) (tail dLst))
    where
    splinePart = SplinePart x xNext pol

    pair = pairsLst !! 0
    nextPair = pairsLst !! 1

    x = head pair
    xNext = head nextPair

    pol = Polynom ai bi ci di x
    ai = head aLst
    bi = head bLst
    ci = head cLst
    di = head dLst

-- Вычисление сплайна
getSpline :: CoorsTable -> Left -> Right -> Spline
getSpline pairsLst left right = getSplineWithCoefs pairsLst aLst bLst cLst dLst
    where
    aLst = getALst pairsLst
    bLst = getBLst pairsLst cLst
    cLst = getCLst runCoefsLst left right []
    dLst = getDLst pairsLst cLst
    runCoefsLst = getRunCoefsLst pairsLst left []

-- Значение полинома сплайна
getPolynomValue :: Polynom -> Var -> Double
getPolynomValue pol var = ai + bi * (var - xi) + ci * (var - xi) ** 2 + di * (var - xi) ** 3
    where
    ai = a pol
    bi = b pol
    ci = c pol
    di = d pol
    xi = xf pol

-- Значение сплайна
getSplineValue :: Spline -> Var -> Double
getSplineValue spline var
    | (var >= x1) && (var <= x2) = getPolynomValue pol var
    | otherwise = getSplineValue (tail spline) var
    where
    x1 = xb curSplinePart
    x2 = xe curSplinePart
    pol = polynom curSplinePart
    curSplinePart = head spline

-- Интерполяция сплайнами
splineInterpolation :: CoorsTable -> Var -> Left -> Right -> Double
splineInterpolation [] _ _ _ = 0 / 0
splineInterpolation coorsTable var left right
    | length xs /= length ys = 0 / 0
    | otherwise = getSplineValue spline var
    where
    xs = head coorsTable
    ys = last coorsTable
    spline = getSpline coorsTable left right

