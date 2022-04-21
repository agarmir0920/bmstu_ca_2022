module Spline where

import Interpol
import Table

type PairsList = [(Var, Var)]

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
}

data SplinePart = SplinePart {
    xb :: Var,
    xe :: Var,
    polynom :: Polynom
}

type Spline = [SplinePart]

data RunCoefs = RunCoefs {
    ksi :: Coef,
    eta :: Coef
}

type RunCoefsList = [RunCoefs]

getALst :: PairsList -> CoefsList
getALst pairsLst
    | length pairsLst <= 1 = []
    | otherwise = y : ((getALst . tail) pairsLst)
    where
    y = (snd . head) pairsLst

getBLst :: PairsList -> CoefsList -> CoefsList
getBLst pairsLst cLst
    | length pairsLst <= 2 = [bn]
    | otherwise = bi : (getBLst (tail pairsLst) (tail cLst))
    where
    bi = (y - yprev) / h - h * (cnext - 2 * c) / 3
    bn = (y - yprev) / h - 2 * h * cnext / 3

    h = x - xprev

    x = fst curPair
    xprev = fst prevPair

    y = snd curPair
    yprev = snd prevPair

    prevPair = head pairsLst
    curPair = (head . tail) pairsLst

    c = head cLst
    cnext = (head . tail) cLst

getRunCoefsLst :: PairsList -> Left -> RunCoefsList -> RunCoefsList
getRunCoefsLst pairsLst left runCoefsLst
    | null runCoefsLst = getRunCoefsLst pairsLst left [fstRunCoefs]
    | length pairsLst <= 2 = runCoefsLst
    | otherwise = getRunCoefsLst (tail pairsLst) left (runCoefsLst ++ [newRunCoefs])
    where
    fstRunCoefs = RunCoefs 0 (left / 2)
    newRunCoefs = RunCoefs newksi neweta

    newksi = negate (h / (prevh * lastksi + 2 * (h + prevh)))
    neweta = (f - prevh * lasteta) / (prevh * lastksi + 2 * (h + prevh))
    f = 3 * ((y - yp) / h - (yp - ypp) / prevh)

    h = x - xp
    prevh = xp - xpp

    pair = (head . tail . tail) pairsLst
    pairp = (head . tail) pairsLst
    pairpp = head pairsLst

    x = fst pair
    y = snd pair

    xp = fst pairp
    yp = snd pairp

    xpp = fst pairpp
    ypp = snd pairpp

    lastcoefs = last runCoefsLst
    lasteta = eta lastcoefs
    lastksi = ksi lastcoefs

getCLst :: RunCoefsList -> Left -> Right -> CoefsList -> CoefsList
getCLst runCoefsLst left right cLst
    | null cLst = getCLst runCoefsLst left right [right]
    | null runCoefsLst = left : cLst
    | otherwise = getCLst (init runCoefsLst) left right (ci : cLst)
    where
    ci = lastksi * cnext + lasteta
    cnext = head cLst
    
    lastRunCoefs = last runCoefsLst
    lastksi = ksi lastRunCoefs
    lasteta = eta lastRunCoefs

getDLst :: PairsList -> CoefsList -> CoefsList
getDLst pairsLst cLst
    | length pairsLst <= 2 = [negate (c / 3 / h)]
    | otherwise = [(cnext - c) / 3 / h] ++ (getDLst (tail pairsLst) (tail cLst))
    where
    c = head cLst
    cnext = (head . tail) cLst

    h = x - xp

    pairp = head pairsLst
    pair = (head . tail) pairsLst

    x = fst pair
    xp = fst pairp

getSplineWithCoefs :: PairsList -> CoefsList -> CoefsList -> CoefsList -> CoefsList -> Spline
getSplineWithCoefs pairsLst aLst bLst cLst dLst
    | length pairsLst <= 1 = []
    | otherwise = splinePart : (getSplineWithCoefs (tail pairsLst) (tail aLst) (tail bLst) (tail cLst) (tail dLst))
    where
    splinePart = SplinePart x xNext pol

    pair = head pairsLst
    nextPair = (head . tail) pairsLst

    x = fst pair
    xNext = fst nextPair

    pol = Polynom ai bi ci di x
    ai = head aLst
    bi = head bLst
    ci = head cLst
    di = head dLst

getSpline :: PairsList -> Left -> Right -> Spline
getSpline pairsLst left right = getSplineWithCoefs pairsLst aLst bLst cLst dLst
    where
    aLst = getALst pairsLst
    bLst = getBLst pairsLst cLst
    cLst = getCLst runCoefsLst left right []
    dLst = getDLst pairsLst cLst
    runCoefsLst = getRunCoefsLst pairsLst left []

getPolynomValue :: Polynom -> Var -> Double
getPolynomValue pol var = ai + bi * (var - xi) + ci * (var - xi) ** 2 + di * (var - xi) ** 3
    where
    ai = a pol
    bi = b pol
    ci = c pol
    di = d pol
    xi = xf pol

getSplineValue :: Spline -> Var -> Double
getSplineValue spline var
    | (var >= x1) && (var <= x2) = getPolynomValue pol var
    | otherwise = getSplineValue (tail spline) var
    where
    x1 = xb curSplinePart
    x2 = xe curSplinePart
    pol = polynom curSplinePart
    curSplinePart = head spline

splineInterpolation :: CoorsTable -> Var -> Left -> Right -> Double
splineInterpolation [] _ _ _ = 0 / 0
splineInterpolation coorsTable var left right
    | length xs /= length ys = 0 / 0
    | otherwise = getSplineValue spline var
    where
    xs = head coorsTable
    ys = last coorsTable
    spline = getSpline pairsLst left right
    pairsLst = zip xs ys

