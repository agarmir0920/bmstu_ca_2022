module Table
    ( Var
    , Coors
    , CoorsTable
    , parseDataTable
    ) where

type ParsingText = [String]

type Var = Double
type Coors = [Var]
type CoorsTable = [Coors]

parseDataTable :: ParsingText -> CoorsTable
parseDataTable [] = []
parseDataTable text = map (toCoors, lines text)
    where
    toCoors = \str -> map (toDouble, words str)
    toDouble = \str -> read str :: Double
 
