module Uzd1
where

import Data.Bool
import Data.Char

msg :: String
--msg ="(l(m\"x\"2\"y\"1\"v\"\"x\")(m\"x\"0\"y\"1\"v\"\"o\")(m\"x\"2\"y\"1\"v\"\"x\"))"
--msg = "l[m[\"x\";2;\"y\";2;\"v\";\"x\"];m[\"x\";0;\"y\";2;\"v\";\"o\"];m[\"x\";1;\"y\";0;\"v\";\"x\"]]"
msg = "l[m[\"x\";1;\"y\";1;\"v\";\"x\"];m[\"x\";0;\"y\";1;\"v\";\"o\"];m[\"x\";0;\"y\";0;\"v\";\"x\"];m[\"x\";2;\"y\";2;\"v\";\"o\"];m[\"x\";2;\"y\";2;\"v\";\"x\"]]"

data Move = Move {
    pozicijaX :: Int,
    pozicijaY :: Int,
    zaidejas :: Char 
} deriving Show

gaukX :: Move -> Int
gaukX (Move x _ _) = x

gaukY :: Move -> Int
gaukY (Move _ y _) = y

gaukZaideja :: Move -> Char
gaukZaideja (Move _ _ z) = z

validate :: String -> String
validate str =
    let
        (moves) = parse str
        correstQuantity = tikrintiKieki (moves)
    in if (correstQuantity)
        then 
            let 
               correctAlignment = isAlignmentCorrect (moves)
            in if (correctAlignment)
                then "Lenta teisingai sudeliota"
                else "Lentoje yra klaidu!"
            else "Lentoje yra klaidu!"

isAlignmentCorrect :: [Move] -> Bool
isAlignmentCorrect [] = True 
isAlignmentCorrect (h:[]) = True 
isAlignmentCorrect (h:t:t1) = 
    let
        deepResult = isAlignmentCorrect (t:t1)
        result = tikrintiMoves h t
    in
        if ((not result) && deepResult)
        then isAlignmentCorrect (h:t1)
        else False

tikrintiMoves :: Move -> Move -> Bool
tikrintiMoves move1 move2 = 
    if (gaukX move1 == gaukX move2
        && gaukY move1 == gaukY move2)
        then True
        else False

tikrintiKieki :: [Move] -> Bool
tikrintiKieki move =
    tikrintiKieki' 0 0 move
    where
        tikrintiKieki' :: Int -> Int -> [Move] -> Bool
        tikrintiKieki' x y [] = lygintiKieki x y
        tikrintiKieki' x y (h:t) =
            if (gaukZaideja h == 'x')
                then tikrintiKieki' (x + 1) y t
                else tikrintiKieki' x (y + 1) t

lygintiKieki :: Int -> Int -> Bool
lygintiKieki x y = if (abs(x - y) < 2)
    then True
    else False

parse :: String -> [Move]
parse str = 
    let
        str1 = removeWhiteSpaces str
        str2 = removeMExpressionPref str1
        str3 = removeOpeningSquareBracket str2
        (moves, result) = parseMExpressions [] str3
    in reverse moves

parseMExpressions :: [Move] -> String -> ([Move], String)
parseMExpressions acc (']':t) = (acc, t)
parseMExpressions acc (';':t) = parseMExpressions acc t
parseMExpressions acc mExpressions =
    let
        (propertyName1, mExpressions1) = readSymbol mExpressions
        mExpressions2 = removeSemicolon mExpressions1
        (move, rest) = parseMExpression mExpressions
    in
        parseMExpressions (move:acc) rest

parseMExpression :: String -> (Move, String)
parseMExpression mExpression =
    let
        mExpression1 = removeMExpressionPrefix mExpression
        mExpression2 = removeOpeningSquareBracket mExpression1

        (propertyName1, mExpression3) = readSymbol mExpression2
        mExpression4 = removeSemicolon mExpression3
        (digit1, mExpression5) = readDigit mExpression4
        mExpression6 = removeSemicolon mExpression5

        (propertyName2, mExpression7) = readSymbol mExpression6
        mExpression8 = removeSemicolon mExpression7
        (digit2, mExpression9) = readDigit mExpression8
        mExpression10 = removeSemicolon mExpression9

        (propertyName3, mExpression11) = readSymbol mExpression10
        mExpression12 = removeSemicolon mExpression11
        (player, mExpression13) = readPlayer mExpression12
        mExpression14 = removeClosingSquareBracket mExpression13

        rest = mExpression14
    in
       (Move digit1 digit2 player, rest)

removeWhiteSpaces :: [Char] -> [Char]
removeWhiteSpaces str =
  removeWhiteSpaces' [] str
  where
    removeWhiteSpaces' :: [Char] -> [Char] -> [Char]
    removeWhiteSpaces' acc [] = acc
    removeWhiteSpaces' acc (h:t) =
        if (h /= ' ')
            then removeWhiteSpaces' (acc++[h]) t
            else removeWhiteSpaces' acc t

removeOpeningSquareBracket :: String -> String
removeOpeningSquareBracket ('[':t) = t
removeOpeningSquareBracket _ = error "'[' expected"

removeClosingSquareBracket :: String -> String
removeClosingSquareBracket (']':t) = t
removeClosingSquareBracket _ = error "']' expected"

removeMExpressionPref :: String -> String
removeMExpressionPref ('l':t) = t
removeMExpressionPref _ = error "'l' expected"

removeMExpressionPrefix :: String -> String
removeMExpressionPrefix ('m':t) = t
removeMExpressionPrefix _ = error "'m' expected"

removeSemicolon :: String -> String
removeSemicolon (';':t) = t
removeSemicolon _ = error "';' expected"

readSymbol :: String -> (Char, String)
readSymbol ('\"':_:'\"':t) = ('k', t)
readSymbol _  = error "Property name expected"

readDigit :: String -> (Int, String)
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest) 
readDigit ('2':rest) = (2, rest) 
readDigit _ = error "Digit expected"

readPlayer :: String -> (Char, String)
readPlayer ('\"': 'x' : '\"': rest) = ('x', rest)
readPlayer ('\"': 'X' : '\"': rest) = ('x', rest)
readPlayer ('\"': 'o' : '\"': rest) = ('o', rest)
readPlayer ('\"': 'O' : '\"': rest) = ('o', rest)
readPlayer _ = error "Player expected"
