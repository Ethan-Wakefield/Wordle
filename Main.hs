{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char
import Data.List
import Data.List.NonEmpty (groupAllWith, groupWith)
import Data.Ord (comparing)

normalise :: String -> String
normalise inp = map toUpper (filter isLetter inp)


isValid :: String -> Bool
isValid guess = normalise guess `elem` guessList


parseCommand :: String -> Command
parseCommand x = case normalise x of
                    "LETTERS" -> ShowLetters
                    "GIVEUP" -> GiveUp
                    _ -> Guess (normalise x)


match :: Char -> Char -> ExactMatch
match a b   | a == b  = IsExact a
            | a /= b  = IsNotExact a


exactMatches :: String -> String -> [ExactMatch]
exactMatches [] [] = []
exactMatches (x:xs) (y:ys) = match x y : exactMatches xs ys


removeExacts :: [ExactMatch] -> String -> String
removeExacts matches ans = [y | (IsNotExact x, y) <- zip matches ans]


getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches [] _ = []
getMatches (IsExact n:xs) g = Exact : getMatches xs g
getMatches (IsNotExact n:xs) g = if n `elem` g then
                                            Partial : getMatches xs (delete n g)
                                        else
                                            None : getMatches xs g


matchingAlgo :: String -> String -> [Match]
matchingAlgo a b = let e = exactMatches a b
                    in getMatches e (removeExacts e b)


eliminate :: String -> [Match] -> [String] -> [String]
eliminate guess matching words = [x | x <- words, matchingAlgo guess x == matching]


eliminateAll :: [(String, [Match])] -> [String]
eliminateAll pairs = let f n = uncurry eliminate n guessList
                        in foldr1 intersect (map f pairs)


eliminateAll' :: [(String, [Match])] -> [String]
eliminateAll' pairs = let f n = uncurry eliminate n answerList 
                        in foldr1 intersect (map f pairs)


score' :: String -> [String] -> [(String, [Match])] -> Int
score' word potAns pairs = let 
                        newPair = [(word, matchingAlgo word h) : pairs | h <- potAns]
                        lists = map eliminateAll' newPair
                         in sum (map length lists)


nextGuess :: [(String, [Match])] -> String
nextGuess [] = "SOARE"
nextGuess [x] = "TULIP"
nextGuess pairs = let potentialAns = eliminateAll' pairs 
                      scores = [score' x potentialAns pairs | x <- potentialAns]
                      in snd (minimumBy (comparing fst) (zip scores potentialAns))
