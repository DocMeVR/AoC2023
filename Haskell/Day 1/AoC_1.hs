module AoC_1 where

    import System.IO
    import Data.Char

    main :: IO ()
    main = do   
        input <- readFile "input_day1.txt"
        print $ part2 input

    -- Part 1

    onlyDigits :: String -> String
    onlyDigits = filter isDigit

    concatStringFirstAndLast :: String -> String
    concatStringFirstAndLast [] = []
    concatStringFirstAndLast [x] = [x, x]
    concatStringFirstAndLast (x:xs) = x : (last xs) : []

    part1 :: String -> Int
    part1 = foldr (\line acc -> acc + read (concatStringFirstAndLast $ onlyDigits line)) 0 . lines

    -- Part 2
    wordsToDigits :: String -> String
    wordsToDigits [] = []
    wordsToDigits ('o':'n':'e':xs) = '1' : wordsToDigits ('e':xs)
    wordsToDigits ('t':'w':'o':xs) = '2' : wordsToDigits ('o':xs)
    wordsToDigits ('t':'h':'r':'e':'e':xs) = '3' : wordsToDigits ('e':xs)
    wordsToDigits ('f':'o':'u':'r':xs) = '4' : wordsToDigits ('r':xs)
    wordsToDigits ('f':'i':'v':'e':xs) = '5' : wordsToDigits ('e':xs)
    wordsToDigits ('s':'i':'x':xs) = '6' : wordsToDigits ('x':xs)
    wordsToDigits ('s':'e':'v':'e':'n':xs) = '7' : wordsToDigits ('n':xs)
    wordsToDigits ('e':'i':'g':'h':'t':xs) = '8' : wordsToDigits ('t':xs)
    wordsToDigits ('n':'i':'n':'e':xs) = '9' : wordsToDigits ('e':xs)
    wordsToDigits (x:xs) = x : wordsToDigits xs

    part2 :: String -> Int
    part2 = foldr (\line acc -> acc + read (concatStringFirstAndLast $ onlyDigits $ wordsToDigits line)) 0 . lines