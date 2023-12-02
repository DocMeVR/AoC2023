import System.IO
import Data.Char

-- Goal:
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
-- to (1, [[(3, "blue"), (4, "red")], [(1, "red"), (2, "green"), (6, "blue")], [(2, "green")]])
type Round = [(Int, String)]
type Game = (Int, [Round])

main :: IO ()
main = do   
    input <- readFile "input_day2.txt"
    print $ part2 input

{- Part 1
    You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

    For example, the record of a few games might look like this:

    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

    In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

    The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

    In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

    Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
-}

-- remove anything besides numbers and letters and whitespaces
removeSpecialChars :: String -> String
removeSpecialChars = filter (\x -> isDigit x || isLetter x || isSpace x || x == ';')

-- drop everything before the first colon
-- Turn "3 blue 7 green 10 red; 4 green 4 red; 1 green 7 blue 5 red; 8 blue 10 red; 7 blue 19 red 1 green"
-- to [Round]: [[(3, blue), (7, green), (10, red)], [(4, green), (4, red)], [(1, green), (7, blue), (5, red)], [(8, blue), (10, red)], [(7, blue), (19, red), (1, green)]]
roundsAsStrings :: String -> [String]
roundsAsStrings = splitOn ';' . removeSpecialChars . dropWhile (/= ':')

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOn c s''
                    where (w, s'') = break (== c) s'

toRounds :: [String] -> [Round]
toRounds [] = []
toRounds (x:xs) = (toRound . words) x : toRounds xs
    where
        toRound :: [String] -> Round
        toRound [] = []
        toRound [x] = []
        toRound (x:y:xs) = (read x, y) : toRound xs


-- We can drop first 5 letters "Game " and then read the number
-- check for colon everything before is the game number
gameId :: String -> Int
gameId = read . takeWhile (/= ':') . drop 5

toGame :: String -> Game
toGame s = (gameId s, toRounds $ roundsAsStrings s)

-- game is not valid if #red > 12, #green > 13 or #blue > 14, I didn't understand the game...
{-
isValid :: Game -> Int
isValid (gameId, rounds) = if isValid' 0 0 0 rounds then gameId else 0
    where
        isValid' :: Int -> Int -> Int -> [Round] -> Bool
        isValid' r g b [] = r <= 12 && g <= 13 && b <= 14 
        isValid' r g b ((round):rounds) = isValid' (r + sumColor "red" round) (g + sumColor "green" round) (b + sumColor "blue" round) rounds
            where
                sumColor :: String -> Round -> Int
                sumColor _ [] = 0
                sumColor color ((n, c):round) = if color == c then n + sumColor color round else sumColor color round
-}

-- game is not valid if red > 12, green > 13 or blue > 14 but in one pass
-- return gameID
isValid :: Game -> Int
isValid (gameId, rounds) = if (foldr (\round acc -> acc && (isValidRound round)) True rounds) then gameId else 0
    where
        isValidRound :: Round -> Bool
        isValidRound round = foldr (\(num, color) acc -> acc && (isValidColor num color)) True round
            where
                isValidColor :: Int -> String -> Bool
                isValidColor num color = case color of
                    "red" -> num <= 12
                    "green" -> num <= 13
                    "blue" -> num <= 14
                    _ -> True

part1 :: String -> Int
part1 = foldr (\game acc -> acc + isValid game) 0 . map toGame . lines

{- Part 2
    As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?

    Again consider the example games from earlier:

    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

    In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
    Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
    Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
    Game 4 required at least 14 red, 3 green, and 15 blue cubes.
    Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

    The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.

    For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?
-}

-- find the max number of each color in one game
-- returns (redMax, greenMax, blueMax)
maxColor :: Game -> (Int, Int, Int)
maxColor (gameId, rounds) = foldr (\round (r, g, b) -> (max r (sumColor "red" round), max g (sumColor "green" round), max b (sumColor "blue" round))) (0, 0, 0) rounds
    where
        sumColor :: String -> Round -> Int
        sumColor _ [] = 0
        sumColor color ((n, c):round) = if color == c then n + sumColor color round else sumColor color round

-- now we got everything, I love foldr, how could you tell?
part2 :: String -> Int
part2 = foldr (\game acc -> acc + (foldr (\(r, g, b) acc -> acc + r * g * b) 0 [maxColor game])) 0 . map toGame . lines

-- I am not proud of this solution, but I got my answer and I am happy with it
-- Bareboning Haskell is fun, but I am not sure if I would like to do it for a living