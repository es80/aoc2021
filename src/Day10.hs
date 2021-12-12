import           Data.List
import           Data.Maybe
import           System.Environment
import           System.IO

-- Part One

push :: a -> [a] -> [a]
push x xs = x : xs

peek :: [a] -> Maybe a
peek []       = Nothing
peek (x : xs) = Just x

pop :: [a] -> [a]
pop []       = []
pop (x : xs) = xs

pairOpener :: Char -> Bool
pairOpener c = c `elem` "([{<"

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

matchingFail :: String -> String -> Maybe Char
matchingFail stack []       = Nothing
matchingFail stack (x : xs) = if pairOpener x
  then matchingFail (push x stack) xs
  else peek stack >>= (\c -> if match c == x then matchingFail (pop stack) xs else Just x)

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137

totalPoints :: String -> Int
totalPoints = sum . map points

partOne :: [String] -> String
partOne = show . sum . map points . mapMaybe (matchingFail [])

-- Part Two

incomplete :: String -> String -> Maybe String
incomplete stack []       = Just stack
incomplete stack (x : xs) = if pairOpener x
  then incomplete (push x stack) xs
  else peek stack >>= (\c -> if match c == x then incomplete (pop stack) xs else Nothing)

findCompletions :: String -> String
findCompletions = map match

points' :: Char -> Int
points' ')' = 1
points' ']' = 2
points' '}' = 3
points' '>' = 4

subtotal :: String -> Int
subtotal = foldl f 0 where f num char = num * 5 + points' char

median :: [a] -> a
median xs = xs !! (length xs `div` 2)

partTwo :: [String] -> String
partTwo = show . median . sort . map (subtotal . findCompletions) . mapMaybe (incomplete [])

-- Parsing

parseInput :: String -> [String]
parseInput = lines

-- IO

readInputFile :: IO String
readInputFile = do
  args     <- getArgs
  inHandle <- openFile (head args) ReadMode
  hGetContents inHandle

main :: IO ()
main = do
  input <- readInputFile
  putStrLn . partOne . parseInput $ input
  putStrLn . partTwo . parseInput $ input
