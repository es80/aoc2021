import           Data.Array
import           Data.Char
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

arrayBounds :: (Int, Int)
arrayBounds = (0, 8)

mkTimersTally :: [Int] -> Array Int Integer
mkTimersTally timers = accumArray (+) 0 arrayBounds [ (i, 1) | i <- timers ]

applyDay :: Array Int Integer -> Array Int Integer
applyDay arr = arr // [ update arr i | (i, _e) <- assocs arr ]

update :: Array Int Integer -> Int -> (Int, Integer)
update arr 6 = (6, (arr ! 7) + (arr ! 0))
update arr 8 = (8, arr ! 0)
update arr i = (i, arr ! (i + 1))

applyDaysAndSum :: Int -> Array Int Integer -> Integer
applyDaysAndSum n timers = sum . elems $ finalTally
  where finalTally = foldl (\acc _ -> applyDay acc) timers [1 .. n]

partOne :: [Int] -> String
partOne = show . applyDaysAndSum 80 . mkTimersTally

-- Part Two

partTwo :: [Int] -> String
partTwo = show . applyDaysAndSum 256 . mkTimersTally

-- Parsing

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseContent :: ReadP [Int]
parseContent = sepBy parseInt (string ",") <* string "\n"

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> [Int]
parseInput = runParser parseContent

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
