import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Environment
import           System.IO

-- Part One

type Signals = [String]
type Outputs = [String]

lengths2347 :: Outputs -> [Int]
lengths2347 xs = filter f (map length xs) where f x = x `elem` [2, 3, 4, 7]

partOne :: [(Signals, Outputs)] -> String
partOne = show . length . concatMap (lengths2347 . snd)

-- Part Two

findByLength :: Int -> Signals -> Signals
findByLength n = filter (\x -> length x == n)

applyConstraints :: Signals -> [Signals]
applyConstraints xs = do
  zero  <- findByLength 6 xs
  one   <- findByLength 2 xs
  two   <- findByLength 5 xs
  three <- findByLength 5 xs
  four  <- findByLength 4 xs
  five  <- findByLength 5 xs
  six   <- findByLength 6 xs
  seven <- findByLength 3 xs
  eight <- findByLength 7 xs
  nine  <- findByLength 6 xs
  let res = [zero, one, two, three, four, five, six, seven, eight, nine]
  guard (length (nub res) == 10)
  guard (one `isSubsequenceOf` three)
  guard (four `isSubsequenceOf` nine)
  guard (five `isSubsequenceOf` six)
  guard (five `isSubsequenceOf` nine)
  return res

uniqueSignals :: Signals -> Signals
uniqueSignals signals = head . applyConstraints $ map sort signals

outputDigits :: (Signals, Outputs) -> [Int]
outputDigits (signals, outputs) = mapMaybe ((`elemIndex` signals) . sort) outputs

digitsToInt :: [Int] -> Int
digitsToInt = foldl f 0 where f acc x = acc * 10 + x

partTwo :: [(Signals, Outputs)] -> String
partTwo = show . sum . map (digitsToInt . outputDigits . first uniqueSignals)

-- Parsing

parseLine :: String -> (Signals, Outputs)
parseLine line =
  let [signals, outputs] = map (splitOn " ") (splitOn " | " line) in (signals, outputs)

parseInput :: String -> [(Signals, Outputs)]
parseInput = map parseLine . lines

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
