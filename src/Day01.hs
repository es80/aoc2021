import           System.Environment
import           System.IO

-- Part One

countIncreases :: [Int] -> Int
countIncreases [x] = 0
countIncreases (x : y : ys) =
  let count = countIncreases (y : ys) in if x < y then 1 + count else count

partOne :: [Int] -> String
partOne = show . countIncreases

-- Part Two

groupTripleSums :: [Int] -> [Int]
groupTripleSums [x, y]           = []
groupTripleSums (x : y : z : zs) = (x + y + z) : groupTripleSums (y : z : zs)

partTwo :: [Int] -> String
partTwo = show . countIncreases . groupTripleSums

-- Parsing

parseInput :: String -> [Int]
parseInput = map read . lines

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
