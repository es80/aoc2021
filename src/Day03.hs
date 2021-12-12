import           Control.Applicative
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type BinaryNum = [Int]

mostCommonBits :: [BinaryNum] -> BinaryNum
mostCommonBits nums =
  let tally = foldr1 (zipWith (+)) nums
      len   = length nums
  in  map (\t -> if t * 2 >= len then 1 else 0) tally

leastCommonBits :: [BinaryNum] -> BinaryNum
leastCommonBits nums = map (1 -) (mostCommonBits nums)

bitsToInt :: BinaryNum -> Int
bitsToInt xs = bitsToInt' (length xs - 1) xs
 where
  bitsToInt' pos []       = 0
  bitsToInt' pos (x : xs) = let rest = bitsToInt' (pos - 1) xs in (2 ^ pos) * x + rest

partOne :: [BinaryNum] -> String
partOne nums = show $ bitsToInt (mostCommonBits nums) * bitsToInt (leastCommonBits nums)

-- Part Two

type Bit = Int
type Index = Int
type Stategy = [BinaryNum] -> Index -> Bit

mostCommonBit :: [BinaryNum] -> Index -> Bit
mostCommonBit nums idx =
  let tally = sum $ map (!! idx) nums
      len   = length nums
  in  if tally * 2 >= len then 1 else 0

leastCommonBit :: [BinaryNum] -> Index -> Bit
leastCommonBit nums idx = 1 - mostCommonBit nums idx

applyFiltering :: Stategy -> Index -> [BinaryNum] -> BinaryNum
applyFiltering strategy idx [num] = num
applyFiltering strategy idx nums =
  let nextNums = filter (\num -> num !! idx == strategy nums idx) nums
  in  applyFiltering strategy (idx + 1) nextNums

partTwo :: [BinaryNum] -> String
partTwo nums =
  let oxy = bitsToInt $ applyFiltering mostCommonBit 0 nums
      co2 = bitsToInt $ applyFiltering leastCommonBit 0 nums
  in  show $ oxy * co2

-- Parsing

parseBit :: ReadP Int
parseBit = read <$> (string "0" <|> string "1")

parseLine :: ReadP [Int]
parseLine = many1 parseBit <* eof

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> [BinaryNum]
parseInput = map (runParser parseLine) . lines

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
