import           Data.Array
import           Data.Char
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

mkArrayFuelCosts :: [Int] -> Array Int Int
mkArrayFuelCosts positions =
  let (low, high) = (minimum positions, maximum positions) in listArray (low, high) $ repeat 0

type CostFn = Int -> Int -> Int

cost :: CostFn
cost index position = abs (index - position)

addFuelForPositions :: CostFn -> [Int] -> Array Int Int -> Array Int Int
addFuelForPositions costFn positions array = foldr f array positions
  where f pos array = accum (+) array [ (index, costFn index pos) | (index, elem) <- assocs array ]

getLeastCost :: CostFn -> [Int] -> Int
getLeastCost cost positions =
  let startingArray = mkArrayFuelCosts positions
  in  minimum $ elems $ addFuelForPositions cost positions startingArray

partOne :: [Int] -> String
partOne xs = show $ getLeastCost cost xs

-- Part Two

cost' :: Int -> Int -> Int
cost' index position = let n = abs (index - position) in n * (n + 1) `div` 2

partTwo :: [Int] -> String
partTwo xs = show $ getLeastCost cost' xs

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
