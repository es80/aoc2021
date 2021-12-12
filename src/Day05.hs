import           Data.Char
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type Point = (Int, Int)

zipPadded :: [a] -> [b] -> [(a, b)]
zipPadded [x]      ys       = zip (repeat x) ys
zipPadded xs       [y     ] = zip xs (repeat y)
zipPadded (x : xs) (y : ys) = (x, y) : zipPadded xs ys

mkPointsFromLine :: (Point, Point) -> [Point]
mkPointsFromLine ((x1, y1), (x2, y2))
  | x1 <= x2 && y1 <= y2 = zipPadded [x1 .. x2] [y1 .. y2]
  | x1 > x2 && y1 <= y2  = zipPadded [x1, x1 - 1 .. x2] [y1 .. y2]
  | x1 <= x2 && y1 > y2  = zipPadded [x1 .. x2] [y1, y1 - 1 .. y2]
  | x1 > x2 && y1 > y2   = zipPadded [x1, x1 - 1 .. x2] [y1, y1 - 1 .. y2]

mkPointsFromLineNoDiagonals :: (Point, Point) -> [Point]
mkPointsFromLineNoDiagonals ((x1, y1), (x2, y2)) =
  if x1 == x2 || y1 == y2 then mkPointsFromLine ((x1, y1), (x2, y2)) else []

mkSetsOfPoints :: [Point] -> (Set Point, Set Point)
mkSetsOfPoints [] = (Set.empty, Set.empty)
mkSetsOfPoints (point : points) =
  let (seenOnce, seenMany) = mkSetsOfPoints points
  in  if Set.member point seenOnce
        then (seenOnce, Set.insert point seenMany)
        else (Set.insert point seenOnce, seenMany)

overlapPoints :: ((Point, Point) -> [Point]) -> [(Point, Point)] -> Int
overlapPoints mkPointsFn linesList =
  let allPoints            = concatMap mkPointsFn linesList
      (seenOnce, seenMany) = mkSetsOfPoints allPoints
  in  length seenMany

partOne :: [(Point, Point)] -> String
partOne = show . overlapPoints mkPointsFromLineNoDiagonals

-- Part Two

partTwo :: [(Point, Point)] -> String
partTwo = show . overlapPoints mkPointsFromLine

-- Parsing

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseLine :: ReadP (Point, Point)
parseLine = do
  x1 <- parseInt
  string ","
  y1 <- parseInt
  string " -> "
  x2 <- parseInt
  string ","
  y2 <- parseInt
  return ((x1, y1), (x2, y2))

parseContent :: ReadP [(Point, Point)]
parseContent = endBy parseLine (string "\n") <* eof

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> [(Point, Point)]
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
