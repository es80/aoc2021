import           Data.Array
import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.List.Split         hiding ( endBy )
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type Coordinates = (Int, Int)
data Fold = AlongX Int | AlongY Int

reflect :: Int -> Int -> Int
reflect a b = a - (2 * (a - b))

applyFold :: Fold -> [Coordinates] -> [Coordinates]
applyFold (AlongX x) = map (\(i, j) -> if i > x then (reflect i x, j) else (i, j))
applyFold (AlongY y) = map (\(i, j) -> if j > y then (i, reflect j y) else (i, j))

partOne :: ([Coordinates], [Fold]) -> String
partOne (coords, folds) = show . length . nub . applyFold (head folds) $ coords

-- Part Two

applyAllFolds :: [Fold] -> [Coordinates] -> [Coordinates]
applyAllFolds fs xs = nub $ foldl (flip applyFold) xs fs

getBounds :: [Coordinates] -> (Coordinates, Coordinates)
getBounds xs = ((0, 0), (maximum (map snd xs), maximum (map fst xs)))

mkArray :: [Coordinates] -> Array Coordinates Char
mkArray xs =
  let blankArray = listArray (getBounds xs) (repeat '.')
  in  blankArray // [ ((i, j), '#') | (j, i) <- xs ]

showArray :: Array Coordinates Char -> String
showArray arr = unlines . chunksOf (1 + snd (snd $ bounds arr)) . elems $ arr

partTwo :: ([Coordinates], [Fold]) -> String
partTwo (coords, folds) = showArray . mkArray . applyAllFolds folds $ coords

-- Parsing

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseCoordLine :: ReadP Coordinates
parseCoordLine = (,) <$> parseInt <* string "," <*> parseInt

parseXFold :: ReadP Fold
parseXFold = AlongX <$> (string "x=" *> parseInt)

parseYFold :: ReadP Fold
parseYFold = AlongY <$> (string "y=" *> parseInt)

parseFoldLine :: ReadP Fold
parseFoldLine = string "fold along " *> (parseXFold <|> parseYFold)

parseFile :: ReadP ([Coordinates], [Fold])
parseFile = do
  coords <- endBy parseCoordLine (string "\n")
  string "\n"
  folds <- endBy parseFoldLine (string "\n")
  eof
  return (coords, folds)

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> ([Coordinates], [Fold])
parseInput = runParser parseFile

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
  putStr . partTwo . parseInput $ input
