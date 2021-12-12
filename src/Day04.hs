import           Control.Applicative     hiding ( optional )
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type Marked = Maybe Int
type Board = [[Marked]]
type Boards = [Board]

mkMarkedBoards :: [[[Int]]] -> Boards
mkMarkedBoards = map $ map $ map Just

rows :: Board -> [[Marked]]
rows = id

columns :: Board -> [[Marked]]
columns = transpose

addMark :: Int -> Marked -> Marked
addMark drawn Nothing  = Nothing
addMark drawn (Just n) = if n == drawn then Nothing else Just n

applyDrawnNum :: Int -> [Board] -> [Board]
applyDrawnNum drawn = map $ map $ map $ addMark drawn

completeLine :: [Marked] -> Bool
completeLine = all (== Nothing)

checkWinning :: Board -> Bool
checkWinning board = any completeLine (rows board) || any completeLine (columns board)

getSum :: Board -> Int
getSum board = sum $ catMaybes $ concat board

getSumOfWinning :: [Board] -> Maybe Int
getSumOfWinning boards =
  let winners = filter checkWinning boards in listToMaybe $ map getSum winners

playBingo :: [Int] -> Boards -> Int
playBingo [] boards = undefined
playBingo (drawn : restOfDraw) boards =
  let nextBoards = applyDrawnNum drawn boards
  in  case getSumOfWinning nextBoards of
        Nothing -> playBingo restOfDraw nextBoards
        Just n  -> n * drawn

partOne :: ([Int], [[[Int]]]) -> String
partOne (draw, boards) = show . playBingo draw . mkMarkedBoards $ boards

-- Part Two

filterOutWinnersAndFindSums :: [Board] -> ([Board], [Int])
filterOutWinnersAndFindSums boards =
  let (wining, notWining) = partition checkWinning boards in (notWining, map getSum wining)

playBingo' :: [Int] -> Boards -> Int
playBingo' [] boards = undefined
playBingo' (drawn : restOfDraw) boards =
  let newBoards = applyDrawnNum drawn boards
  in  case filterOutWinnersAndFindSums newBoards of
        ([]         , wins) -> drawn * last wins
        (stillBoards, _   ) -> playBingo' restOfDraw stillBoards

partTwo :: ([Int], [[[Int]]]) -> String
partTwo (draw, boards) = show . playBingo' draw . mkMarkedBoards $ boards

-- Parsing

parseNewLine :: ReadP String
parseNewLine = string "\n"

parseSpaces :: ReadP String
parseSpaces = string " " <|> string "  "

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseDraw :: ReadP [Int]
parseDraw = sepBy parseInt (string ",") <* parseNewLine

parseRow :: ReadP [Int]
parseRow = optional parseSpaces *> sepBy1 parseInt parseSpaces

parseBoard :: ReadP [[Int]]
parseBoard = endBy parseRow parseNewLine

parseBoards :: ReadP [[[Int]]]
parseBoards = sepBy parseBoard parseNewLine <* eof

parseContent :: ReadP ([Int], [[[Int]]])
parseContent = (,) <$> parseDraw <* parseNewLine <*> parseBoards

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> ([Int], [[[Int]]])
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
