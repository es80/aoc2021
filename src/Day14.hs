import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type Template = String
type InsertionRule = (String, Char)

type PairsTally = Map String Integer
type InsertionRulesMap = Map String (String, String)

mkPairUpdatesMap :: [InsertionRule] -> InsertionRulesMap
mkPairUpdatesMap insertionRules =
  let rulesAsPairUpdates = map f insertionRules
      f (str, ch) = (str, (head str : [ch], ch : tail str))
  in  Map.fromList rulesAsPairUpdates

mkInitialTally :: Template -> PairsTally
mkInitialTally string = Map.fromListWith (+) (zip (mkPairs string) (repeat 1))
 where
  mkPairs [x] = []
  mkPairs xs  = take 2 xs : mkPairs (drop 1 xs)

takeStep :: InsertionRulesMap -> PairsTally -> PairsTally
takeStep rules = Map.foldrWithKey f Map.empty
 where
  f pair count tally =
    let (insert1, insert2) = rules Map.! pair
    in  Map.insertWith (+) insert1 count (Map.insertWith (+) insert2 count tally)

takeSteps :: Int -> InsertionRulesMap -> PairsTally -> PairsTally
takeSteps n rules tally = go n tally
 where
  go 0 tally = tally
  go n tally = go (n - 1) $ takeStep rules tally

finalElementTally :: Template -> PairsTally -> Map Char Integer
finalElementTally polymerTemplate pairsTally =
  let charTally = Map.mapKeysWith (+) last pairsTally
  in  Map.insertWith (+) (head polymerTemplate) 1 charTally

rangeOfTallys :: Map Char Integer -> Integer
rangeOfTallys charTally = let tallys = Map.elems charTally in maximum tallys - minimum tallys

getAnswer :: Int -> (Template, [InsertionRule]) -> String
getAnswer n (polymerTemplate, insertions) =
  let tally = takeSteps n (mkPairUpdatesMap insertions) (mkInitialTally polymerTemplate)
  in  show $ rangeOfTallys (finalElementTally polymerTemplate tally)

partOne :: (Template, [InsertionRule]) -> String
partOne = getAnswer 10

-- Part Two

partTwo :: (Template, [InsertionRule]) -> String
partTwo = getAnswer 40

-- Parsing

parseNewLine :: ReadP String
parseNewLine = string "\n"

parseUpper :: ReadP Char
parseUpper = satisfy isUpper

parseTemplate :: ReadP Template
parseTemplate = many parseUpper <* parseNewLine

parseInsertionRule :: ReadP InsertionRule
parseInsertionRule = (,) <$> count 2 parseUpper <* string " -> " <*> parseUpper

parseFile :: ReadP (Template, [InsertionRule])
parseFile = (,) <$> parseTemplate <* parseNewLine <*> endBy parseInsertionRule parseNewLine <* eof

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> (Template, [InsertionRule])
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
  putStrLn . partTwo . parseInput $ input
