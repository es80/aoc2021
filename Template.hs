import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

partOne :: a -> String
partOne _a = "TODO"

-- Part Two

partTwo :: a -> String
partTwo _a = "TODO"

-- Parsing

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> a
parseInput = runParser pfail

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
