import           Control.Applicative
import           Data.Char
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

data Command = Forward Int | Down Int | Up Int deriving Show

data Submarine = Submarine
  { horizontal :: Int
  , depth      :: Int
  }
  deriving Show

applyCommand :: Submarine -> Command -> Submarine
applyCommand submarine (Forward x) = submarine { horizontal = horizontal submarine + x }
applyCommand submarine (Down    x) = submarine { depth = depth submarine + x }
applyCommand submarine (Up      x) = submarine { depth = depth submarine - x }

applyCommands :: Submarine -> [Command] -> Submarine
applyCommands = foldl applyCommand

partOne :: [Command] -> String
partOne xs =
  let finalSubmarine = applyCommands (Submarine 0 0) xs
  in  show $ horizontal finalSubmarine * depth finalSubmarine

-- Part Two

data Submarine' = Submarine'
  { horizontal' :: Int
  , depth'      :: Int
  , aim'        :: Int
  }
  deriving Show

applyCommand' :: Submarine' -> Command -> Submarine'
applyCommand' submarine (Forward x) = submarine { horizontal' = horizontal' submarine + x
                                                , depth' = depth' submarine + (aim' submarine * x)
                                                }
applyCommand' submarine (Down x) = submarine { aim' = aim' submarine + x }
applyCommand' submarine (Up   x) = submarine { aim' = aim' submarine - x }

applyCommands' :: Submarine' -> [Command] -> Submarine'
applyCommands' = foldl applyCommand'

partTwo :: [Command] -> String
partTwo xs =
  let finalSubmarine = applyCommands' (Submarine' 0 0 0) xs
  in  show $ horizontal' finalSubmarine * depth' finalSubmarine

-- Parsing

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseForward :: ReadP Command
parseForward = Forward <$> (string "forward " *> parseInt <* eof)

parseDown :: ReadP Command
parseDown = Down <$> (string "down " *> parseInt <* eof)

parseUp :: ReadP Command
parseUp = Up <$> (string "up " *> parseInt <* eof)

parseLine :: ReadP Command
parseLine = parseForward <|> parseDown <|> parseUp

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> [Command]
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
