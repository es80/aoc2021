import           Data.Char
import           Data.Maybe
import           Data.Tuple
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

type Range = (Int, Int)
type Target = (Range, Range)
type Path = [(Int, Int)]

xInitialVelocities :: Range -> [Int]
xInitialVelocities (leftEdge, rightEdge) = filter f [0 .. rightEdge]
  where f x = sum [1 .. x] >= leftEdge

yInitialVelocities :: Range -> [Int]
yInitialVelocities (bottomEdge, _topEdge) = [bottomEdge .. (negate bottomEdge)]

velocitiesToConsider :: Target -> [(Int, Int)]
velocitiesToConsider (xTarget, yTarget) =
  [ (x, y) | x <- xInitialVelocities xTarget, y <- yInitialVelocities yTarget ]

xPositions :: Int -> [Int]
xPositions vel = scanl1 (+) velocities
  where velocities = 0 : [ f x | x <- [vel, vel - 1 ..], let f x = if x > 0 then x else 0 ]

yPositions :: Int -> [Int]
yPositions vel = scanl1 (+) velocities where velocities = 0 : [vel, vel - 1 ..]

pathOfSuccessfulProbe :: Target -> (Int, Int) -> Maybe Path
pathOfSuccessfulProbe ((leftEdge, rightEdge), (bottomEdge, topEdge)) (xVelocity, yVelocity) =
  let pathToTarget     = takeWhile hasNotPassed $ zip (xPositions xVelocity) (yPositions yVelocity)
      hasNotPassed     = \(x, y) -> x <= rightEdge && y >= bottomEdge
      hitsTarget       = dropWhile notReachedTarget pathToTarget
      notReachedTarget = \(x, y) -> x < leftEdge || y > topEdge
  in  if not . null $ hitsTarget then Just pathToTarget else Nothing

allSuccessfulProbes :: Target -> [Path]
allSuccessfulProbes target = mapMaybe (pathOfSuccessfulProbe target) (velocitiesToConsider target)

partOne :: Target -> String
partOne target = show $ maximum (map snd $ concat $ allSuccessfulProbes target)

-- Part Two

partTwo :: Target -> String
partTwo target = show $ length $ allSuccessfulProbes target

-- Parsing

parseInt :: ReadP Int
parseInt = do
  minus  <- option "+" (string "-")
  digits <- many1 (satisfy isDigit)
  return . (if minus == "-" then negate else id) . read $ digits

parseIntRange :: String -> String -> ReadP (Int, Int)
parseIntRange prefix suffix =
  (,) <$> (string prefix *> parseInt) <* string ".." <*> parseInt <* string suffix

parseTarget :: ReadP Target
parseTarget = (,) <$> parseIntRange "target area: x=" ", " <*> parseIntRange "y=" "\n"

runParser :: ReadP a -> String -> a
runParser parser string = case readP_to_S parser string of
  [(a, "")] -> a
  _         -> error $ "Parsing failed: " ++ string

parseInput :: String -> Target
parseInput = runParser parseTarget

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
