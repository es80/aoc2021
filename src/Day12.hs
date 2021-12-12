import           Data.Bifunctor
import           Data.Char
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List
import           Data.List.Split
import           Data.Tuple
import           System.Environment
import           System.IO

-- Part One

type Cave = String
type SmallCaves = [Cave]
type Path = [Cave]
type AdjacencyList = Map Cave [Cave]

mkAdjacencyList :: [(Cave, Cave)] -> AdjacencyList
mkAdjacencyList pairs =
  let firstToSecond = map (second singleton) pairs
      secondToFirst = map (second singleton . swap) pairs
  in  Map.fromListWith (++) (firstToSecond ++ secondToFirst)

smallCaves :: AdjacencyList -> SmallCaves
smallCaves adjacencyList =
  let caves = filter (\c -> (c /= "start") && (c /= "end")) (Map.keys adjacencyList)
  in  filter (all isLower) caves

type PathExtender = AdjacencyList -> SmallCaves -> Path -> [Path]

extendPath :: PathExtender
extendPath adjacencyList smallCaves path =
  let avoid = "start" : path `intersect` smallCaves
  in  [ y : path | y <- adjacencyList Map.! head path, y `notElem` avoid ]

numPaths :: PathExtender -> AdjacencyList -> SmallCaves -> Path -> Int
numPaths pathExtender adjacencyList smallCaves path = if head path == "end"
  then 1
  else
    let extensions = pathExtender adjacencyList smallCaves path
    in  sum $ map (numPaths pathExtender adjacencyList smallCaves) extensions

getAnswer :: PathExtender -> [(Cave, Cave)] -> String
getAnswer pathExtender input =
  let adjacencyList = mkAdjacencyList input
  in  show $ numPaths pathExtender adjacencyList (smallCaves adjacencyList) ["start"]

partOne :: [(Cave, Cave)] -> String
partOne = getAnswer extendPath

-- Part Two

extendPath' :: PathExtender
extendPath' adjacencyList smallCaves path =
  let smallCavesInPath  = path `intersect` smallCaves
      smallVisitedTwice = smallCavesInPath \\ smallCaves
      avoid             = "start" : if null smallVisitedTwice then [] else smallCavesInPath
  in  [ y : path | y <- adjacencyList Map.! head path, y `notElem` avoid ]

partTwo :: [(Cave, Cave)] -> String
partTwo = getAnswer extendPath'

-- Parsing

parseLine :: String -> (Cave, Cave)
parseLine str = let caves = splitOn "-" str in (head caves, last caves)

parseInput :: String -> [(Cave, Cave)]
parseInput = map parseLine . lines

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
