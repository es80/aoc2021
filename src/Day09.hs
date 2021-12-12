import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           System.Environment
import           System.IO

-- Part One

type Row = Int
type Col = Int
type Location = (Row, Col)
type Height = Int
type HeightMap = Map Location Height

mkHeightMap :: [[Int]] -> HeightMap
mkHeightMap nums =
  let indexes = [ (i, j) | i <- [1 .. length nums], j <- [1 .. (maximum (map length nums))] ]
  in  Map.fromList $ zip indexes (concat nums)

adjacentLocations :: Location -> [Location]
adjacentLocations (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

getAdjacents :: HeightMap -> Location -> [(Location, Height)]
getAdjacents heightMap location = mapMaybe f $ adjacentLocations location
  where f location = Map.lookup location heightMap >>= (\height -> Just (location, height))

isLowPoint :: HeightMap -> Location -> Height -> Bool
isLowPoint heightMap location height = all ((> height) . snd) (getAdjacents heightMap location)

findLowPoints :: HeightMap -> [(Location, Height)]
findLowPoints heightMap = Map.foldrWithKey f [] heightMap
 where
  f location height acc =
    if isLowPoint heightMap location height then (location, height) : acc else acc

risks :: [(Location, Int)] -> [Int]
risks = map (\(location, height) -> 1 + height)

partOne :: [[Int]] -> String
partOne = show . sum . risks . findLowPoints . mkHeightMap

-- Part Two

getHigherAdjacents :: HeightMap -> (Location, Int) -> [(Location, Int)]
getHigherAdjacents heightMap (location, height) = filter f (getAdjacents heightMap location)
  where f (_, h) = h /= 9 && h > height

getBasin :: HeightMap -> (Location, Int) -> Set (Location, Int)
getBasin heightMap x = go Set.empty [x]
 where
  go set [] = set
  go set (x : xs) =
    let higher  = getHigherAdjacents heightMap x
        notSeen = filter (\x -> not $ Set.member x set) higher
    in  go (Set.insert x set) $ xs ++ notSeen

getBasins :: HeightMap -> [[(Location, Int)]]
getBasins heightMap = map (Set.toList . getBasin heightMap) (findLowPoints heightMap)

productThreeLargest :: [Int] -> Int
productThreeLargest = product . take 3 . reverse . sort

partTwo :: [[Int]] -> String
partTwo = show . productThreeLargest . map length . getBasins . mkHeightMap

-- Parsing

parseInput :: String -> [[Int]]
parseInput = map (map (\x -> read [x])) . lines

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
