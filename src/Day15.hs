import           Data.Array
import           Data.Heap                      ( Heap )
import qualified Data.Heap                     as Heap
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment
import           System.IO

-- Part One

type Location = (Int, Int)
type RiskMap = Array Location Int
type PathCosts = Map Location Int
type Vertex = Heap.Entry Int Location

mkRiskMap :: [[Int]] -> RiskMap
mkRiskMap nums = listArray getBounds (concat nums)
  where getBounds = ((1, 1), (length nums, length $ head nums))

initialPathCosts :: [[Int]] -> PathCosts
initialPathCosts nums =
  Map.fromList [ ((i, j), maxBound) | i <- [1 .. length nums], j <- [1 .. length (head nums)] ]

initialHeap :: [[Int]] -> Heap Vertex
initialHeap nums = Heap.singleton (Heap.Entry 0 (1, 1))

directions :: Location -> [Location]
directions (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

getNeighboursCosts :: Location -> PathCosts -> [(Location, Int)]
getNeighboursCosts location costs =
  let getCosts location = Map.lookup location costs >>= (\cost -> Just (location, cost))
  in  mapMaybe getCosts (directions location)

getCheaperCosts :: RiskMap -> Int -> (Location, Int) -> Maybe (Location, Int)
getCheaperCosts risks currentCost (loc, cst) =
  let newCost = currentCost + risks ! loc in if newCost < cst then Just (loc, newCost) else Nothing

dijkstra :: RiskMap -> Heap Vertex -> PathCosts -> PathCosts
dijkstra weights unvisited costs = if Heap.null unvisited
  then costs
  else
    let minVertex       = Heap.minimum unvisited
        currentLocation = Heap.payload minVertex
        currentCost     = Heap.priority minVertex
        neighbourCosts  = getNeighboursCosts currentLocation costs
        cheaperCosts    = mapMaybe (getCheaperCosts weights currentCost) neighbourCosts
        insertToHeap    = \(loc, cst) -> Heap.insert (Heap.Entry cst loc)
        newHeap         = foldr insertToHeap (Heap.deleteMin unvisited) cheaperCosts
        newCosts        = foldr (uncurry Map.insert) costs cheaperCosts
    in  dijkstra weights newHeap newCosts

getAnswer :: [[Int]] -> Int
getAnswer nums =
  last . Map.elems $ dijkstra (mkRiskMap nums) (initialHeap nums) (initialPathCosts nums)

partOne :: [[Int]] -> String
partOne = show . getAnswer

-- Part Two

transform :: [Int] -> Int -> [Int]
transform nums n = map (\num -> (num + n - 1) `mod` 9 + 1) nums

repeatCols :: [[Int]] -> [[Int]]
repeatCols = map (\nums -> concatMap (transform nums) [0 .. 4])

repeatRows :: [[Int]] -> [[Int]]
repeatRows nums = concatMap f [0 .. 4] where f n = map (`transform` n) nums

partTwo :: [[Int]] -> String
partTwo = partOne . repeatRows . repeatCols

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
