import           Data.Array
import           System.Environment
import           System.IO

-- Part One

getBounds :: [[Int]] -> ((Int, Int), (Int, Int))
getBounds nums = ((1, 1), (length nums, maximum $ map length nums))

type OctopusArray = Array (Int, Int) Int

mkOctopusArray :: [[Int]] -> OctopusArray
mkOctopusArray nums = listArray (getBounds nums) $ concat nums

adjacents :: (Int, Int) -> OctopusArray -> [(Int, Int)]
adjacents (i, j) arr =
  [ (k, l)
  | k <- [i - 1, i, i + 1]
  , l <- [j - 1, j, j + 1]
  , not (k == i && l == j)
  , k >= (fst . fst . bounds) arr
  , l >= (snd . fst . bounds) arr
  , k <= (fst . snd . bounds) arr
  , l <= (snd . snd . bounds) arr
  ]

countAndApplyFlashes :: OctopusArray -> (Int, OctopusArray)
countAndApplyFlashes arr = (length flashIdxs, newArr)
 where
  flashIdxs     = map fst $ filter (\(i, e) -> e > 9) (assocs arr)
  setFlashedTo0 = arr // [ (i, 0) | i <- flashIdxs ]
  countAdjacentFlashes idx = length $ filter (`elem` flashIdxs) (adjacents idx arr)
  updateEntry (idx, e) = if e == 0 then (idx, e) else (idx, e + countAdjacentFlashes idx)
  newArr = setFlashedTo0 // [ updateEntry (i, e) | (i, e) <- assocs setFlashedTo0 ]

applyStep :: OctopusArray -> (Int, OctopusArray)
applyStep arr = go 0 (fmap (+ 1) arr)
 where
  go count arr =
    let (count', arr') = countAndApplyFlashes arr
    in  if count' == 0 then (count, arr') else go (count + count') arr'

flashesFromNSteps :: Int -> OctopusArray -> Int
flashesFromNSteps steps arr = go steps 0 arr
 where
  go 0 count arr = count
  go n count arr = let (count', arr') = applyStep arr in go (n - 1) (count + count') arr'

partOne :: [[Int]] -> String
partOne = show . flashesFromNSteps 100 . mkOctopusArray

-- Part Two

stepsTo100Flashes :: Int -> OctopusArray -> Int
stepsTo100Flashes steps arr =
  let (count, arr') = applyStep arr
  in  if count == 100 then steps else stepsTo100Flashes (steps + 1) arr'

partTwo :: [[Int]] -> String
partTwo = show . stepsTo100Flashes 1 . mkOctopusArray

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
