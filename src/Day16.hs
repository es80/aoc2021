import           Control.Applicative     hiding ( many )
import           Data.List
import           Numeric
import           System.Environment
import           System.IO
import           Text.ParserCombinators.ReadP

-- Part One

hexStringsToInts :: String -> [Int]
hexStringsToInts = map (fst . head . readHex . singleton)

intToBits :: Int -> String
intToBits x = pad $ go x
 where
  go 0 = "0"
  go 1 = "1"
  go n = let m = n `div` 2 in if even n then go m ++ "0" else go m ++ "1"
  pad xs = if length xs == 4 then xs else pad ('0' : xs)

bitsToNum :: [String] -> Int
bitsToNum xs = bitsToInt' (length xs - 1) (map read xs)
 where
  bitsToInt' pos []       = 0
  bitsToInt' pos (x : xs) = let rest = bitsToInt' (pos - 1) xs in 2 ^ pos * x + rest

data Packet a = Packet
  { version :: Int
  , typeId  :: Int
  , payload :: a
  }
  deriving Show

data PacketTree a = Leaf (Packet Int) | Node (Packet [PacketTree a]) deriving Show

parseBit :: ReadP String
parseBit = string "0" <|> string "1"

parsePacketLeaf :: ReadP (PacketTree a)
parsePacketLeaf = do
  version <- count 3 parseBit
  typeId  <- string "100"
  digits  <- many $ string "1" *> count 4 parseBit
  last    <- string "0" *> count 4 parseBit
  return (Leaf (Packet (bitsToNum version) 4 (bitsToNum $ concat $ digits ++ [last])))

parseOperator1 :: ReadP (PacketTree a)
parseOperator1 = do
  version      <- count 3 parseBit
  typeId       <- count 3 parseBit
  lengthTypeId <- string "0"
  totalLength  <- count 15 parseBit
  packets      <- count (bitsToNum totalLength) parseBit
  let runParser parser str = fst . head $ readP_to_S parser str
  let val = runParser (many parsePacket <* eof) (concat packets)
  return (Node (Packet (bitsToNum version) (bitsToNum typeId) val))

parseOperator2 :: ReadP (PacketTree a)
parseOperator2 = do
  version      <- count 3 parseBit
  typeId       <- count 3 parseBit
  lengthTypeId <- string "1"
  totalLength  <- count 11 parseBit
  packets      <- count (bitsToNum totalLength) parsePacket
  return (Node (Packet (bitsToNum version) (bitsToNum typeId) packets))

parsePacket :: ReadP (PacketTree a)
parsePacket = parsePacketLeaf <|> parseOperator1 <|> parseOperator2

mkPacketTree :: String -> PacketTree a
mkPacketTree s =
  let binaryString = concatMap intToBits (hexStringsToInts s)
  in  fst $ head $ readP_to_S parsePacket binaryString

sumVersions :: PacketTree a -> Int
sumVersions (Leaf (Packet version typeId num    )) = version
sumVersions (Node (Packet version typeId packets)) = version + sum (map sumVersions packets)

partOne :: String -> String
partOne a = show $ sumVersions $ mkPacketTree a

-- Part Two

eval :: PacketTree a -> Int
eval (Leaf (Packet version typeId num    )) = num
eval (Node (Packet version typeId packets)) = f typeId
 where
  f 0 = sum (map eval packets)
  f 1 = product (map eval packets)
  f 2 = minimum (map eval packets)
  f 3 = maximum (map eval packets)
  f 5 = let [x, y] = map eval packets in if x > y then 1 else 0
  f 6 = let [x, y] = map eval packets in if x < y then 1 else 0
  f 7 = let [x, y] = map eval packets in if x == y then 1 else 0

partTwo :: String -> String
partTwo a = show $ eval $ mkPacketTree a

-- Parsing

parseInput :: String -> String
parseInput xs = head $ lines xs

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
