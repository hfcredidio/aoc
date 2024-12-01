module Day16 where

import Control.Monad

data Bit = B0 | B1
instance Show Bit where
    show B0 = "0"
    show B1 = "1"

type Bits = [Bit]

data Op = Sum | Product | Minimum | Maximum | Gt | Lt | Equals deriving(Show)

opFromInt :: Int -> Op
opFromInt 0 = Sum
opFromInt 1 = Product
opFromInt 2 = Minimum
opFromInt 3 = Maximum
opFromInt 5 = Gt
opFromInt 6 = Lt
opFromInt 7 = Equals
opFromInt i = error $ "Invalid operation id " ++ show i

opFromBits = opFromInt . toInt


data Packet = Operator { op :: Op, version :: Int, subPackets :: [Packet] }
            | Literal  { version :: Int, value :: Int }
            deriving(Show)


parseBinPacket :: Bits -> Either String (Packet, Bits)
parseBinPacket bits | length bits < 6   = Left $ "parseBinPacket: too few bits" ++ show bits
parseBinPacket (v0:v1:v2:B1:B0:B0:bits) = parseLiteral  (v0:v1:v2:bits)
parseBinPacket (v0:v1:v2:o0:o1:o2:bits) = parseOperator (opFromBits [o0, o1, o2]) (v0:v1:v2:bits)
parseBinPacket bits                     = Left $ "parseBinPacket: weird pattern " ++ show bits


toInt :: Bits -> Int
toInt = toInt' 0
    where toInt' x []        = x
          toInt' x (B0:bits) = toInt' (x * 2) bits
          toInt' x (B1:bits) = toInt' (x * 2 + 1) bits


splitAsInt :: Int -> Bits -> (Int, Bits)
splitAsInt i bits = let (a, b) = splitAt i bits
                  in (toInt a, b)


parseLiteralValue' :: Bits -> Bits -> Either String (Int, Bits)
parseLiteralValue' _ [] = Left "parseLiteralValue': No value to parse"
parseLiteralValue' partial (b:bits)
    | length bits < 4 = Left $ "parseLiteralValue': to few bits " ++ show bits
    | otherwise       = let (h, rest) = splitAt 4 bits in case b of
      B1 -> parseLiteralValue' (partial ++ h) rest
      B0 -> Right (toInt $ partial ++ h, rest)
parseLiteralValue = parseLiteralValue' []


parseLiteral :: Bits -> Either String (Packet, Bits)
parseLiteral bits
  | length bits < 3 = Left $ "parseLiteral: too few bits " ++ show bits
  | otherwise       = do
      let (version, rest) = splitAsInt 3 bits
      (value, rest') <- parseLiteralValue rest
      return (Literal version value, rest')


parseOperator :: Op -> Bits -> Either String (Packet, Bits)
parseOperator op bits
  | length bits < 7 = Left $ "parseOperator: too few bits " ++ show bits
  | otherwise       = let (version, dig:rest) = splitAsInt 3 bits in case dig of
        B0 -> if length rest < 15 
              then Left $ "parseOperator: too few bits for size " ++ show bits
              else do let (size, rest') = splitAt 15 rest
                      (subpack, rest'') <- parseOperatorBySize (toInt size) rest'
                      return (Operator op version subpack, rest'')

        B1 -> if length rest < 11 
              then Left $ "parseOperator: too few bits for count " ++ show bits
              else do let (count, rest') = splitAsInt 11 rest
                      (subpack, rest'') <- parseOperatorByCount count rest'
                      return (Operator op version subpack, rest'')


parseOperatorBySize :: Int -> Bits -> Either String ([Packet], Bits)
parseOperatorBySize 0    bits = Right([], bits)
parseOperatorBySize size bits
    | length bits < size = Left $ "parseOperatorBySize: invalid size " ++ show size ++ show bits
    | size < 0           = Left $ "parseOperatorBySize: negtive size found" ++ show bits
    | otherwise          = do
        (packet, rest) <- parseBinPacket bits
        let newSize = size - (length bits - length rest)
        (packets, rest') <- parseOperatorBySize newSize rest
        return (packet:packets, rest')


parseOperatorByCount :: Int -> [Bit] -> Either String ([Packet], Bits)
parseOperatorByCount 0     bits = Right ([], bits)
parseOperatorByCount count bits = do
    (packet, rest) <- parseBinPacket bits
    (packets, rest') <- parseOperatorByCount (count - 1) rest
    return (packet:packets, rest')


toBit :: Char -> Bit
toBit '0' = B0
toBit '1' = B1
toBit _ = error "Invalid digit"


hexToBits :: Char -> Bits
hexToBits '0' = map toBit "0000"
hexToBits '1' = map toBit "0001"
hexToBits '2' = map toBit "0010"
hexToBits '3' = map toBit "0011"
hexToBits '4' = map toBit "0100"
hexToBits '5' = map toBit "0101"
hexToBits '6' = map toBit "0110"
hexToBits '7' = map toBit "0111"
hexToBits '8' = map toBit "1000"
hexToBits '9' = map toBit "1001"
hexToBits 'A' = map toBit "1010"
hexToBits 'B' = map toBit "1011"
hexToBits 'C' = map toBit "1100"
hexToBits 'D' = map toBit "1101"
hexToBits 'E' = map toBit "1110"
hexToBits 'F' = map toBit "1111"
hexToBits _   = error "Invalid digit"


sumVersions :: Packet -> Int
sumVersions (Literal version _) = version
sumVersions (Operator _ version packs) = version + sum (map sumVersions packs)


evalPacket :: Packet -> Either String Int
evalPacket (Literal _ value) = Right value
evalPacket (Operator Sum     _ packets)  = sum <$!> mapM evalPacket packets
evalPacket (Operator Product _ packets)  = product <$!> mapM evalPacket packets
evalPacket (Operator Minimum _ packets)  = minimum <$!> mapM evalPacket packets
evalPacket (Operator Maximum _ packets)  = maximum <$!> mapM evalPacket packets
evalPacket (Operator Gt      _ [p0, p1]) = do v0 <- evalPacket p0
                                              v1 <- evalPacket p1
                                              return $ if v0 > v1 then 1 else 0
evalPacket (Operator Lt      _ [p0, p1]) = do v0 <- evalPacket p0
                                              v1 <- evalPacket p1
                                              return $ if v0 < v1 then 1 else 0
evalPacket (Operator Equals  _ [p0, p1]) = do v0 <- evalPacket p0
                                              v1 <- evalPacket p1
                                              return $ if v0 == v1 then 1 else 0
evalPacket (Operator op _ packets) = Left $ "evalPacket: Malformed Packet " ++ show op ++ " has " ++ show (length packets) ++ " subpackets "


main :: IO ()
main = do
    bits <- concatMap hexToBits . head . lines <$!> readFile "data/day16.txt"
    let packet = fst <$> parseBinPacket bits
    print $ sumVersions <$> packet
    print $ evalPacket =<< packet
