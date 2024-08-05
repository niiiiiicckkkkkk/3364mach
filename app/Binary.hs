module Binary where

import Data.List.NonEmpty(NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

data Bit = Hi | Lo deriving (Eq , Show)
data BitSum = BitSum {
    sumB   :: Bit,
    carryB :: Bit
}
newtype Binary = Binary {getB :: NE.NonEmpty Bit} deriving Show

one :: Binary
one = Binary (Hi :| [Lo])

zero :: Binary
zero = Binary (Lo :| [Lo])


(!+) :: Binary -> Binary -> Binary
(!+) = addBinary
(!*) :: Binary -> Binary -> Binary
(!*) = multBinary
(!-) :: Binary -> Binary -> Binary
(!-) = addBinary . negBin
(!++) :: Binary -> Binary -> Binary
(!++) (Binary b1) (Binary b2) = Binary (b1 <> b2)

xorBit :: Bit -> Bit -> Bit
xorBit Lo Hi = Hi
xorBit Hi Lo = Hi
xorBit _ _ = Lo

andBit :: Bit -> Bit -> Bit
andBit Hi Hi = Hi
andBit _ _ = Lo

negBit :: Bit -> Bit
negBit Lo = Hi
negBit Hi = Lo

orBit :: Bit -> Bit -> Bit
orBit Lo Lo = Lo
orBit _ _ = Hi

halfAddr :: Bit -> Bit -> BitSum
halfAddr b b' = BitSum {
    sumB   = b `xorBit` b',
    carryB = b `andBit` b'
}

fullAddr :: Bit -> Bit -> Bit -> BitSum
fullAddr b b' c = BitSum {
    sumB = b `xorBit` b' `xorBit` c,
    carryB = (b `andBit` b') `orBit` (c `andBit` (b `xorBit` b'))
}

negBin :: Binary -> Binary
negBin = (!+ one) . Binary . NE.map negBit . getB

addBit :: Bit -> [Bit] -> NE.NonEmpty Bit
addBit c [] = c :| []
addBit c (b : bs) =
    let
        s = halfAddr c b
    in
        sumB s <| addBit (carryB s) bs

signB :: Binary -> Bit
signB (Binary b) = NE.last b

addBinary :: Binary -> Binary -> Binary
addBinary (Binary b1) (Binary b2) = 
    Binary $ foldr f addBit b1 cin (NE.toList b2)
    where
        f :: Bit -> 
            (Bit -> [Bit] -> NE.NonEmpty Bit) ->
             Bit -> [Bit] -> NE.NonEmpty Bit
        f b g c [] =
            let 
                s = halfAddr b c
            in
                sumB s <| g (carryB s) []
        f b g c (b' : bs) =
            let
                s = fullAddr b b' c
            in
                sumB s <| g (carryB s) bs

        cin :: Bit
        cin = Lo

multBinary :: Binary -> Binary -> Binary
multBinary (Binary b1) (Binary b2) = 
    (ssum . map (Binary . snd)) $ 
        NE.filter (\(b, _) -> b == Hi) 
            (NE.zip b1 $ NE.scanl (\bs _ -> Lo <| bs) b2 b1)
    where
        ssum = foldr (!+) (Binary (Lo :| []))

resizeS :: Num a => a -> Binary -> Binary
resizeS = undefined

readBinary :: String -> Maybe Binary
readBinary = fmap Binary . (>>= NE.nonEmpty) . traverse binOfChar
    where
        binOfChar :: Char -> Maybe Bit
        binOfChar '1' = Just Hi
        binOfChar '0' = Just Lo
        binOfChar _ = Nothing

readHex :: String -> Maybe Binary
readHex = fmap Binary . (>>= NE.nonEmpty) . fmap concat . traverse binOfHex
    where
        binOfHex :: Char -> Maybe [Bit]
        binOfHex '0' = Just [Lo, Lo, Lo, Lo]
        binOfHex '1' = Just [Lo, Lo, Lo, Hi]
        binOfHex '2' = Just [Lo, Lo, Hi, Lo]
        binOfHex '3' = Just [Lo, Lo, Hi, Hi]
        binOfHex '4' = Just [Lo, Hi, Lo, Lo]
        binOfHex '5' = Just [Lo, Hi, Lo, Hi]
        binOfHex '6' = Just [Lo, Hi, Hi, Lo]
        binOfHex '7' = Just [Lo, Hi, Hi, Hi]
        binOfHex '8' = Just [Hi, Lo, Lo, Lo]
        binOfHex '9' = Just [Hi, Lo, Lo, Hi]
        binOfHex 'a' = Just [Hi, Lo, Hi, Lo]
        binOfHex 'b' = Just [Hi, Lo, Hi, Hi]
        binOfHex 'c' = Just [Hi, Hi, Lo, Lo]
        binOfHex 'd' = Just [Hi, Hi, Lo, Hi]
        binOfHex 'e' = Just [Hi, Hi, Hi, Lo]
        binOfHex 'f' = Just [Hi, Hi, Hi, Hi]
        binOfHex _ = error "TODO: add support for sign extension"

readNum :: Integral a => a -> Maybe Binary
readNum a
    | a < 0 = negBin <$> readNum (negate a)
    | otherwise = 
        fmap Binary $ 
            go a (reverse $ takeWhile (<= a) [i ^ 2 | i <- [1..]]) [] >>= NE.nonEmpty . (:) Lo
    where
        go :: (Num a, Ord a) => a -> [a] -> [Bit] -> Maybe [Bit]
        go 0 [] bs = Just bs
        go _ [] _ = Nothing
        go a (v:vs) bs
            | a >= v = go (a - v) vs (Hi : bs)
            | otherwise = go a vs (Lo : bs)

showBin :: Binary -> String
showBin = foldl (\str b -> charOfBit b : str) [] . getB
    where
        charOfBit :: Bit -> Char
        charOfBit Hi = '1'
        charOfBit Lo = '0'


showHex :: Binary -> String
showHex = map (toHex . NE.toList . NE.reverse . NE.map snd) . NE.groupBy (flip ((==) . fst) . fst) . NE.zip groups . getB
    where
        groups :: NE.NonEmpty Int
        groups = NE.fromList $ concat [[i, i, i, i] | i <- [0..]]

        toHex :: [Bit] -> Char
        toHex [Lo, Lo, Lo, Lo] = '0'
        toHex [Lo, Lo, Lo, Hi] = '1'
        toHex [Lo, Lo, Hi, Lo] = '2'
        toHex [Lo, Lo, Hi, Hi] = '3'
        toHex [Lo, Hi, Lo, Lo] = '4'
        toHex [Lo, Hi, Lo, Hi] = '5'
        toHex [Lo, Hi, Hi, Lo] = '6'
        toHex [Lo, Hi, Hi, Hi] = '7'
        toHex [Hi, Lo, Lo, Lo] = '8'
        toHex [Hi, Lo, Lo, Hi] = '9'
        toHex [Hi, Lo, Hi, Lo] = 'a'
        toHex [Hi, Lo, Hi, Hi] = 'b'
        toHex [Hi, Hi, Lo, Lo] = 'c'
        toHex [Hi, Hi, Lo, Hi] = 'd'
        toHex [Hi, Hi, Hi, Lo] = 'e'
        toHex [Hi, Hi, Hi, Hi] = 'f'
        toHex _ = error "????"

toNum :: Integral a => Binary -> a
toNum = sum . NE.zipWith f digits . getB
    where
        f :: Num a => a -> Bit -> a
        f _ Lo = 0
        f i Hi = i

        digits :: Num a => NE.NonEmpty a
        digits = NE.fromList [2 ^ i | i <- [0..]]

toNumS :: Integral a => Binary -> a
toNumS b = 
    case signB b of
        Hi -> -(toNum (negBin b))
        Lo -> toNum b

showNum :: Binary -> String
showNum = show . toNumS
