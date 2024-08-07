{-# LANGUAGE InstanceSigs #-}
module Binary(
    Bin8,
    Bin16,
    extend8,
    trim8,
    combine
) where

import Data.List.NonEmpty(NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Default

newtype Bin8 = Bin8 Binary deriving Eq
newtype Bin16 = Bin16 Binary deriving Eq

extend8 :: Bin8 -> Bin16
extend8 (Bin8 b) = Bin16 $ resizeS 16 b

trim8 :: Bin16 -> Bin8
trim8 (Bin16 b) = Bin8 $ resizeS 8 b

combine :: Bin8 -> Bin8 -> Bin16
combine (Bin8 (Binary b)) (Bin8 (Binary b')) = Bin16 $ Binary (b <> b')

instance Num Bin8 where
    (+) :: Bin8 -> Bin8 -> Bin8
    (Bin8 b) + (Bin8 b') = Bin8 $ resizeS 8 (b !+ b')

    (*) :: Bin8 -> Bin8 -> Bin8
    (Bin8 b) * (Bin8 b') = Bin8 $ resizeS 8 (b !* b')

    negate :: Bin8 -> Bin8
    negate (Bin8 b) = Bin8 $ resizeS 8 (negBin b)

    abs :: Bin8 -> Bin8
    abs bin@(Bin8 b)
        | signB b = negate bin
        | otherwise = bin

    signum :: Bin8 -> Bin8
    signum (Bin8 b)
        | signB b = Bin8 (Binary $ True :| replicate 7 True)
        | otherwise = Bin8 (Binary $ True :| replicate 7 False)

    fromInteger :: Integer -> Bin8
    fromInteger n =
        case readNum n of
            Just bin -> Bin8 $ resizeS 8 bin
            Nothing -> error $ "couldnt read " ++ show n ++ " to bin8"

instance Ord Bin8 where
    compare :: Bin8 -> Bin8 -> Ordering
    (Bin8 b) `compare` (Bin8 b') = compare (toNumS b) (toNumS b')

instance Default Bin8 where
    def :: Bin8
    def = 0


instance Num Bin16 where
    (+) :: Bin16 -> Bin16 -> Bin16
    (Bin16 b) + (Bin16 b') = Bin16 $ resizeS 16 (b !+ b')

    (*) :: Bin16 -> Bin16 -> Bin16
    (Bin16 b) * (Bin16 b') = Bin16 $ resizeS 16 (b !* b')

    negate :: Bin16 -> Bin16
    negate (Bin16 b) = Bin16 $ resizeS 16 (negBin b)

    abs :: Bin16 -> Bin16
    abs bin@(Bin16 b)
        | signB b = negate bin
        | otherwise = bin

    signum :: Bin16 -> Bin16
    signum (Bin16 b)
        | signB b = Bin16 (Binary $ True :| replicate 15 True)
        | otherwise = Bin16 (Binary $ True :| replicate 15 False)

    fromInteger :: Integer -> Bin16
    fromInteger n =
        case readNum n of
            Just bin -> Bin16 $ resizeS 16 bin
            Nothing -> error $ "couldnt read " ++ show n ++ " to bin8"

instance Ord Bin16 where
    compare :: Bin16 -> Bin16 -> Ordering
    (Bin16 b) `compare` (Bin16 b') = compare (toNumS b) (toNumS b')

instance Default Bin16 where
    def :: Bin16
    def = 0

data BitSum = BitSum {
    sumB   :: Bool,
    carryB :: Bool
}
newtype Binary = Binary {getB :: NE.NonEmpty Bool} deriving (Eq, Show)

one :: Binary
one = Binary (True :| [False])

zero :: Binary
zero = Binary (False :| [False])


(!+) :: Binary -> Binary -> Binary
(!+) = addBinary
(!*) :: Binary -> Binary -> Binary
(!*) = multBinary
(!-) :: Binary -> Binary -> Binary
(!-) = addBinary . negBin
(!++) :: Binary -> Binary -> Binary
(!++) (Binary b1) (Binary b2) = Binary (b1 <> b2)

xor :: Bool -> Bool -> Bool
xor = (/=)

halfAddr :: Bool -> Bool -> BitSum
halfAddr b b' = BitSum {
    sumB   = b `xor` b',
    carryB = b && b'
}

fullAddr :: Bool -> Bool -> Bool -> BitSum
fullAddr b b' c = BitSum {
    sumB = b `xor`b' `xor` c,
    carryB = (b && b') || (c && (b `xor` b'))
}

negBin :: Binary -> Binary
negBin = (!+ one) . Binary . NE.map not . getB

addBit :: Bool -> [Bool] -> NE.NonEmpty Bool
addBit c [] = c :| []
addBit c (b : bs) =
    let
        s = halfAddr c b
    in
        sumB s <| addBit (carryB s) bs

signB :: Binary -> Bool
signB (Binary b) = NE.last b

addBinary :: Binary -> Binary -> Binary
addBinary (Binary b1) (Binary b2) = 
    Binary $ foldr f addBit b1 cin (NE.toList b2)
    where
        f :: Bool -> 
            (Bool -> [Bool] -> NE.NonEmpty Bool) ->
             Bool -> [Bool] -> NE.NonEmpty Bool
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

        cin :: Bool
        cin = False

multBinary :: Binary -> Binary -> Binary
multBinary (Binary b1) (Binary b2) = 
    (ssum . map (Binary . snd)) $ 
        NE.filter (\(b, _) -> b == True) 
            (NE.zip b1 $ NE.scanl (\bs _ -> False <| bs) b2 b1)
    where
        ssum = foldr (!+) (Binary (False :| []))

resizeS :: Int -> Binary -> Binary
resizeS n b@(Binary (b0 :| bs)) = Binary $ b0 :| resizeAux (n - 1) (signB b) bs
    where
        resizeAux :: Int -> a -> [a] -> [a]
        resizeAux n a as
            | n < length as = take n as
            | n > length as = as ++ replicate (n - length as) a
            | otherwise = as

readBinary :: String -> Maybe Binary
readBinary = fmap Binary . (>>= NE.nonEmpty) . traverse binOfChar
    where
        binOfChar :: Char -> Maybe Bool
        binOfChar '1' = Just True
        binOfChar '0' = Just False
        binOfChar _ = Nothing

readHex :: String -> Maybe Binary
readHex = fmap Binary . (>>= NE.nonEmpty) . fmap concat . traverse binOfHex
    where
        binOfHex :: Char -> Maybe [Bool]
        binOfHex '0' = Just [False, False, False, False]
        binOfHex '1' = Just [False, False, False, True]
        binOfHex '2' = Just [False, False, True, False]
        binOfHex '3' = Just [False, False, True, True]
        binOfHex '4' = Just [False, True, False, False]
        binOfHex '5' = Just [False, True, False, True]
        binOfHex '6' = Just [False, True, True, False]
        binOfHex '7' = Just [False, True, True, True]
        binOfHex '8' = Just [True, False, False, False]
        binOfHex '9' = Just [True, False, False, True]
        binOfHex 'a' = Just [True, False, True, False]
        binOfHex 'b' = Just [True, False, True, True]
        binOfHex 'c' = Just [True, True, False, False]
        binOfHex 'd' = Just [True, True, False, True]
        binOfHex 'e' = Just [True, True, True, False]
        binOfHex 'f' = Just [True, True, True, True]
        binOfHex _ = error "TODO: add support for sign extension"

readNum :: Integral a => a -> Maybe Binary
readNum a
    | a < 0 = negBin <$> readNum (negate a)
    | otherwise = 
        Binary . (False <|) <$> 
            (go a (reverse $ takeWhile (<= a) [i ^ 2 | i <- [1..]]) [] >>= NE.nonEmpty)  
    where
        go :: (Num a, Ord a) => a -> [a] -> [Bool] -> Maybe [Bool]
        go 0 [] bs = Just bs
        go _ [] _ = Nothing
        go a (v:vs) bs
            | a >= v = go (a - v) vs (True : bs)
            | otherwise = go a vs (False : bs)

showBin :: Binary -> String
showBin = foldl (\str b -> charOfBit b : str) [] . getB
    where
        charOfBit :: Bool -> Char
        charOfBit True = '1'
        charOfBit False = '0'


showHex :: Binary -> String
showHex = map (toHex . NE.toList . NE.reverse . NE.map snd) . NE.groupBy (flip ((==) . fst) . fst) . NE.zip groups . getB
    where
        groups :: NE.NonEmpty Int
        groups = NE.fromList $ concat [[i, i, i, i] | i <- [0..]]

        toHex :: [Bool] -> Char
        toHex [False, False, False, False] = '0'
        toHex [False, False, False, True] = '1'
        toHex [False, False, True, False] = '2'
        toHex [False, False, True, True] = '3'
        toHex [False, True, False, False] = '4'
        toHex [False, True, False, True] = '5'
        toHex [False, True, True, False] = '6'
        toHex [False, True, True, True] = '7'
        toHex [True, False, False, False] = '8'
        toHex [True, False, False, True] = '9'
        toHex [True, False, True, False] = 'a'
        toHex [True, False, True, True] = 'b'
        toHex [True, True, False, False] = 'c'
        toHex [True, True, False, True] = 'd'
        toHex [True, True, True, False] = 'e'
        toHex [True, True, True, True] = 'f'
        toHex _ = error "????"

toNum :: Integral a => Binary -> a
toNum = sum . NE.zipWith f digits . getB
    where
        f :: Num a => a -> Bool -> a
        f _ False = 0
        f i True = i

        digits :: Num a => NE.NonEmpty a
        digits = NE.fromList [2 ^ i | i <- [0..]]

toNumS :: Integral a => Binary -> a
toNumS b = 
    case signB b of
        True -> -(toNum (negBin b))
        False -> toNum b

showNum :: Binary -> String
showNum = show . toNumS
