{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary(
    BinVal,
    Debug(..),
    reduce,
    extend,
    binConv8,
    binConv16
) where

import Data.Word
import Data.Bits
import Data.Ix
import Data.Monoid
import Data.Proxy
import GHC.TypeLits

-- Binary Kinds hold information about their length
type Size = Nat

data BinVal (sz :: Size) = 
    forall a. (Show a, Integral a, Num a, Ord a, Ix a, FiniteBits a) 
    => BinVal a

instance Show (BinVal s) where
    show (BinVal b) =  show b

-- newtype storing destination size in phantom type
newtype BinConv (sz :: Size) = BinConv (forall a. Integral a => a -> BinVal sz)

binConv8 :: BinConv 8
binConv8 = BinConv (BinVal @8 . fromIntegral @_ @Word8)

binConv16 :: BinConv 16
binConv16 = BinConv (BinVal @16 . fromIntegral @_ @Word16)
            
-- convert from "larger" binary values to "smaller"
reduce :: forall srcSz dstSz lo hi. (
          KnownNat srcSz,
          KnownNat dstSz,
          KnownNat lo,
          KnownNat hi,
          hi - lo + 1 <= dstSz, 
          hi <= srcSz,
          0 <= lo) => 
          BinVal srcSz -> Proxy lo -> Proxy hi -> BinConv dstSz -> BinVal dstSz
reduce (BinVal b) l h (BinConv f) =
    let 
        digits = [2 ^ i | i <- [0..]]
        mask = [ testBit b i | i <- [lo .. hi]]
        res = (foldMap (Sum . snd). filter fst) (zip mask digits)
    in
        f (getSum res)

    where
        lo = fromIntegral (natVal l)
        hi = fromIntegral (natVal h)

-- extend a "smaller" BinVal to a "larger" one
extend :: forall srcSz dstSz. (
          KnownNat srcSz,
          KnownNat dstSz,
          (srcSz <=? dstSz) ~ 'True) => 
          BinVal srcSz ->
          BinConv dstSz -> BinVal dstSz
extend (BinVal b) (BinConv f) = f b

-- Binary values should implement these type classes for simulation
class (Num a, Ord a, Integral a, FiniteBits a) => Binary a
instance Binary (BinVal 8)
instance Binary (BinVal 16)

-- Debug values should implement some methods for display
class (Binary a) => Debug a where
    showHex :: a -> String
    showNumS :: a -> String
    showNumU :: a -> String
    showBin :: a -> String

showBinB :: forall sz . ((KnownNat sz), Bits (BinVal sz)) => BinVal sz -> String
showBinB b = 
    let len = fromIntegral (natVal b - 1) in
        map charOfBool $ [testBit b i | i <- [0..len]]
    where
        charOfBool :: Bool -> Char
        charOfBool True = '1'
        charOfBool False = '0'

showHexB :: forall sz . BinVal sz -> String
showHexB = error "TODO"

showNumSB :: forall sz . (KnownNat sz, Bits (BinVal sz), Num (BinVal sz), Integral (BinVal sz)) => BinVal sz -> String
showNumSB b
    | testBit b (fromInteger $ natVal b - 1) = show $ -1 * (toInteger (complement b + 1))
    | otherwise = show $ toInteger b

showNumUB :: forall sz . Integral (BinVal sz) => BinVal sz -> String
showNumUB = show . toInteger

instance Debug (BinVal 8) where
    showHex :: BinVal 8 -> String
    showHex = undefined

    showBin :: BinVal 8 -> String
    showBin = showBinB

    showNumU :: BinVal 8 -> String
    showNumU = showNumUB

    showNumS :: BinVal 8 -> String
    showNumS = showNumSB

instance Debug (BinVal 16) where
    showHex :: BinVal 16 -> String
    showHex = undefined

    showBin :: BinVal 16 -> String
    showBin = showBinB

    showNumU :: BinVal 16 -> String
    showNumU = showNumUB

    showNumS :: BinVal 16 -> String
    showNumS = showNumSB

-- threading Num, Integer, Rational, Ord etc instances through BinVal newtype
instance Num (BinVal 8) where
    (+) :: BinVal 8 -> BinVal 8 -> BinVal 8
    (+) = plusB

    (*) :: BinVal 8 -> BinVal 8 -> BinVal 8
    (*) = mulB

    negate :: BinVal 8 -> BinVal 8
    negate = negB

    abs :: BinVal 8 -> BinVal 8
    abs = absB

    signum :: BinVal 8 -> BinVal 8
    signum = sigB

    fromInteger :: Integer -> BinVal 8
    fromInteger a = BinVal (fromInteger @Word8 a)

instance Eq (BinVal 8) where
    (==) :: BinVal 8 -> BinVal 8 -> Bool
    (==) = eqB

instance Ord (BinVal 8) where
    compare :: BinVal 8 -> BinVal 8 -> Ordering
    compare = cmpB

instance Enum (BinVal 8) where
    toEnum :: Int -> BinVal 8
    toEnum n = BinVal (toEnum @Word8 n)
    
    fromEnum :: BinVal 8 -> Int
    fromEnum = fromEnumB

instance Integral (BinVal 8) where
    quotRem :: BinVal 8 -> BinVal 8 -> (BinVal 8, BinVal 8)
    quotRem = quotRemB

    toInteger :: BinVal 8 -> Integer
    toInteger = toIntegerB

instance Real (BinVal 8) where
    toRational :: BinVal 8 -> Rational
    toRational = toRationalB

instance Bits (BinVal 8) where
    (.&.) :: BinVal 8 -> BinVal 8 -> BinVal 8
    (.&.) = andB

    (.|.) :: BinVal 8 -> BinVal 8 -> BinVal 8
    (.|.) = orB

    xor :: BinVal 8 -> BinVal 8 -> BinVal 8
    xor = xorB

    complement :: BinVal 8 -> BinVal 8
    complement = complementB

    shift :: BinVal 8 -> Int -> BinVal 8
    shift = shiftB

    rotate :: BinVal 8 -> Int -> BinVal 8
    rotate = rotateB

    bitSize :: BinVal 8 -> Int
    bitSize = bitSizeB

    bitSizeMaybe :: BinVal 8 -> Maybe Int
    bitSizeMaybe = bitSizeMaybeB

    isSigned :: BinVal 8 -> Bool
    isSigned = isSignedB

    testBit :: BinVal 8 -> Int -> Bool
    testBit = testBitB

    bit :: Int -> BinVal 8
    bit n = BinVal $ bit @Word8 n

    popCount :: BinVal 8 -> Int
    popCount = popCountB

instance FiniteBits (BinVal 8) where
    finiteBitSize :: BinVal 8 -> Int
    finiteBitSize = sizeB

instance Ix (BinVal 8) where
    range :: (BinVal 8, BinVal 8) -> [BinVal 8]
    range (BinVal b, BinVal b') = BinVal <$> range (b, fromIntegral (toInteger b'))

    index :: (BinVal 8, BinVal 8) -> BinVal 8 -> Int
    index (BinVal b, BinVal b') (BinVal i) =
        let
            c x = fromIntegral (toInteger x)
        in
            index (b, c b') (c i)

    inRange :: (BinVal 8, BinVal 8) -> BinVal 8 -> Bool
    inRange (BinVal b, BinVal b') (BinVal i) =
        let
            c x = fromIntegral (toInteger x)
        in
            inRange (b, c b') (c i)

instance Num (BinVal 16) where
    (+) :: BinVal 16 -> BinVal 16 -> BinVal 16
    (+) = plusB

    (*) :: BinVal 16 -> BinVal 16 -> BinVal 16
    (*) = mulB

    negate :: BinVal 16 -> BinVal 16
    negate = negB

    abs :: BinVal 16 -> BinVal 16
    abs = absB

    signum :: BinVal 16 -> BinVal 16
    signum = sigB

    fromInteger :: Integer -> BinVal 16
    fromInteger a = BinVal (fromInteger @Word16 a)

instance Eq (BinVal 16) where
    (==) :: BinVal 16 -> BinVal 16 -> Bool
    (==) = eqB

instance Ord (BinVal 16) where
    compare :: BinVal 16 -> BinVal 16 -> Ordering
    compare = cmpB

instance Enum (BinVal 16) where
    toEnum :: Int -> BinVal 16
    toEnum n = BinVal (toEnum @Word16 n)
    
    fromEnum :: BinVal 16 -> Int
    fromEnum = fromEnumB

instance Real (BinVal 16) where
    toRational :: BinVal 16 -> Rational
    toRational = toRationalB

instance Integral (BinVal 16) where
    quotRem :: BinVal 16 -> BinVal 16 -> (BinVal 16, BinVal 16)
    quotRem = quotRemB

    toInteger :: BinVal 16 -> Integer
    toInteger = toIntegerB

instance Bits (BinVal 16) where
    (.&.) :: BinVal 16 -> BinVal 16 -> BinVal 16
    (.&.) = andB

    (.|.) :: BinVal 16 -> BinVal 16 -> BinVal 16
    (.|.) = orB

    xor :: BinVal 16 -> BinVal 16 -> BinVal 16
    xor = xorB

    complement :: BinVal 16 -> BinVal 16
    complement = complementB

    shift :: BinVal 16 -> Int -> BinVal 16
    shift = shiftB

    rotate :: BinVal 16 -> Int -> BinVal 16
    rotate = rotateB

    bitSize :: BinVal 16 -> Int
    bitSize = bitSizeB

    bitSizeMaybe :: BinVal 16 -> Maybe Int
    bitSizeMaybe = bitSizeMaybeB

    isSigned :: BinVal 16 -> Bool
    isSigned = isSignedB

    testBit :: BinVal 16 -> Int -> Bool
    testBit = testBitB

    bit :: Int -> BinVal 16
    bit n = BinVal $ bit @Word16 n

    popCount :: BinVal 16 -> Int
    popCount = popCountB

instance FiniteBits (BinVal 16) where
    finiteBitSize :: BinVal 16 -> Int
    finiteBitSize = sizeB

plusB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> BinVal s1
(BinVal b1) `plusB` (BinVal b2) = 
    let k = b1 + fromIntegral (toInteger b2) in BinVal k
mulB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> BinVal s1
(BinVal b1) `mulB` (BinVal b2) =
    let k = b1 * fromIntegral (toInteger b2) in BinVal k
negB :: BinVal sz -> BinVal sz
negB (BinVal b) = BinVal (negate b)
absB :: BinVal sz -> BinVal sz
absB (BinVal b) = BinVal (abs b)
sigB :: BinVal sz -> BinVal sz
sigB (BinVal b) = BinVal (signum b)
eqB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> Bool
eqB (BinVal b1) (BinVal b2) = b1 == fromIntegral (toInteger b2)
cmpB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> Ordering
cmpB (BinVal b1) (BinVal b2) = b1 `compare` fromIntegral (toInteger b2)
fromEnumB :: forall sz. BinVal sz -> Int
fromEnumB (BinVal b) = fromEnum b
toRationalB :: forall sz. BinVal sz -> Rational
toRationalB (BinVal b) = toRational b
quotRemB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> (BinVal s1, BinVal s1)
(BinVal b1) `quotRemB` (BinVal b2) = 
    let (k, k') = b1 `quotRem` fromIntegral (toInteger b2) in (BinVal k, BinVal k')
toIntegerB :: forall sz. BinVal sz -> Integer
toIntegerB (BinVal b) = toInteger b
andB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> BinVal s1
(BinVal b1) `andB` (BinVal b2) =
    let k = b1 .&. fromIntegral (toInteger b2) in BinVal k
orB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> BinVal s1
(BinVal b1) `orB` (BinVal b2) =
    let k = b1 .|. fromIntegral (toInteger b2) in BinVal k
xorB :: forall s1 s2. (s1 ~ s2) => BinVal s1 -> BinVal s2 -> BinVal s1
(BinVal b1) `xorB` (BinVal b2) =
    let k = b1 `xor` fromIntegral (toInteger b2) in BinVal k
complementB :: forall sz . BinVal sz -> BinVal sz
complementB (BinVal b) = BinVal (complement b)
shiftB :: forall sz . BinVal sz -> Int -> BinVal sz
shiftB (BinVal b) n = BinVal (shift b n)
rotateB :: forall sz . BinVal sz -> Int -> BinVal sz
rotateB (BinVal b) n = BinVal (rotate b n)
bitSizeB :: forall sz . BinVal sz -> Int
bitSizeB (BinVal b) = bitSize b
bitSizeMaybeB :: forall sz . BinVal sz -> Maybe Int
bitSizeMaybeB (BinVal b) = bitSizeMaybe b
isSignedB :: forall sz . BinVal sz -> Bool
isSignedB (BinVal b) = isSigned b
testBitB :: forall sz . BinVal sz -> Int -> Bool
testBitB (BinVal b) = testBit b
popCountB :: forall sz . BinVal sz -> Int
popCountB (BinVal b) = popCount b
sizeB :: forall sz . BinVal sz -> Int
sizeB (BinVal b) = finiteBitSize b



