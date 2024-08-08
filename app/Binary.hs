--{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ExplicitForAll #-}

module Binary() where

import Data.Word
import Data.Bits
import Data.Ix

import Data.Monoid

import Data.Proxy

import GHC.TypeLits

-- Binary Kinds hold information about their length
type Size = Nat

data BinVal (sz :: Size) = forall a. (Show a, Integral a, Num a, Ix a, FiniteBits a) => BinVal a

instance Show (BinVal s) where
    show (BinVal b) =  show b


newtype BinConv (sz :: Size) = BinConv (forall a. Integral a => a -> BinVal sz)

binConv8 :: BinConv 8
binConv8 = BinConv (BinVal @8 . fromIntegral @_ @Word8)

binConv16 :: BinConv 16
binConv16 = BinConv (BinVal @16 . fromIntegral @_ @Word16)
            

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

extend :: forall srcSz dstSz. (
          KnownNat srcSz,
          KnownNat dstSz,
          (srcSz <=? dstSz) ~ 'True) => 
          BinVal srcSz ->
          BinConv dstSz -> BinVal dstSz
extend (BinVal b) (BinConv f) = f b

test8 :: BinVal 8
test8 = BinVal (0x5 :: Word8)

test16 :: BinVal 16
test16 = BinVal (0x5 :: Word16)




