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

module Simulator where

import Prelude hiding (read)

import Binary
import Data.Array
import Data.Proxy
import Data.Maybe

import qualified Control.Monad.State as ST

class MachCmp f where
    tick :: f a -> f a
    write :: a -> f a -> f a
    read :: f a -> a

data DFF a = DFF { d :: a, q :: a}

instance MachCmp DFF where
    tick :: DFF a -> DFF a
    tick dff' = dff' {q = d dff'}

    read :: DFF a -> a
    read = q

    write :: a -> DFF a -> DFF a
    write a dff' = dff' {d = a}


data Reg a = Reg {
    dff :: DFF a,
    we :: Bool
}

instance MachCmp Reg where
    tick :: Reg a -> Reg a
    tick r
        | we r = r {dff = tick (dff r), we = False}
        | otherwise = r
    
    read :: Reg a -> a
    read = read . dff

    write :: a -> Reg a -> Reg a
    write a r = r {we = True, dff = write a (dff r)}


type MachWord = BinVal 16
type MachAddr = BinVal 8

data RAM addr munit word = RAM { 
    mem :: Array addr (munit word),
    memAddr :: addr,
    memIn :: word,
    memOut :: word
}

instance (Ix a, MachCmp m) => MachCmp (RAM a m) where
    tick :: RAM a m w -> RAM a m w
    tick r =
        let
            r' = r { mem = tick <$> mem r }
        in
            r' { memOut = read $ mem r' ! memAddr r' }

    read :: RAM a b w -> w
    read = memOut

    write :: w -> RAM a m w -> RAM a m w
    write w r =
        let
            reg' = write w $ mem r ! memAddr r
            mem' = mem r // [(memAddr r, reg')]
        in
            r { mem = mem'}

select :: a -> RAM a mu w -> RAM a mu w
select a r = r { memAddr = a }

data Opcode =
              Load
            | Store
            | Jump
            | JumpZ
            | JumpN
            | JumpNZ
            | Add
            | Sub
            | Mul
            | Out deriving (Eq, Show)

data MachInsn = MachInsn Opcode (BinVal 8) | NOP

newtype Transition a = T (ST.State a (Transition a))

data Program = Program {
    bindings :: [(MachAddr, MachWord)],
    start :: MachAddr,
    end :: MachAddr
}

data CPU munit addr word = CPU {
    pc :: munit addr,
    ac :: munit word,
    ir :: munit MachInsn
}


data S = S {
    ram :: RAM MachAddr Reg MachWord,
    cpu :: CPU Reg MachAddr MachWord,
    prev :: Maybe S,
    t :: Transition S
}


initState :: Program -> S
initState p = S {
    ram = RAM {
        mem = array (0, 255) [fromMaybe (i, regOfVal 0) $ flip (,) <$> fmap regOfVal (lookup i (bindings p)) <*> pure i | i <- [0..255]],
        memAddr = 0,
        memIn = 0,
        memOut = 0
    },
    cpu = CPU {
        pc = regOfVal (start p),
        ac = regOfVal 0,
        ir = regOfVal NOP
    },
    prev = Nothing,
    t = fetch
}
    where
        regOfVal :: a -> Reg a
        regOfVal a = Reg { dff = DFF{d = a, q = a}, we = False}

tickRegs :: ST.State S ()
tickRegs = 
    ST.get >>=
        \s -> ST.put s{
            cpu = updateRegs (cpu s)
        }
    where
    updateRegs :: (MachCmp mu) => CPU mu a w -> CPU mu a w
    updateRegs c = c {
        pc = tick (pc c),
        ac = tick (ac c),
        ir = tick (ir c)
    }

risingEdge :: ST.State S ()
risingEdge = 
    tickRegs >> ST.get >>=
        \s -> ST.put $ s { ram = tick (ram s) }


fetch :: Transition S
fetch = T $ do
    risingEdge
    s <- ST.get
    r <- ST.gets ram
    c <- ST.gets cpu
    idx <- ST.gets (read . pc . cpu)

    ST.put s {
        cpu = c { pc = write (idx + 1) (pc c)},
        ram = r { memAddr = idx },
        prev = Just s,
        t = decode
    }
    return decode
    

decode :: Transition S
decode = T $ do
    risingEdge
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    ireg <- ST.gets (ir . cpu)
    mout <- ST.gets (memOut . ram)

    let opcode = reduce mout (Proxy @8) (Proxy @15) binConv8
    let operand = reduce mout (Proxy @0) (Proxy @7) binConv8
    let insn = fromMaybe NOP (MachInsn <$> lookup opcode opcodes <*> pure operand)

    ST.put s {
        cpu = c { ir = write insn ireg },
        ram = r { memAddr = operand},
        prev = Just s,
        t = execute
    }
    return execute

    where
        opcodes :: [(BinVal 8, Opcode)]
        opcodes = [
                        (0, Load),
                        (1, Store),
                        (2, Jump),
                        (3, JumpZ),
                        (4, JumpN),
                        (5, Add),
                        (6, Sub),
                        (7, Mul),
                        (8, Out),
                        (9, JumpNZ)
                ]
                

execute :: Transition S
execute = T $ do
    risingEdge
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    mdata <- ST.gets (read . ram)
    pcreg <- ST.gets (pc . cpu)
    acreg <- ST.gets (ac . cpu)
    acval <- ST.gets (read . ac . cpu)
    insn <- ST.gets (read . ir . cpu)

    let s' = s { prev = Just s, t = fetch }

    case insn of
        NOP -> return fetch
        MachInsn Load _ ->
            ST.put s' { cpu = c {ac = write mdata acreg} } >> return fetch
        MachInsn Store a -> do
            let r' = write acval $ r { memAddr = a }
            ST.put s' { ram = r', t = store2}
            return store2
        MachInsn Jump a -> do
            ST.put s' { cpu = c {pc = write a pcreg } }
            return fetch
        MachInsn JumpZ a ->
            if acval == 0 
                then ST.put s' { cpu = c {pc = write a pcreg } } >> return fetch
                else ST.put s' >> return fetch
        MachInsn JumpN a ->
            if acval < 0
                then ST.put s' { cpu = c {pc = write a pcreg } } >> return fetch
                else ST.put s' >> return fetch
        MachInsn JumpNZ a ->
            if acval /= 0
                then ST.put s' { cpu = c {pc = write a pcreg } } >> return fetch
                else ST.put s' >> return fetch
        MachInsn Add _ ->
            ST.put s' { cpu = c {ac = write (mdata + acval) acreg} } >> return fetch
        MachInsn Sub _ ->
            ST.put s' { cpu = c {ac = write (mdata - acval) acreg} } >> return fetch
        MachInsn Mul _ ->
            ST.put s' { cpu = c {ac = write (mdata * acval) acreg} } >> return fetch
        MachInsn Out _ -> return fetch

store2 :: Transition S
store2 = T $ do 
    risingEdge
    s <- ST.get
    ST.put s { prev = Just s, t = fetch }
    return fetch

