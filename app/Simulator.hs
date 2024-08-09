{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Simulator where

import Prelude hiding (read, write)

import Binary
import Data.Array

import Data.Ix

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
    assocs :: [(MachAddr, MachWord)],
    start :: MachAddr,
    end :: MachAddr
}

data CPU munit addr word = CPU {
    pc :: munit addr,
    ac :: munit word,
    ir :: munit MachInsn
}


data S mu a w = S {
    ram :: RAM a mu w,
    cpu :: CPU mu a w,
    prev :: Maybe (S mu a w),
    t :: Transition (S mu a w)
}
{-
initState :: Program -> S
initState p = S {
    ram = RAM {
        mem = M.fromAscList ((fmap . fmap) regOfVal (assocs p)),
        memWE = False,
        memAddr = Wire 0,
        memIn = Wire 0,
        memOut = Wire 0
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
        regOfVal a = Reg { dff = DFF{d = a, q = a}, we = False} -}

tickRegs :: (MachCmp mu) => ST.State (S mu a w) ()
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

risingEdge :: (Ix a, MachCmp mu) => ST.State (S mu a w) ()
risingEdge = 
    tickRegs >> ST.get >>=
        \s -> ST.put $ s { ram = tick (ram s) }


fetch :: (Ix a, Debug a, Debug w, MachCmp mu) => Transition (S mu a w)
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
    

decode :: Transition (S mu a w)
decode = undefined {-T $ do
    tick
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    reg <- ST.gets (ir . cpu)
    i <- ST.gets (w . memOut . ram)

    let insn = readInsn i

    ST.put s {
        cpu = c { ir = setReg insn reg },
        ram = r { memAddr = Wire $ trim8 i},
        prev = Just s,
        t = execute
    }
    return execute

    where
        readInsn :: MachWord -> MachInsn
        readInsn (Bin16 i) =
            let bits = NE.toList (getB i)
                opcode = Bin8 (Binary $ bits !! 8 :| tail (drop 8 bits))
                arg = Bin8 (Binary $ head bits :| tail (take 8 bits)) in
                    case opcode of
                        0 -> MachInsn Load arg
                        1 -> MachInsn Store arg
                        2 -> MachInsn Jump arg
                        3 -> MachInsn JumpZ arg
                        4 -> MachInsn JumpN arg
                        5 -> MachInsn Add arg
                        6 -> MachInsn Sub arg
                        7 -> MachInsn Mul arg
                        8 -> MachInsn Out arg
                        9 -> MachInsn JumpNZ arg
                        _ -> error "bad insn" -}
                
{-
execute :: Transition S
execute = T $ do
    tick
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    memData <- ST.gets (w . memOut . ram)
    pcreg <- ST.gets (pc . cpu)
    acreg <- ST.gets (ac . cpu)
    acval <- ST.gets (regOut . ac . cpu)
    insn <- ST.gets (regOut . ir . cpu)

    case insn of
        NOP -> return fetch
        MachInsn Load a ->
            ST.put s { cpu = c {ac = setReg memData acreg}, prev = Just s, t = fetch} >> return fetch
        MachInsn Store a -> do
            ST.put s { ram = r {memAddr = Wire a, memIn = Wire acval, memWE = True}, prev = Just s, t = store2}
            return store2
        MachInsn Jump a -> do
            ST.put s { cpu = c {pc = setReg a pcreg }, prev = Just s, t = fetch}
            return fetch
        MachInsn JumpZ a ->
            if acval == 0 
                then ST.put s { cpu = c {pc = setReg a pcreg }, prev = Just s, t = fetch} >> return fetch
                else return fetch
        MachInsn JumpN a ->
            if acval < 0
                then ST.put s { cpu = c {pc = setReg a pcreg }, prev = Just s, t = fetch} >> return fetch
                else return fetch
        MachInsn JumpNZ a ->
            if acval /= 0
                then ST.put s { cpu = c {pc = setReg a pcreg }, prev = Just s, t = fetch} >> return fetch
                else return fetch
        MachInsn Add a ->
            ST.put s { cpu = c {ac = setReg (memData + acval) acreg}, prev = Just s, t = fetch} >> return fetch
        MachInsn Sub a ->
            ST.put s { cpu = c {ac = setReg (memData - acval) acreg}, prev = Just s, t = fetch} >> return fetch
        MachInsn Mul a ->
            ST.put s { cpu = c {ac = setReg (memData * acval) acreg}, prev = Just s, t = fetch} >> return fetch
        MachInsn Out _ -> return fetch

store2 :: Transition S
store2 = T $ do 
    tick
    s <- ST.get
    r <- ST.gets ram
    ST.put s {ram = r { memWE = False }, prev = Just s, t = fetch}
    return fetch

test1 :: Transition S'
test1 = T $ ST.get >>= \s -> ST.put S' {test = test s + 1} >> return test2

test2 :: Transition S'
test2 = T $ ST.get >>= \s -> ST.put S' {test = test s + 2} >> return test1

inittest :: S'
inittest = S' {
    test = 0
}


-- State S transition -> 
transitionN :: Int -> Transition a -> ST.State a a
transitionN 0 _ = ST.get
transitionN n (T t) = t >>= \t' -> transitionN (n - 1) t'

transitionWhile :: (a -> Bool) -> Transition a -> ST.State a a
transitionWhile p (T t) = 
    ST.get >>=
        \s -> case p s of
                True -> t >>= \t' -> transitionWhile p t'
                False -> ST.get

--testState :: ST.State S' S'
--testState = stepN 3 test1


stepN :: Int -> S -> S
stepN i s = ST.evalState (transitionN i (t s)) s

stepWhile :: (S -> Bool) -> S -> S
stepWhile p s = ST.evalState (transitionWhile p (t s)) s



-- >>> ST.execState testState inittest
-- S {test = 4}

-- 1 3 4 6 7 9 10 12 13 15 16 18 19 21

--ttt :: ST.State S' S'
--ttt = stepWhile (\s -> test s < 20) test1

-- >>> ST.execState ttt inittest
-- S {test = 21}


-}

