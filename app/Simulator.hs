{-# LANGUAGE InstanceSigs #-}
module Simulator where

import Binary
import qualified Data.Map as M

import qualified Control.Monad.State as ST

import Data.Default

newtype Wire a = Wire { w :: a }

data DFF a = DFF { d :: a, q :: a}

data Reg a = Reg {
    dff :: DFF a,
    we :: Bool
}

instance Default a => Default (Reg a) where
    def :: Reg a
    def = Reg { dff = DFF {d = def, q = def}, we = False }

type MachWord = Bin16
type MachAddr = Bin8

data RAM = RAM { 
    mem :: M.Map MachAddr (Reg MachWord), 
    memWE :: Bool,
    memAddr :: Wire MachAddr,
    memIn :: Wire MachWord,
    memOut :: Wire MachWord
}

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
            | Out deriving Eq

type Arg = Bin8
data MachInsn = MachInsn Opcode Arg | NOP

data Program = Program {
    assocs :: [(MachAddr, MachWord)],
    start :: MachAddr,
    end :: MachAddr
}

data CPU = CPU {
    pc :: Reg MachAddr,
    ac :: Reg MachWord,
    ir :: Reg MachInsn
}

data S = S {
    ram :: RAM,
    cpu :: CPU,
    prev :: Maybe S
}

type Word = Bin16

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
    prev = Nothing
}
    where
        regOfVal :: a -> Reg a
        regOfVal a = Reg { dff = DFF{d = a, q = a}, we = False}

regIn :: Reg a -> a
regIn = d . dff

regOut :: Reg a -> a
regOut = q . dff

setReg :: a -> Reg a -> Reg a
setReg a r = r { dff = (dff r) {d = a}, we = True}

tickDFF :: DFF a -> DFF a
tickDFF DFF {d = d, q = q} = DFF {d = d, q = d}

tickReg :: Reg a -> Reg a
tickReg r
    | we r = r {dff = tickDFF (dff r), we = False}
    | otherwise = r

toggleRegWE :: Bool -> Reg a -> Reg a
toggleRegWE b r = r { we = b}

tickRegs :: ST.State S ()
tickRegs = 
    ST.get >>=
        \s -> ST.put s{
            cpu = updateRegs (cpu s)
        }
    where
    updateRegs :: CPU -> CPU
    updateRegs c = c {
        pc = tickReg (pc c),
        ac = tickReg (ac c),
        ir = tickReg (ir c)
    }

tickMem :: ST.State S ()
tickMem =
    ST.get >>=
        \s -> ST.put s{
            ram = updateRam (ram s)
        }
    where
    updateRam :: RAM -> RAM
    updateRam r
        | memWE r =
            let 
                m = tickReg <$> mem r
                m' = assign16 (w $ memAddr r) (w $ memIn r) m
            in
                r {mem = m', memOut = Wire $ proj16 (w $ memAddr r) m'}
        | otherwise =
            let 
                m = tickReg <$> mem r
                o = Wire $ proj16 (w $ memAddr r) m
            in 
                r {mem = m, memOut = o}

    proj16 :: (Ord a, Default b) => a -> M.Map a (Reg b) -> b
    proj16 addr memory = (d . dff) $ M.findWithDefault def addr memory

    assign16 :: (Ord a, Default b) => a -> b -> M.Map a (Reg b) -> M.Map a (Reg b)
    assign16 addr v memory =
        let reg = M.findWithDefault Reg {dff = DFF {d = def, q = def}, we = False } addr memory in
            M.insert addr (setReg v reg) memory

tick :: ST.State S ()
tick = tickRegs >> tickMem

data S' = S' {
    test :: Int
} deriving Show

newtype Transition a = T (ST.State a (Transition a))

fetch :: Transition S
fetch = T $ do
    tick
    s <- ST.get
    r <- ST.gets ram
    c <- ST.gets cpu
    curr <- ST.gets (q . dff . pc . cpu)

    ST.put s {
        cpu = c { pc = setReg (curr + 1) (pc c)},
        ram = r { memAddr = Wire curr }
    }

    return decode
    

decode :: Transition S
decode = T $ do
    tick
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    reg <- ST.gets (ir . cpu)
    i <- ST.gets (w . memOut . ram)

    let insn = readInsn i

    ST.put s {
        cpu = c { ir = setReg insn reg },
        ram = r { memAddr = Wire $ trim8 i}
    }
    return execute

    where
        readInsn :: MachWord -> MachInsn
        readInsn = undefined

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
            ST.put s { cpu = c {ac = setReg memData acreg}} >> return fetch
        MachInsn Store a -> do
            ST.put s { ram = r {memAddr = Wire a, memIn = Wire acval, memWE = True}}
            return store2
        MachInsn Jump a -> do
            ST.put s { cpu = c {pc = setReg a pcreg }}
            return fetch
        MachInsn JumpZ a ->
            if acval == 0 
                then ST.put s { cpu = c {pc = setReg a pcreg }} >> return fetch
                else return fetch
        MachInsn JumpN a ->
            if acval < 0
                then ST.put s { cpu = c {pc = setReg a pcreg }} >> return fetch
                else return fetch
        MachInsn JumpNZ a ->
            if acval /= 0
                then ST.put s { cpu = c {pc = setReg a pcreg }} >> return fetch
                else return fetch
        MachInsn Add a ->
            ST.put s { cpu = c {ac = setReg (memData + acval) acreg}} >> return fetch
        MachInsn Sub a ->
            ST.put s { cpu = c {ac = setReg (memData - acval) acreg}} >> return fetch
        MachInsn Mul a ->
            ST.put s { cpu = c {ac = setReg (memData * acval) acreg}} >> return fetch
        MachInsn Out _ -> return fetch

store2 :: Transition S
store2 = T $ do 
    tick
    s <- ST.get
    r <- ST.gets ram
    ST.put s {ram = r { memWE = False }}
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
stepN :: Int -> Transition a -> ST.State a a
stepN 0 _ = ST.get
stepN n (T t) = t >>= \t' -> stepN (n - 1) t'

stepWhile :: (a -> Bool) -> Transition a -> ST.State a a
stepWhile p (T t) = 
    ST.get >>=
        \s -> case p s of
                True -> t >>= \t' -> stepWhile p t'
                False -> ST.get

testState :: ST.State S' S'
testState = stepN 3 test1

-- >>> ST.execState testState inittest
-- S {test = 4}

-- 1 3 4 6 7 9 10 12 13 15 16 18 19 21

ttt :: ST.State S' S'
ttt = stepWhile (\s -> test s < 20) test1

-- >>> ST.execState ttt inittest
-- S {test = 21}


