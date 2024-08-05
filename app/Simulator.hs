module Simulator where

import Binary
import Data.Array

import qualified Control.Monad.State as ST

data Wire = Wire { w :: Binary }

data DFF a = DFF { d :: a, q :: a}

data Reg a = Reg {
    dff :: DFF a,
    we :: Bool
}

data RAM = RAM { 
    mem :: Array Int (Reg Binary), 
    memWE :: Bool,
    memAddr :: Wire,
    memIn :: Wire,
    memOut :: Wire
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
            | Out

type Arg = Binary
data Insn = Insn Opcode Arg

data CPU = CPU {
    pc :: Reg Binary,
    ac :: Reg Binary,
    ir :: Reg Insn
}

data S = S {
    ram :: RAM,
    cpu :: CPU,
    prev :: S
}

regIn :: Reg a -> a
regIn = d . dff

regOut :: Reg a -> a
regOut = q . dff

setReg :: a -> Reg a -> Reg a
setReg a r = r { dff = (dff r) {d = a}, we = True}

setReg8 :: Binary -> Reg Binary -> Reg Binary
setReg8 = setReg . resizeS 8

setReg16 :: Binary -> Reg Binary -> Reg Binary
setReg16 = setReg . resizeS 16

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
                r {mem = m'}
        | otherwise =
            let 
                m = tickReg <$> mem r
                o = Wire $ proj16 (w $ memAddr r) m
            in 
                r {mem = m, memOut = o}

    proj16 :: (Ix a, Integral a) => Binary -> Array a (Reg Binary) -> Binary
    proj16 addr memory = (resizeS 16 . d . dff) $ memory ! toNum (resizeS 8 addr)

    assign16 :: (Ix a, Integral a) => Binary -> Binary -> Array a (Reg Binary) -> Array a (Reg Binary)
    assign16 addr v memory = 
        let 
            idx = toNum (resizeS 8 addr)
            reg = memory ! idx
            value = resizeS 16 v
        in 
            memory // [(idx, setReg16 value reg)]

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
        cpu = c { pc = setReg (resizeS 8 $ curr !+ one) (pc c)},
        ram = r { memAddr = Wire (resizeS 8 curr) }
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
        ram = r { memAddr = Wire $ resizeS 8 i}
    }
    return execute

    where
        readInsn :: Binary -> Insn
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
        Insn Load a ->
            ST.put s { cpu = c {ac = setReg16 memData acreg}} >> return fetch
        Insn Store a -> do
            ST.put s { ram = r {memAddr = Wire a, memIn = Wire acval, memWE = True}}
            return store2
        Insn Jump a -> do
            ST.put s { cpu = c {pc = setReg8 a pcreg }}
            return fetch
        Insn JumpZ a ->
            if toNumS acval == 0 
                then ST.put s { cpu = c {pc = setReg8 a pcreg }} >> return fetch
                else return fetch
        Insn JumpN a ->
            if toNumS acval < 0
                then ST.put s { cpu = c {pc = setReg8 a pcreg }} >> return fetch
                else return fetch
        Insn JumpNZ a ->
            if toNumS acval /= 0
                then ST.put s { cpu = c {pc = setReg8 a pcreg }} >> return fetch
                else return fetch
        Insn Add a ->
            ST.put s { cpu = c {ac = setReg16 (memData !+ acval) acreg}} >> return fetch
        Insn Sub a ->
            ST.put s { cpu = c {ac = setReg16 (memData !- acval) acreg}} >> return fetch
        Insn Mul a ->
            ST.put s { cpu = c {ac = setReg16 (memData !* acval) acreg}} >> return fetch
        Insn Out _ -> return fetch

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


