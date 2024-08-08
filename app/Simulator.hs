{-# LANGUAGE InstanceSigs #-}

module Simulator where

import Binary
import qualified Data.Map as M

import qualified Control.Monad.State as ST

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import Data.Default

newtype Wire a = Wire { w :: a } deriving Show

data DFF a = DFF { d :: a, q :: a} deriving Show

data Reg a = Reg {
    dff :: DFF a,
    we :: Bool
} deriving Show

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
} deriving Show

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

type Arg = Bin8
data MachInsn = MachInsn Opcode Arg | NOP deriving Show

data Program = Program {
    assocs :: [(MachAddr, MachWord)],
    start :: MachAddr,
    end :: MachAddr
}

data CPU = CPU {
    pc :: Reg MachAddr,
    ac :: Reg MachWord,
    ir :: Reg MachInsn
} deriving Show

data S = S {
    ram :: RAM,
    cpu :: CPU,
    prev :: Maybe S,
    t :: Transition S
}

instance Show S where
    show :: S -> String
    show s = show (ram s) ++ "\n" ++ show (cpu s)

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
    prev = Nothing,
    t = fetch
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
        ram = r { memAddr = Wire curr },
        prev = Just s,
        t = decode
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
                        _ -> error "bad insn"
                

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


