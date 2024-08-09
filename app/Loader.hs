{-# LANGUAGE DataKinds #-}
module Loader where

import Binary
import Simulator

import Data.Maybe

type Label = String
data Operand = L Label | B Integer deriving Show
data ASM = Insn (Maybe Label) Opcode Operand | Def Label Integer deriving Show

memSize :: Integer
memSize = 256

opcodes :: [(Opcode, BinVal 16)]
opcodes = [
        (Load, 0x0000),
        (Store, 0x0100),
        (Jump, 0x0200),
        (JumpZ, 0x0300),
        (JumpN, 0x0400),
        (JumpNZ, 0x0900),
        (Add, 0x0500),
        (Sub, 0x0600),
        (Mul, 0x0700),
        (Out, 0x0800)
    ]

memLayout :: Integral a => a -> [ASM] -> [(MachAddr, ASM)]
memLayout sz asm = zip (fromIntegral <$> [sz - 1, sz - 2..0]) (reverse asm)

encodeASM :: [(Label, MachAddr)] -> ASM -> Maybe MachWord
encodeASM labels (Insn _ op arg) =
    case arg of
        B b -> (+) <$> lookup op opcodes <*> pure (fromIntegral b)
        L l -> (+) <$> lookup op opcodes <*> (fromIntegral <$> lookup l labels)
encodeASM _ (Def _ b) = Just (fromIntegral b)

hasLabel :: ASM -> Bool
hasLabel (Insn (Just _) _ _) = True
hasLabel (Def _ _) = True
hasLabel _ = False

getLabel :: (MachAddr, ASM) -> Maybe (Label, MachAddr)
getLabel (addr, Insn (Just l) _ _) = Just (l, addr)
getLabel (addr, Def l _) = Just (l, addr)
getLabel _ = error "shouldn't happen"

labelLayout :: [(MachAddr, ASM)] -> Maybe [(Label, MachAddr)]
labelLayout = traverse getLabel . filter (hasLabel . snd)

loader :: [ASM] -> Maybe Program
loader asm = do
    let constants = filter (\asm -> case asm of {Def _ _ -> True; _ -> False}) asm
    let insns = filter (\asm -> case asm of {Insn _ _ _ -> True; _ -> False}) asm
    let layout = memLayout memSize (reverse constants ++ reverse insns)
    labels <- labelLayout layout
    as <- traverse (traverse (encodeASM labels)) layout
    s <- lookup "start" labels
    e <- lookup "done" labels
    return Program { bindings = [], start = 0, end = 0 }
