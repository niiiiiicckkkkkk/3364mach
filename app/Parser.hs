{-# LANGUAGE InstanceSigs #-}

module Parser(
    Parser,
) where

import Control.Applicative
import Data.Char
import Binary
import Data.Array
import Data.Maybe


newtype Parser a = P {doParse :: String -> Maybe (a, String)}
data Operation =
              Load
            | Store
            | Jump
            | JumpZ
            | JumpN
            | JumpNZ
            | Add
            | Sub
            | Mul
            | Out deriving (Eq)

type Label = String
data Value = BinaryVal Binary | LabelVal Label
data ASM = Insn (Maybe Label) Operation Value| Def Label Value | NOP

data Program = Program {
    memory :: Array Int Binary,
    pc :: Binary
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \s -> 
                doParse p s >>=
                    \(a, s') -> return (f a, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \s -> Just (a, s)

    (<*>) :: Parser (t -> a) -> Parser t -> Parser a
    f <*> t = P $ \s -> 
                doParse f s >>=
                    \(f, s') -> 
                        doParse t s' >>=
                            \(t, s'') -> return (f t, s'')

instance Alternative Parser where
    empty :: Parser a
    empty = P $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s
        where
            firstJust (Just x) _ = Just x
            firstJust Nothing y = y


get :: Parser Char
get = P $ \s ->
        case s of
            ""      -> Nothing
            (c:cs)  -> Just (c, cs)

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP f p = P $ \s -> doParse p s >>= check f
    where
        check :: (a -> Bool) -> (a, String) -> Maybe (a, String)
        check guard (a, s')
            | guard a = Just (a, s')
            | otherwise = Nothing

takeWhileP :: (a -> Bool) -> Parser a -> Parser [a]
takeWhileP f p = (:) <$> filterP f p <*> (takeWhileP f p <|> eof *> pure [])

alphaChar :: Parser Char
alphaChar = filterP isAlpha get

digitChar :: Parser Char
digitChar = filterP isDigit get

identifierChar :: Parser Char
identifierChar = alphaChar <|> digitChar <|> char '_'

char :: Char -> Parser Char
char = flip filterP get . ((==) . toLower)

charCase :: Char -> Parser Char
charCase = flip filterP get . (==)

stringP :: String -> Parser String
stringP = foldr ((\c p -> (:) <$> char c <*> p) . toLower) (pure "")

eof :: Parser ()
eof = P $ \s -> 
        case s of
            "" -> Just ((), s)
            _ ->  Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure []

numP :: Parser Integer
numP = fmap read (some digitChar)

hexP :: Parser String
hexP = stringP "0x" *> many (filterP (flip elem $ ['0'..'9'] ++ ['a'..'f']) get)

wsP :: Parser a -> Parser a
wsP p = p <* many (filterP isSpace get)

commentP :: Parser ()
commentP = char '#' *> pure ()

labelP :: Parser Label
labelP = (:) <$> filterP ((&&) <$> isAlpha <*> isLower) get <*> many identifierChar

opP :: Parser Operation
opP =   
        stringP "load" *> pure Load
    <|> stringP "store" *> pure Store
    <|> stringP "jump" *> pure Jump
    <|> stringP "jumpz" *> pure JumpZ
    <|> stringP "jumpn" *> pure JumpN
    <|> stringP "jumpnz" *> pure JumpNZ
    <|> stringP "add" *> pure Add
    <|> stringP "sub" *> pure Sub
    <|> stringP "mul" *> pure Mul
    <|> stringP "out" *> pure Out

-- p ( m a -> p a) -> p (m a) -> p a

maybeP :: Parser (Maybe a) -> Parser a
maybeP p = P $ \s -> doParse p s >>=
                     \(a, s') -> a >>=
                        \a -> return (a, s')

valueP :: Parser Value
valueP = binaryP <|> hexArg <|> labelArg
    where
        binaryP = BinaryVal <$> maybeP (readNum <$> numP)
        hexArg = BinaryVal <$> maybeP (readHex <$> hexP)
        labelArg = LabelVal <$> labelP

asmP :: Parser ASM
asmP = insnP <|> defP
    where
        insnP = Insn <$> wsP (optional (labelP <* char ':')) <*> wsP opP <*> wsP valueP
        defP = Def <$> wsP (labelP <* char ':') <* wsP (stringP ".data") <*> wsP valueP

memLayout :: (Enum a, Num a) => a -> [ASM] -> [(a, ASM)]
memLayout sz asm = zip [sz - 1, sz - 2..0] (reverse asm ++ repeat NOP)

loadMemory :: (Integral a) => a -> [(a, ASM)] -> [(Label, a)] -> Maybe [(a, Binary)]
loadMemory sz assembly labels = undefined
    where
        binOfAsm :: ASM -> Maybe Binary
        binOfAsm (Insn _ op (BinaryVal b)) = fmap (!++) (lookup op opcodes) <*> pure b
        binOfAsm (Insn _ op (LabelVal l)) = fmap (!++) (lookup op opcodes) <*> (lookup l labels >>= toAddr)
        binOfAsm (Def _ (BinaryVal b)) = pure b
        binOfAsm (Def l (LabelVal l')) = fmap (resizeS 16) $ lookup l' labels >>= toAddr
        binOfAsm NOP = pure (resizeS 16 zero)


        loadNum n = resizeS sz <$> readNum n
        loadBin = resizeS sz
        toAddr a = readNum a >>= \b -> return $ resizeS 8 b

        opcodes :: [(Operation, Binary)]
        opcodes = [
                (Load, undefined),
                (Store, undefined),
                (Jump, undefined),
                (JumpZ, undefined),
                (JumpN, undefined),
                (JumpNZ, undefined),
                (Add, undefined),
                (Sub, undefined),
                (Mul, undefined),
                (Out, undefined)
            ]


loader :: [ASM] -> Maybe Program
loader asm = undefined
    where
        constants = filter (\asm -> case asm of {Def _ _ -> True; _ -> False}) asm
        insns = filter (\asm -> case asm of {Insn _ _ _ -> True; _ -> False}) asm
        layout = memLayout 256 (reverse constants ++ reverse insns)
        labels = foldr mkLabels [] layout

        mkLabels :: Num a => (a, ASM) -> [(a, Label)] -> [(a, Label)]
        mkLabels (a, Def l _) acc = (a, l) : acc
        mkLabels (a, Insn (Just l) _ _) acc = (a, l) : acc
        mkLabels _ acc = acc

programP :: Parser Program
programP = maybeP (loader <$> many asmP)







