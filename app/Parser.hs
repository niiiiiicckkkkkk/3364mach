{-# LANGUAGE InstanceSigs #-}

{-
module Parser(
    parseASM
) where -}
module Parser where

import Control.Applicative
import Data.Char

import Loader
import Simulator


newtype Parser a = P {doParse :: String -> Maybe (a, String)}

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
takeWhileP f p = (:) <$> filterP f p <*> (takeWhileP f p <|> pure [])

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
numP = minusP <*> fmap Prelude.read (some digitChar)
    where
        minusP :: Parser (Integer -> Integer)
        minusP = char '-' *> pure negate <|> pure id

hexP :: Parser String
hexP = stringP "0x" *> many (filterP (flip elem $ ['0'..'9'] ++ ['a'..'f']) get)

wsP :: Parser a -> Parser a
wsP p = p <* many (filterP isSpace get)

commentP :: Parser ()
commentP = wsP $ char '#' *> optional (takeWhileP (/='\n') get) *> pure ()

labelP :: Parser Label
labelP = (:) <$> filterP isAlphaNum get <*> many identifierChar

opP :: Parser Opcode
opP =   
        stringP "load" *> pure Load
    <|> stringP "store" *> pure Store
    <|> stringP "jumpnz" *> pure JumpNZ
    <|> stringP "jumpn" *> pure JumpN
    <|> stringP "jumpz" *> pure JumpZ
    <|> stringP "jump" *> pure Jump
    <|> stringP "add" *> pure Add
    <|> stringP "sub" *> pure Sub
    <|> stringP "mul" *> pure Mul
    <|> stringP "out" *> pure Out

-- p ( m a -> p a) -> p (m a) -> p a

maybeP :: Parser (Maybe a) -> Parser a
maybeP p = P $ \s -> doParse p s >>=
                     \(a, s') -> a >>=
                        \a -> return (a, s')

operandP :: Parser Operand
operandP = L <$> labelP <|> B . fromIntegral <$> numP

asmP :: Parser ASM
asmP = many commentP *> (wsP insnP <|> wsP defP)
    where
        insnP = Insn <$> wsP (optional (labelP <* char ':')) <*> wsP opP <*> wsP operandP
        defP = Def <$> wsP (labelP <* char ':') <* wsP (stringP ".data") <*> wsP numP

programP :: Parser Program
programP = maybeP (loader <$> many asmP)

parseASM :: String -> Maybe Program
parseASM = fmap fst . doParse programP


test :: String
test = "#\n        # This block is here to delay O(1 second) after displaying each new\n        # prime number, so that you can see each number display.\n        #\nload    Jdelay  #"

-- >>> doParse (many asmP) test
-- Just ([Insn Nothing Load (L "Jdelay")],"#")

-- >>> doParse (asmP) "load     jdelay    #"
-- Just (Insn Nothing Load (L "jdelay"),"#")







