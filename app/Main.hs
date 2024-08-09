module Main where

import Parser
import Simulator

import Loader

import System.Environment
import System.IO

import Control.Applicative

simulate :: S -> IO ()
simulate s = do
    cmd <- getLine

    case cmd of
        "dump" -> undefined
        "step" -> undefined
        "run" -> putStrLn "TODO" >> simulate s
        "quit" -> return ()
        _ -> putStrLn ""


loadSim :: String -> IO ()
loadSim file = do
    asm <- readFile file
    {-
    let program = parseASM asm

    case program of
        Nothing -> putStrLn "parse fail"
        Just p -> simulate (initState p)
    -}
    let test = doParse (many asmP) asm
    --print test
    case parseASM asm of
        Just p -> do 
            putStrLn "success"
        Nothing -> putStrLn ":("

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs

    case args of
        [] -> putStrLn "no file provided"
        [f] -> loadSim f
        args@(_ : _) -> 
            putStrLn $ "recieved " ++ show (length args) ++ " arguments but expected 1"

