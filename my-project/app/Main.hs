
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
    putStrLn "Insert command: "
    string <- getLine
    let prompt = parseFirst replCommand string
    case prompt of
        Just Quit -> return () 
        Just (Load a) -> do 
            putStrLn "The load command!"
            main
        Just (Eval a) -> do
            putStrLn a
            let rez = parseFirst exprParser a
            case rez of
                Nothing -> do
                    putStrLn "Could not parse!"
                    main
                (Just exp) ->  
                            let text1 = showExp exp
                            in do
                                putStrLn text1
                                main

