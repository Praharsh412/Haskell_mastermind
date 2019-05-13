module Main where

import Mastermind
import System.Random

main :: IO ()
main = do
    putStrLn "MASTERMIND"

    gen <- getStdGen -- getting Secret code input from the user
    playGame gen 

playGame :: StdGen -> IO()
playGame gen = do
    
    putStrLn "a)Computer Solving your Algorithm"
    putStrLn "b)Human VS Human"
    putStrLn "c)computer Generated code"
    putStrLn "Enter the type of game to be played(a,b,c)"

    mode<-getLine
    if mode=="a"
    	then
    		do--let secret ="abcd"
    			putStrLn "Enter secret code: "
    			secret <- getLine
    			totalmoves <- solvecomp secret
    			putStrLn $ "Found solution in " ++ (show totalmoves) ++ " moves!"
    	else 
    		if mode=="b"
    			then 
    				do
    				    putStrLn "Press Enter to start"
    				    _<-getLine
         
    				    attempt<-humantohuman 
    				    putStrLn $ "Found solution in "++(show attempt)

        else
            if mode=="c"
                 then 
                     do 
                        putStrLn "Press Enter to start"
                        _<-getLine
                        let 
                            (_, genNew) = (random gen) :: (Int, StdGen)
                        attempt<-humanGuess genNew
                        putStrLn $"Found soultion in "++show(attempt)
        else
            do 
              putStrLn "bye"                 
        