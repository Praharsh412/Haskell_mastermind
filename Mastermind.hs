module Mastermind
    ( 
        Code
        ,humanGuess
        ,solvecomp
        ,humantohuman
        ,getGuessResult
        ,codeMyformat
        ,generateSecretCode
    ) where   --Module mastermind for all sort of functions and typeclasses

import Data.Maybe (fromJust)  --LIbrary for optional values 
import Data.List (elemIndex, sort, group) --Sorting library functions
import System.Random  --For random generation
import System.IO 
import Data.Char (digitToInt)  --For converting string to Integer
import System.Console.ANSI

type Code = (Int, Int, Int, Int) -- code formata


data Result = Result Int -- no of whites
                     Int  -- no of blacks
                     deriving (Eq, Show)

printCode :: Code -> String  --Declaring the printcode which takes the number and prints
printCode (d1,d2,d3,d4) = (show d1) ++ (show d2) ++ (show d3) ++ (show d4)

printwhite 0 = return ()
printwhite n =
 do
  print ("-")
  printwhite (n-1)

printblack 0 = return ()
printblack n =
 do
  print ("+")
  printblack (n-1)
 
hello_worlds ::Int->IO()
hello_worlds n = putStrLn $ unlines (replicate n "Hello World")
-- algorithm for solving the mastermind game 
solvecomp :: String ->IO(Int) -- solves the game and returns the no of attempts taken
solvecomp secret = do
    
    let 
        -- build list of candidate codes
        
        possibleSet= [(d1::Int ,d2::Int ,d3::Int ,d4::Int) | d1 <- [0..5], d2 <- [0..5], d3 <- [0..5], d4 <- [0..5]]
        numOfMoves = 1::Int
        initialGuess = (1::Int ,1::Int ,2::Int ,2::Int) -- Initial Guess by the computer is 1122
        secretcode = codeMyformat secret
     
    finalNumOfMoves <- goloop1 possibleSet numOfMoves initialGuess  secretcode -- Final number of moves is updated by goloop1 which is declared later  
    return finalNumOfMoves -- Function solvecomp returns the final number of moves

    where
        goloop1 :: [Code] -> Int -> Code ->Code -> IO(Int) -- This function takes the and produces output and does it iteratively.
        goloop1 set moves guess secretcode= do

            putStrLn $ "Guess Code: " ++ (printCode guess)  -- Code guessed by the system.

            let 
                Result white black = getGuessResult guess secretcode
            putStrLn $"No of Whites: " ++ show(white)  --get whites is updtaed
            putStrLn $"No of Blacks: "++ show (black)++"\n" --Blacks number is updated

            if black == 4 -- When black is equal to 4 then the output by the system is equal to the user input
                then
                    return moves
                else -- Do this until black is 4
                    do 
                        -- eliminate impossible codes from candidate list
                        let 
                            newcandcodes = differenceSet set guess $ Result white black
                            -- get a new candidate code
                            newguess = head newcandcodes
                            -- loop again
                        goloop1 newcandcodes (moves + 1) newguess secretcode

                        

        --This function removes the set of all possible combinations that has same score as guess
        differenceSet codes guess res = 
            filter (\c -> (getGuessResult guess c) == res) codes

-- Check a guess against a code and returns the result
getGuessResult :: 
           Code  -- ^ guess code
        -> Code  -- ^ secret code
        -> Result
getGuessResult (g1,g2,g3,g4) (s1,s2,s3,s4) = 
    let 
        gList = [g1,g2,g3,g4]
        sList = [s1,s2,s3,s4]
        numOfBlacks = sum $ zipWith (\g s -> if g == s then 1::Int else 0::Int) gList sList

    
        zippedList = filter (\(g,s) -> g /= s) (zip gList sList)
        new_gList = fst $ unzip zippedList
        new_sList = snd $ unzip zippedList

        gListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_gList)  -- note that the Data.List.group function produces a list of lists: group :: Eq a => [a] -> [[a]]
        sListGrouped = map (\a_group -> (head a_group, length a_group)) $ group (sort new_sList)

      
        numOfWhites =   sum $ map (
                                    -- for each guess code digit return the number of whites
                                    \(g, numOfg) -> sum $
                                        -- traverse the secret code list
                                        map (
                                            \(s,numOfs) -> if s == g then min (numOfg::Int) (numOfs::Int) else 0::Int
                                        ) sListGrouped

                                ) gListGrouped          
    in Result numOfWhites numOfBlacks

humantohuman :: IO(Int) -- ^ returns number of moves until secret code is found
humantohuman = do -- undefined
    
    putStrLn "P1 : Create your code" 
    p1code <-getLine
    clearScreen
    let secretcode = codeMyformat p1code
    putStr "P2:Your guess should be (a 4-digit number)\n"

    let attempts = 1::Int
    totalmoves <- goloop2 attempts secretcode    
    return totalmoves    

    where
        goloop2 :: Int -> Code -> IO(Int)
        goloop2 moves secretcode = do 

            putStrLn "P2 Guess: "
            hFlush stdout
            guess <- getLine
            let 
                gcode = codeMyformat guess
                Result numOfWhites numOfBlacks = getGuessResult gcode secretcode
            if numOfBlacks == 4 && numOfWhites == 0 
                then 
                    return moves
                else
                    do 
                        putStrLn $ "Blacks :" ++ (show numOfBlacks)
                        putStrLn $ "Whites :" ++ (show numOfWhites)
                        goloop2 (moves + 1) secretcode




codeMyformat :: String -> Code
codeMyformat ans = 
    let
        -- get prefix
        [c1,c2,c3,c4] = take 4 ans
    in (digitToInt c1, digitToInt c2, digitToInt c3, digitToInt c4)

humanGuess:: StdGen -> IO(Int) -- ^ returns number of moves until secret code is found
humanGuess gen = do -- undefined
    
    let scode = generateSecretCode gen -- generate the secret code


    let numOfMoves = 1::Int
    finalNumOfMoves <- goloop2 numOfMoves scode    
    return finalNumOfMoves    

    where
        goloop2 :: Int -> Code -> IO(Int)
        goloop2 moves scode = do 
            putStr "Give your guess (a 4-digit number): "
            hFlush stdout
            guess <- getLine
            let 
                gcode = codeMyformat guess
                Result numOfWhites numOfBlacks = getGuessResult gcode scode
            if numOfBlacks == 4 && numOfWhites == 0 
                then 
                    return moves
                else
                    do 
                        putStrLn $ "Blacks = " ++ (show numOfBlacks)
                        putStrLn $ "Whites = " ++ (show numOfWhites)
                        goloop2 (moves + 1) scode

generateSecretCode :: StdGen -> Code
generateSecretCode gen = 
    let
        ls = take 4 (randoms gen :: [Int])        
        [s1,s2,s3,s4] = map (\s -> abs $ mod s 10 ) ls
    in (s1,s2,s3,s4) 