-- This is the running comman:
-- ./masterWorkers "Fibonacci" 6 "3" "100" "101" "102" "103" "104" "105"
-- masterSlaves is the execusion program
-- "Fibonacci" is the function name in GAP
-- 6 is chunk size
-- number of workers is the number of PEs
-- the rest are the list elements 
module Main where
import System(getArgs)
import IO
import System.Time
-- import Control.Parallel.Strategies
-- import Control.Parallel
import GapAPI
import Skeletons
import Eden		-- Enabling remote processes in a functional style.
import System.IO.Unsafe(unsafePerformIO) -- for Eden-6 skeletons below
import Control.Monad
import List(transpose, sortBy)
import Data.Char
---------------------------------------------------------------------------

main :: IO ()
main =  do args <- getArgs
	   let
             fn = getWord (args!!0) :: String
	     chunk = read (args!!1) :: Int
	     value = argsList(args)
-- 	     a = gapInitAllGum
 	   a <- gapInitAllGum
--  	   hPutStrLn stderr (" Function " ++ fn ++ " pes:" ++ (show a) ++" " )
-- 	   hFlush stderr
--	   t1 <- getClockTime
 	   print (solveProblem fn chunk value)
--	   t2 <- getClockTime
--	   putStrLn (show (diffClockTimes t2 t1))
	   hFlush stderr
	   b <- gapTermAllGum  
	   return()

---------------------------------------------------------------------------


solveProblem :: String -> Int -> [String] -> [String]
-- solveProblem :: String -> [String] -> [GapObject]
solveProblem fname chunk myList = masterSlaves chunk calculate myList
    where
    calculate x = gapObject2String(gapEval fname  [string2GapExpr x])  
--     calculate x = gapEval fname  [string2GapExpr x]  

---------------------------------------------------------------------------
getWord :: String -> String
getWord [] = []
getWord (x:xs)
	| elem x whitespace = []
	| otherwise         = x : getWord xs
	where 
	 whitespace = ['\n', '\t', ' ']
---------------------------------------------------------------------------
argsList :: [a] -> [a]
argsList [] = []
argsList (x:xs) = args2List xs

args2List :: [a] -> [a]
args2List [] = []
args2List (x:xs) = xs

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- splitAtN
---------------------------------------------------------------------------
-- Breaks a list into a list of lists, each of length n

splitAtN :: Int -> [a] -> [[a]]
splitAtN n [] = []
splitAtN n xs = ys : splitAtN n zs
 	        where (ys,zs) = splitAt n xs


---------------------------------------------------------------------------
-- Eden staff
---------------------------------------------------------------------------

