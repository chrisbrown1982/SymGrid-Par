-- This is the running command 
-- ./parZipWith "Gcd" "9" "3" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" +RTS -qN9
-- parZipWith the file name
-- Gcd is the function name in GAP
-- the first numbr in the list (9) is the size of the first list the rest will be assigned to the second list.
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
import List(transpose)
import Data.Char
---------------------------------------------------------------------------

main :: IO ()
main =  do args <- getArgs
	   let
             fn = getWord (args!!0) :: String
	     values = argsList(args)
	     firstList = getFirstList(values)
	     secondList = getSecondList(values)
--  	   t1 <- getClockTime
	   a <- gapInitAllGum
--   	   hPutStrLn stderr (" value1 " ++ (show firstList) ++ " value2: " ++ (show secondList) ++" processid" ++ (show a) ++" ")
--  	   hFlush stderr
 	   print (solveProblem fn firstList secondList)
	   hFlush stderr
	   b <- gapTermAllGum
--  	   t2 <- getClockTime
--  	   putStrLn (show (diffClockTimes t2 t1))
	   return()

---------------------------------------------------------------------------
solveProblem :: String -> [String] -> [String] -> [String]
-- solveProblem :: String -> [String] -> [GapObject]
solveProblem fname list1 list2  = parZipWith calculate list1 list2
    where
    calculate x y = gapObject2String(gapEval fname  [string2GapExpr x, string2GapExpr y])  
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
argsList (x:xs) = xs

---------------------------------------------------------------------------
getFirstList :: [String] -> [String]
getFirstList (x:xs) = take (string2Int x) xs

getSecondList :: [String] -> [String]
getSecondList (x:xs) = drop (string2Int x) xs

string2Int :: [Char] -> Int
string2Int x = sum (zipWith (*) listInt tmpList)
    where
    listInt = reverse (map digitToInt x)
    y = [0 .. length x]
    tmpList = [10^n | n <-y] 

