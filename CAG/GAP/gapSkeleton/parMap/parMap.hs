module Main where
import System(getArgs)
import IO
import System.Time
import Control.Parallel.Strategies
import Control.Parallel
import GapAPI
import Skeletons
import Eden		-- Enabling remote processes in a functional style.
import System.IO.Unsafe(unsafePerformIO) -- for Eden-6 skeletons below
import Control.Monad
import List(transpose)

---------------------------------------------------------------------------

main :: IO ()
main =  do args <- getArgs
	   let
             fn = getWord (args!!0) :: String
	     value = argsList(args)
-- 	   t1 <- getClockTime
	   a <- gapInitAllGum
--  	   hPutStrLn stderr (" Function " ++ fn ++ " and the args:" ++ (show value) ++ " value length: " ++ show(length(value)) ++ " ")
-- 	   hFlush stderr
 	   print (solveProblem fn value)
	   hFlush stderr
	   b <- gapTermAllGum  
-- 	   t2 <- getClockTime
-- 	   putStrLn (show (diffClockTimes t2 t1))
	   return()

---------------------------------------------------------------------------
solveProblem :: String -> [String] -> [String]
-- solveProblem :: String -> [String] -> [GapObject]
solveProblem fname myList = parmap calculate myList
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
argsList (x:xs) = xs

---------------------------------------------------------------------------

parMapF :: (Trans a, Trans b) => (a->b) -> [a] -> [b]
parMapF f xs = map deLift result
    where 
    pe = noPe
    result = [createProcess (pf f)  x | x <- xs] `using` whnfspine

pf :: (Trans a, Trans b) => (a->b) -> Process a b
pf f = process f  -- JB: simplified process syntax

whnfspine []     = () -- JB: evaluation must be forced explicitly
whnfspine (x:xs) = x `seq` whnfspine xs
---------------------------------------------------------------------------
