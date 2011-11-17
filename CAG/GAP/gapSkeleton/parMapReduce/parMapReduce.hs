-- This is the running comman:
-- ./parReduce "Sum" "3" "9" "3" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" +RTS -qN3
-- parReduce is the execusion program
-- "Sum" is the function name in GAP
-- the rest are the list elements 
module Main where
import System(getArgs)
import IO
import System.Time
import Control.Parallel.Strategies
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
             fn1 = getWord (args!!0) :: String
             fn2 = getWord (args!!1) :: String
	     value = args2List(args)
-- 	     a = gapInitAllGum
--	   t1 <- getClockTime
 	   a <- gapInitAllGum
  	   hPutStrLn stderr (" Functions " ++ fn1 ++ ", " ++ fn2 ++ " args:" ++ (show value) ++" " )
 	   hFlush stderr
  	   print (solveProblem fn1 fn2  value)
	   b <- gapTermAllGum  
--	   t2 <- getClockTime
--	   putStrLn (show (diffClockTimes t2 t1))
--	   hFlush stderr
	   return()

---------------------------------------------------------------------------
solveProblem :: String -> String -> [String] -> String
solveProblem mapFname redFname myList = parmapReduce mapCalculate redCalculate myList
    where
    mapCalculate x     = gapObject2String(gapEval mapFname [string2GapExpr x])
    redCalculate xList = gapObject2String(gapEval redFname (map string2GapExpr xList))
    
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
args2List (x:xs) = args3List xs

args3List :: [a] -> [a]
args3List [] = []
args3List (x:xs) = xs

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
-- parMapEden :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
-- parMapEden f xs = map deLift ([ createProcess (process f) x
--                               | x <- xs ] `using` whnfspine)
-- -- demand control helper
-- whnfspine :: Strategy [a]
-- whnfspine [] = ()
-- whnfspine (x:xs) = x `seq` whnfspine xs

-- parReduceL :: (Trans b) => Int -> ([b] -> b) -> [b] -> b
-- parReduceL np f list
--    = let sublists = splitIntoN np list
--          subRs = parMapEden f sublists
--      in f subRs

-- splitIntoN :: Int -> [a] -> [[a]]
-- splitIntoN n xs = takeIter parts xs
--  where l = length xs
--        parts = zipWith (+)
--                  ((replicate (l `mod` n) 1) ++ repeat 0)
--                  (replicate n (l `div` n))
-- takeIter :: [Int] -> [a] -> [[a]]
-- takeIter [] [] = []
-- takeIter [] _ = error "elements left over"
-- takeIter (t:ts) xs = hs : takeIter ts rest
--    where (hs,rest) = splitAt t xs

