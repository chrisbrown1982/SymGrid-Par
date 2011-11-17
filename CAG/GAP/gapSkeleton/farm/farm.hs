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
import List(transpose, sortBy)

---------------------------------------------------------------------------

main :: IO ()
main =  do args <- getArgs
	   let
             fn = getWord (args!!0) :: String
	     value = argsList(args)
-- 	   t1 <- getClockTime
	   a <- gapInitAllGum
-- 	   hPutStrLn stderr (" Function " ++ fn ++ " and the args:" ++ (show value) ++ " value length: " ++ show(length(value)) ++ " ")
	   hFlush stderr
 	   print (solveProblem fn value)
	   b <- gapTermAllGum  
-- 	   t2 <- getClockTime
-- 	   putStrLn (show (diffClockTimes t2 t1))
	   hFlush stderr
	   return()

---------------------------------------------------------------------------
solveProblem :: String -> [String] -> [String]
-- solveProblem :: String -> [String] -> [GapObject]
solveProblem fname myList = parallelMap calculate myList
    where
    calculate x = gapObject2String(gapEval fname  [string2GapExpr x])  
    -- I separated the skeleton and the number of workers for a better view of the options:
    -- use all processors, but reserve one for the master (executing this code)
    np          = noPe
    -- simple: one process per job
    -- parallelMap = edenParMap
    -- farm: np processes,static task distribution
    parallelMap f = farm np unshuffleN shuffle (process (map f))
    -- "workpool" skeleton (see TFP07 paper), good for irregular tasks
    -- parallelMap = sortmw np c 

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

-- Eden parmap:
edenParMap :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
edenParMap f xs = unsafePerformIO ( mapM (instantiate (process f)) xs )

-- version with process parameter:
edenParMap' :: (Trans a, Trans b) => (Process a b) -> [a] -> [b]
edenParMap' proc xs = unsafePerformIO (mapM (instantiate proc) xs)

-- Eden process farm (np processes working on whole lists)
farm :: (Trans a, Trans b) => 
                Int -> (Int -> [a] -> [[a]]) -- n, distribute
                    -> ([[b]] -> [b])        -- combine
                    -> Process [a] [b]       -- worker process
                    -> [a] -> [b]            -- what to do
farm np distr combine p inputs = 
        combine (edenParMap' p (distr np inputs))



-- functions to distribute and combine a list
---------------------------------------------
-- Helper: take each  n-th element
takeEach :: Int -> [a] -> [a]
takeEach n [] = []
takeEach n (x:xs) = x : (takeEach n (drop (n-1) xs))

-- unshuffleN splits a list into n lists (round-robin)
unshuffleN :: Int -> [a] -> [[a]]
-- simple: unshuffleN n xs = [takeEach n (drop i xs) | i <- [0..(n-1)]] 
-- optimised by UK (2001):
unshuffleN n xs = unshuffle xs
                where  unshuffle xs = map (f xs) [0..n-1]
                                where f xs i = g (drop i xs)
                                      g [] = []
                                      g xs = head xs : (g (drop n xs))
-- inverse: 
-- shuffle . unshuffle == id
shuffle :: [[a]] -> [a]
shuffle = concat . transpose



---------------------------------------------------------------------------
