module Main where
import System(getArgs)
import IO
import System.Time
import Control.Parallel.Strategies
import Control.Parallel
import GapAPI
import EdenSkel
import Eden		-- Enabling remote processes in a functional style.
import System.IO.Unsafe(unsafePerformIO) -- for Eden-6 skeletons below
import Control.Monad
import List(transpose, sortBy)

---------------------------------------------------------------------------

main :: IO ()
main =  do args <- getArgs
	   let
             fn = getWord (args!!0) :: String
	     chunk = read (args!!1) :: Int
	     value = argsList(args)
	   a <- gapInitAllGum
-- 	   hPutStrLn stderr (" Function " ++ fn ++ " and the args:" ++ (show value) ++ " value length: " ++ show(length(value)) ++ " ")
	   hFlush stderr
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
solveProblem fname c myList = parallelMap calculate myList
    where
    calculate x = gapObject2String(gapEval fname  [string2GapExpr x])  
    -- I separated the skeleton and the number of workers for a better view of the options:
    -- use all processors, but reserve one for the master (executing this code)
    np          = noPe
    -- simple: one process per job
    -- parallelMap = edenParMap
    -- farm: np processes,static task distribution
    -- parallelMap f = farm np unshuffleN shuffle (process (map f))
    -- "workpool" skeleton (see TFP07 paper), good for irregular tasks
    parallelMap = sortmw np c 

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



-- amw :: (Trans t, Trans r) => Int -> Int -> (t -> r) -> [t] -> [r] 
-- amw x y f ls = fix (mw x y f' ls')
-- 	       where
-- 	         ls' = zip ls [1..]
-- 		 f' :: (Trans t, Trans r) => (t, Int) -> (r, Int)  
-- 		 f' (v, t) = (f v, t)
-- 		 order (_,a) (_,b) = compare a b
-- 		 fix ls = sortBy order ls 



-- workpool (alias "master-worker") taken from TFP paper:
spawn :: (Trans a, Trans b) => [Process a b] -> [a] -> [b]
spawn ps is = unsafePerformIO (zipWithM (instantiateAt 0) ps is)


mw :: (Trans t, Trans r) => Int -> Int -> (t -> r) -> [t] -> [r]
mw n prefetch wf tasks = ress
  where
   (reqs, ress) =  (unzip . merge) (spawn workers inputs)
   -- workers   :: [Process [t] [(Int,r)]]
   workers      =  [process (zip [i,i..] . map wf) | i <- [0..n-1]]
   inputs       =  distribute n tasks (initReqs ++ reqs)
   initReqs     =  concat (replicate prefetch [0..n-1])

-- task distribution according to worker requests
distribute               :: Int -> [t] -> [Int] -> [[t]]
distribute np tasks reqs = [taskList reqs tasks n | n<-[0..np-1]]
    where taskList (r:rs) (t:ts) pe | pe == r    = t:(taskList rs ts pe)
                                    | otherwise  =    taskList rs ts pe
          taskList _      _      _  = []



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

--
-- functions to sort the results
--

tagTasks :: [t] -> [(Int,t)]
tagTasks ts = zip [1..] ts


-- sortRes :: [(Int,r)] -> [(Int,r)]
-- sortRes rsTagged = let ordering (a,_) (b,_) = compare a b
--                    in sortBy ordering rsTagged

sortRes :: [(Int,r)] -> [r]
sortRes rsTagged = let ordering (a,_) (b,_) = compare a b
                   in map snd (sortBy ordering rsTagged)



sortmw :: (Trans t, Trans r) => Int -> Int -> (t->r) -> [t] -> [r]
sortmw np prefetch workerF tasks =
    let taggedTasks = tagTasks tasks
	tagWorkerF (tag,t) = (tag, workerF t)

        in sortRes (mw np prefetch tagWorkerF taggedTasks)
---------------------------------------------------------------------------
