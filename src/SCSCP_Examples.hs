{-# OPTIONS_GHC -cpp -XParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-} 
-- Time-stamp: <Sat Jun 26 2010 03:17:34 Stardate: Stardate: [-28]3280.26 hwloidl>
--
-- Several simple examples of how to compose and use SCSCP calls on Haskell side
-----------------------------------------------------------------------------

module SCSCP_Examples where

-- this contains the main SCSCP functionality
#ifdef __PARALLEL_HASKELL__ 
import ParSCSCP
#else
import SCSCP_API
#endif
import HS_SCSCP
import HS2SCSCP

#ifdef __PARALLEL_HASKELL__ 
-- import EdenHelpers -- helper functions
-- import FoldDM
import Control.Parallel.Eden 
#endif

#ifdef __PARALLEL_HASKELL__ 
import System.IO.Unsafe
#endif

import BaseServices
import CA_Skeletons
-- import Control.Parallel.Strategies

-----------------------------------------------------------------------------
-- SCSCP interface for euler numbers

eulerWithSCSCP :: Int -> IO Int
eulerWithSCSCP n = do
  -- do the computation ...
  let fName = scscp_WS_Phi
  let args  = map toOM [n]
  let resOM = callSCSCP fName args
  -- print the result
  let x = (fromOM resOM) :: Int
  return x

sumEulerWithSCSCP :: Int -> IO Int
sumEulerWithSCSCP n = do
   xs <- mapM eulerWithSCSCP [1..n]
   return (sum xs)

sumEulerListSCSCP :: [Int] -> IO Int
sumEulerListSCSCP ns = do
   xs <- mapM eulerWithSCSCP ns
   return (sum xs)
                      
sumEulerFromToWithSCSCP :: Int -> Int -> IO Int
sumEulerFromToWithSCSCP m n = do
   xs <- mapM eulerWithSCSCP [m..n]
   return (sum xs)

-----------------------------------------------------------------------------
-- top level sumEuler function

-- version directly taken from GpH examples; passes lists
sumEulerParSCSCP' :: Int -> Int -> IO Int
sumEulerParSCSCP' n c = do
  let ns = [1..n]            -- [1,2,..,n]
  let z = noFromSize c n     -- number of blocks of size c
  let nss = unshuffleN z ns  -- [(1,c), ...]
  let xs' = map (createProcess (process (\ns -> 
                  unsafePerformIO (sumEulerList ns)))) nss
  let xs :: [Int]
      xs =  map deLift xs'
  return (sum xs)

sumEulerList :: [Int] -> IO Int
sumEulerList  = return . fromOM . {- unsafePerformIO . -} (callSCSCP scscp_WS_sumEulerList) . (map toOM)

-- NB: this passes ranges, not entire lists, via SCSCP calls
sumEulerParSCSCP :: Int -> Int -> IO Int
sumEulerParSCSCP n c = do
  let ranges = [ [i*c+1, (i+1)*c] | i <- [0..(noFromSize c n)-1] ] 
  let xs' = map (createProcess (process (\ns -> 
                  unsafePerformIO (sumEulerRange ns)))) ranges
            `using` whnfspine
  let xs :: [Int]
      xs =  map deLift xs'
  return (sum xs)

sumEulerRange :: [Int] -> IO Int
sumEulerRange  = return . 
  fromOM . (callSCSCP scscp_WS_SumEulerRange) . (map toOM)

-- sumEulerClassic :: [Int] -> IO Int
-- sumEulerClassic  = return . fromOM . {- unsafePerformIO . -} (callSCSCP scscp_WS_sumEulerClassic) . (map toOM)

-- no splitting; just does a sequence of sumEulers entirely on GAP side
-- entire sumEuler (not just euler) on GAP side; computes sumEuler n, n<-ns
sumEulerClassicSCSCP :: [Int] -> IO Int                      
sumEulerClassicSCSCP ns = do
     let xs' = map (createProcess (process (callSCSCP scscp_WS_sumEulerClassic)) . (map toOM)) (map (\ n -> [1,n]) ns)
               `using` whnfspine
     let xs :: [Int]
         xs = map (fromOM . deLift) xs'
     return (sum xs)

-------------------------------------------------------
-- purely Haskell side code

sumEuler_seq :: Int -> Int
sumEuler_seq = sum . map euler . enumFromTo (1::Int)

---------------------------------------------------------------------------
-- main fct

euler :: Int -> Int
euler n = length (filter (relprime n) [1..(n-1)])

---------------------------------------------------------------------------
-- aux fcts
hcf     :: Int -> Int -> Int
hcf x 0 = x
hcf x y = hcf y (rem x y)

relprime     :: Int -> Int -> Bool
relprime x y = hcf x y == 1

---------------------------------------------------------------------------

-- produce a list of input values
mkList :: Int -> [Int]
mkList = reverse . enumFromTo 1 
-- random numbers
-- mkList seed n = take n (randoms seed)

-----------------------------------------------------------------------------
-- parallel variants

#ifdef __PARALLEL_HASKELL__ 

-- main fct
{-
sumEuler_dm     :: Int -> Int
sumEuler_dm n = fold_map_dm 10 euler (+) 0 (mkList n) 
-}

-- 
sum' :: Num a => [Lift a] -> a
sum' = sum . (map deLift)

sum'' :: Num a => [Lift (IO a)] -> a
sum'' = sum . (map (unsafePerformIO . deLift))

sumEulerShuffle :: Int -> Int -> Int
sumEulerShuffle c n = length xs `seq` sum' xs
                      where xs = [ createProcess (process (\x -> (sum . map euler) x)) chunk
                                 | chunk <- unshuffleN (noFromSize c n) (mkList n) ] 
				   `using` whnfspine

-- evaluate exactly 2 sumEuler instances in parallel
sumEuler2SCSCP :: Int -> Int -> IO Int
sumEuler2SCSCP m n =  do
     let p1 = createProcess (process (callSCSCP scscp_WS_sumEulerClassic)) (map toOM [1,m])
     let p2 = createProcess (process (callSCSCP scscp_WS_sumEulerClassic)) (map toOM [1,n])
     let x :: Int
         x = (fromOM . deLift) p1
     let y :: Int
         y = (fromOM . deLift) p2
     return ((x+y) `using` \ _ -> whnfspine [p1,p2])

-----------------------------------------------------------------------------
-- skeletons
-- abstracts over sumEuler2SCSCP

parProcesses1 :: String -> [OMObj] -> IO OMObj
parProcesses1 f xs'  = do
     let fName = Left ("scscp_transient_1", f)
     let xOM = callSCSCP fName xs' 
     return xOM

parProcesses2 :: String -> String -> [Integer] -> [Integer] -> IO Integer
parProcesses2 f g xs ys = do
     let fName = Left ("scscp_transient_1", f)
     let gName = Left ("scscp_transient_1", g)
     let p1 = createProcess (process (callSCSCP fName)) (map toOM xs)
     let p2 = createProcess (process (callSCSCP fName)) (map toOM ys)
     let x :: Integer  -- hard-wired result type
         x = (fromOM . deLift) p1
     let y :: Integer  -- hard-wired result type
         y = (fromOM . deLift) p2
     {-
     let gName = Left ("scscp_transient_1", g)
     let z :: Int
         z = fromOM (callSCSCP gName (map toOM [x, y]))
     -}
     let z = x+y -- HACK: no g for now
     return (z `using` \ _ -> whnfspine [p1,p2])

-- abstracts over sumEulerClassic
parProcesses :: (OMData a) => [CAName] -> ([a] -> a) -> [[a]] -> IO a
parProcesses fs g xss = do
     let ps = [ createProcess (process (callSCSCP f)) (map toOM xs) 
              | f <- fs | xs <- xss ]
               `using` whnfspine
     let -- xs :: [a]
         xs = map (fromOM . deLift) ps
     return (g xs)

-- hand-rolled parallel list skeleton
parListSCSCP :: String -> [Int] -> IO [Int]
parListSCSCP f ns = do
     putStrLn $ "parList "++f++" "++(show ns)++" i.e. using parmapfarm to apply service in parallel to all list elements"
     let fName = Left ("scscp_transient_1", f)
     let ys :: [Int]
         ys = parmapfarm 1 (\ n -> fromOM (callSCSCP fName [n])) (map toOM ns)
     return (ys `using` \ _ -> whnfspine ys)

{- no parallelism in this version; needs `using` whnfspine or such

                       -- let l = length xs `seq` sum' xs
                       let ns = (mkList n) 
		       putStrLn $ "sumEuler2SCSCP "++(show m)++" "++(show n)++" ..."
                       -- let x = deLift $ createProcess (process (\n -> unsafePerformIO (eulerWithSCSCP n))) m
                       x <- sumEulerClassicSCSCP 1 m
                       let y = deLift $ createProcess (process (\n -> unsafePerformIO (sumEulerClassicSCSCP 1 n))) n
                       return (x+y)
-}

-- parMap and parMapFold skeletons: see parMapSCSCP and parMapFoldSCSCP below

-----------------------------------------------------------------------------

-- parallel process for every number and using SCSCP for every number
sumEulerNaiveSCSCP :: Int -> IO Int
sumEulerNaiveSCSCP n =  do
                             -- let l = length xs `seq` sum' xs
                             let ns = (mkList n) 
		             putStrLn $ "sumEulerNaiveSCSCP "++(show n)++" ..."
                             let xs = map (createProcess (process (\n -> unsafePerformIO (eulerWithSCSCP n)))) ns
                             -- xs <- mapM eulerWithSCSCP ns
                             -- return (sum'' xs)
                             return (sum' xs)

-- similar to sumEulerShuffle, but using SCSCP for every number
sumEulerShuffleSCSCP :: Int -> Int -> IO Int
sumEulerShuffleSCSCP n c =  do
                             -- let l = length xs `seq` sum' xs
                             let ns = (mkList n) 
                             let z = (noFromSize c n)
                             let nss = unshuffleN z ns
		             putStrLn $ "sumEulerShuffleSCSCP "++(show c)++" "++(show n)++" i.e. "++(show z)++" clusters ..."
                             let xs = map (createProcess (process (\ns -> unsafePerformIO (sumEulerListSCSCP ns)))) nss
                             -- let xs = map (createProcess (process (\n -> unsafePerformIO (eulerWithSCSCP n)))) ns
                             -- xs <- mapM eulerWithSCSCP ns
                             -- return (sum'' xs)
                             let res = (sum' xs)
		             putStrLn $ "Result: "++(show res)
                             return res

-- similar to sumEulerShuffle, but using SCSCP for every number
sumEulerParMapSCSCP :: Int -> Int -> IO Int
sumEulerParMapSCSCP n c =  do
                             -- let l = length xs `seq` sum' xs
                             let ns = (mkList n) 
                             let z = (noFromSize c n)
                             -- let nss = unshuffleN z ns
		             putStrLn $ "sumEulerParMapSCSCP "++(show c)++" "++(show n)++" i.e. using parmapfarm of sumEulerSCSCP on "++(show z)++" clusters ..."
                             let xs = parmapfarm z (\ n -> unsafePerformIO (eulerWithSCSCP n)) ns
                             let res = (sum xs)
		             putStrLn $ "Result: "++(show res)
                             return res
#endif

noFromSize c n | n `mod` c == 0 = n `div` c 
               | otherwise      = n `div` c + 1


-- these should come from EdenHelpers

shuffleN :: [[b]] -> [b]
-- shuffleN = concat . transpose
		-- this impl. sequentially evaluation on input list after
		-- the other
-- for Eden we need a version, that produces the first outputs as fast
--  as possible, i. e. evaluates all input lists concurrently:
shuffleN xxs
	| and (map null xxs) = []
	| otherwise = (mymaphead xxs) ++ ( shuffleN (map mytail xxs))
		 where mymaphead [] = []
		       mymaphead ([]:xxs) = mymaphead xxs
		       mymaphead ((x:xs):xxs) = x : mymaphead xxs
		       mytail [] = []
		       mytail xs = tail xs



unshuffleN :: Int -> [a] -> [[a]]
unshuffleN n xs = unshuffle xs
		where  unshuffle xs = map (f xs) [0..n-1]
				where f xs i = g (drop i xs)
				      g [] = []
				      g xs = head xs : (g (drop n xs))


-- demand control helper
whnfspine :: [a] -> ()
whnfspine [] = ()
whnfspine (x:xs) = x `seq` whnfspine xs


