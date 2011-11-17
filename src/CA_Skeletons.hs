{-# OPTIONS_GHC -cpp -XParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-} 
-- Time-stamp: <Sat Jun 26 2010 03:18:45 Stardate: Stardate: [-28]3280.27 hwloidl>
--
-- Some skeletons for computer algebra computations, based on Eden skeletons.
----------------------------------------------------------------------------------

module CA_Skeletons where

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
import Data.List hiding (foldl', foldl1')
import Data.Set hiding (map, null)
import SGPTypes

import Debug.Trace
-----------------------
instance NFData (Arith)
instance Trans (Arith)



-----------------------------------------
-- special skeleton for SCSCP calls 
--
parMHISCSCP :: CAName         -- map input to homomorphic image
             -> CAName         -- solver
             -> CAName         -- combiner
             -> [ Integer ]        -- hom. imgs. to use
             -> Arith          -- vector and matrix -- neutral elem in the overall domain
             -- -> Arith          -- vector
             -> Arith          -- result
parMHISCSCP h f g ps {-e'-} m
  = multHomImgEden (call2 h) (call2 f) (call2 g) ps {-e'-} m

multHomImgEden :: -- :: (Trans p, Trans c', Trans b') =>
              (Integer -> Arith -> Arith ) ->  -- map input to homomorphic images
              (Integer -> Arith -> Arith) -> -- solve the problem in the hom. imgs.
              ([Integer] -> Arith -> Arith) ->  -- combine the results to an overall result              
              [ Integer ] ->             -- hom. imgs. to use. NOTE - will be supplied in a vector format!
              Arith ->               -- input (matrix, vector)
              Arith              -- result-}
multHomImgEden h f g ps x =
 res
 where
    xList    = zipWith h ps (repeat x)
    resL     = parZipWith f ps xList
    res      = g ps (toOMMatrix resL)

toOMMatrix xs = (Matrix xs)
toOMList xs = SGPTypes.List xs

fromOMList (List xs) = xs
fromOMList (Matrix xs) = xs
fromOMList (MatrixRow xs) = xs


parZipWith f primes list2 -- list3
  = newTasks
    where
      workerProcs = [process (zip [n,n..] . (worker f)) | n <- [1..noPe] ]

      (newReqs, newTasks) = (unzip . merge) (zipWith ( # ) workerProcs (distributeLists (primes, list2) requests)) -- (zipWith (#) workerProcs (distributeLists (list1, list2) requests))

      requests = (concat (replicate 2 [1..noPe])) ++ newReqs

      -- worker f [] = []
      -- worker :: (Trans a, Trans b) => (a -> b -> c) -> [(a,b)] -> [c]
      worker f [] = []
      worker f ((p, t2) : ts) = if res == List [Num 0] then worker f ts
                                                else res : worker f ts
                                   where res = f p t2

      -- distributeLists :: ([t], [t]) -> [Int] -> [[(t,t)]]
      distributeLists tasks reqs = [ taskList reqs tasks n | n <- [ 1 .. noPe ] ]
        where
           taskList (r:rs) ( p:ps, t2:ts2 ) pe | pe == r   = (p, t2) : (taskList rs (ps, ts2) pe)
                                               | otherwise =      taskList rs (ps, ts2) pe
           taskList _      _                _  = []




-- parOrbitSCSCP :: [CAName] -> [ [Int] ] -> Int -> [ [Int] ] 
-- parOrbitSCSCP :: [CAName] -> [ [Arith] ] -> Arith -> [ [Arith] ]
parOrbitSCSCP :: [CAName] -> [ Arith ] -> Arith -> [ Arith ]
--parOrbitSCSCP (g:gens) init s = [ [ process ((call2 g)) init) # s   ] ]
parOrbitSCSCP gens init s = orbitPar 2 (orbitOnList' s) (map ParSCSCP.call2'' gens) init


orbitOnList' :: Arith -> [Arith -> Arith -> Arith] -> Arith -> [Arith]
orbitOnList' s [] _ = []
orbitOnList' s (g:gens) list = {-Debug.Trace.trace (show (length (g:gens)) ++ "sending " ++ show list ++ "res in or': " ++ show res)-} res 
    where res = g s list : orbitOnList' s gens list

-- orbitOnList :: Int -> [Int -> Int -> Int] -> [Int] -> [[Int]]
{-orbitOnList :: Arith -> [Arith -> Arith -> Arith] -> [Arith] -> [Arith]
orbitOnList s [] _ = []
orbitOnList s (g:gens) list = Debug.Trace.trace (show (length (g:gens)) ++ show list ++ "res: " ++ show res) res 
    where res = (Data.List.map (g s) list : orbitOnList s gens list) -}

orbitPar prefetch orbitfun gens init = dat -- (process (concat . map (orbitfun gens)) # (toWorker dat) -- nub newTasks)
    where
        nGens = length gens

        (newReqs, newTasks) = (unzip . merge) (zipWith (#) workerProcs (toWorker dat))

        dat = (addNewTask empty (init' ++ newTasks) (length init'))

        initialReqs = concat (replicate prefetch [1..noPe])

        init' = take noPe (cycle init)

        addNewTask set (t:ts) c
          | not (t `member` set) = t : (addNewTask (Data.Set.insert t set) ts c')
          | c <= 1             = [] 
          | otherwise          = addNewTask set ts (c - 1)
                                 where c' = (c-1) + nGens

        workerProcs = [ process (zip [n,n..] . (concat . (Data.List.map (orbitfun gens)))) | n <- [1..noPe] ]
          
        toWorker tasks = distribute tasks requests
          
        requests = initialReqs ++ newReqs


        distribute :: [t] -> [Int] -> [[t]]
        distribute tasks reqs = [taskList reqs tasks n | n<-[1..noPe]]
          where taskList (r:rs) (t:ts) pe | pe == r    = t:(taskList rs ts pe)
                                          | otherwise  =    taskList rs ts pe
                taskList _      _      _  = []


parMapSCSCP :: (Trans a, Trans b, OMData a, OMData b) =>
               CAName ->   -- (a -> b) 
               [a] ->      -- input, 
               [b]         -- result
parMapSCSCP mapF = parmapfarm noPe (call1 mapF)

-- parmap in EdI 
parMap' :: (Trans a, Trans b) => (Process a b) -> [a] -> [b]
parMap' proc xs = runParallel (mapM (instantiate proc) xs)

-- Eden process farm (np processes, each working on a whole sublist)
parmapfarm :: (Trans a, Trans b) => 
                Int -> (a -> b)         -- worker process
		    -> [a] -> [b]       -- what to do
parmapfarm np f inputs = 
        shuffle (parMap' (process (map f)) (unshuffle np inputs))

-- potential optimisation: use explicit IO inside the processes, write
-- out chunks of pcalls instead of one by one. Or: make reply threads
-- work!

-- imported from FoldDM

fold_map_dm :: Trans b => Int -> (a->b) -> (b->b->b) -> b -> [a] -> b
fold_map_dm thr
 | noPe > thr = fmDM (noPe-1) 
 | otherwise  = fmDM noPe
 
fmDM :: Trans b => Int ->  (a->b) -> (b->b->b) -> b -> [a] -> b
fmDM np f comb cero tasks = foldl' comb cero (map deLift subresults) -- JB: remove Lift ...
 where subresults = [createProcess (worker pid np f comb cero tasks) () | pid <- [0,1..np-1]] `using` rseq 
		    -- JB: use createProcess instead of (***)

worker pid np f comb cero tasks = process (\() -> foldl' comb cero subresults)
 where subresults = map f (extract pid np tasks)
       extract i np ts = (unshuffle np ts) !! i

       extract i np ts = (unshuffle np ts) !! i

foldl' :: Trans a => (a->b->a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = y `seq` foldl' f y xs
 where y = f a x 

foldl1' :: Trans a => (a->a->a) -> [a] -> a
foldl1' f [] = error "foldl1: empty list"
foldl1' f [x] = x
foldl1' f (x:(ys@(y:xs))) = foldl' f x ys

-- master worker... 
-- import this from EdiWP.

-----------------------------------
-- fold only
-- 
parFoldSCSCP_,
 parFoldSCSCP  :: (Trans b, OMData b) =>
                   CAName ->   -- (b -> b -> b) 
                   b -> [b] -> -- neutral, input, 
                   b           -- result
parFoldSCSCP foldF neutral inputs = scscpFoldl subRs
    where subRs = runParallel $ do 
                    sequence [ instantiateAt i 
                                 (process scscpFoldl) 
                                 ins 
                              | ins <- unshuffle noPe inputs
                              | i   <- [1..noPe] ]
          scscpFoldl = foldl' (call2 foldF) neutral 
          -- non-optimised, moves accumulator around

parFoldSCSCP_ foldF neutral inputs 
    = scscpFoldlStart neutral subRs -- final stage with acc. reference as well
    where subRs = runParallel $ do 
                    sequence [ instantiateAt i 
                                 (process (scscpFoldlStart neutral)) 
                                 ins 
                              | ins <- unshuffle noPe inputs
                              | i   <- [1..noPe] ]
          -- store intermediate results (acc.) inside the CA system
          scscpFoldlStart :: (Trans a, OMData a) => a -> [a] -> a
          scscpFoldlStart n [] = n
          scscpFoldlStart n xs = let nRef = call1R (Right StoreObj) n
                                 in nRef `seq` scscpFoldl nRef xs  
          scscpFoldl :: (OMData a, Trans a) => CARef -> [a] -> a
          scscpFoldl x []     = call1 (Right RetrieveObj) x
          scscpFoldl x (y:ys) = let x' = (call2R foldF x y)
                                in  x' `seq` scscpFoldl x' ys


-- map and fold
parMapFoldSCSCP,
 parMapFoldSCSCP_ :: (Trans a, Trans b, OMData a, OMData b) =>
                     CAName ->   -- (a -> b) 
                     CAName ->   -- (b -> b -> b) 
                     b -> [a] -> -- neutral, input, 
                     b           -- result
parMapFoldSCSCP mapF foldF neutral inputs = scscpFoldl neutral subRs
    where subRs = runParallel $ do 
                    sequence [ instantiateAt i (process mapFoldSCSCP) ins 
                              | ins <- unshuffle noPe inputs
                              | i   <- [1..noPe] ]
          mapFoldSCSCP [] = neutral
          mapFoldSCSCP xs = scscpFoldl neutral (map (call1 mapF) xs)
                            -- TODO: optimise this, by storing the map
                            -- results inside the CA system and using
                            -- a foldF :: [b] -> b which operates on
                            -- whole lists.
--          scscpFoldl x []    = x
--          scscpFoldl x (y:ys) = let x' = (call2 foldF x y)
--                                in  x' `seq` scscpFoldl x' ys
          scscpFoldl = foldl' (call2 foldF)

-- map and fold
parMapFold1SCSCP  :: (Trans a, Trans b, OMData a, OMData b) =>
                     CAName ->   -- (a -> b) 
                     CAName ->   -- (b -> b -> b) 
                     [a] ->      -- input, 
                     b           -- result
parMapFold1SCSCP mapF foldF inputs = scscpFoldl1 subRs
    where subRs = runParallel $ do 
                    sequence [ instantiateAt i (process mapFold1SCSCP) ins 
                              | ins <- unshuffle noPe inputs
                              | i   <- [1..noPe] ]
          mapFold1SCSCP [] = error "mapFold1SCSCP: Empty list"
          mapFold1SCSCP xs = scscpFoldl1 (map (call1 mapF) xs)
                            -- TODO: optimise this, by storing the map
                            -- results inside the CA system and using
                            -- a foldF :: [b] -> b which operates on
                            -- whole lists.
--          scscpFoldl1 x []    = x
--          scscpFoldl1 x (y:ys) = let x' = (call2 foldF x y)
--                                in  x' `seq` scscpFoldl x' ys
          scscpFoldl1 = foldl1' (call2 foldF)


-- map and fold
parMapFoldSCSCP_ mapF foldF neutral inputs 
    = scscpFoldl nRef subRs
    where nRef  = call1R (Right StoreObj) neutral
          subRs = runParallel $ do 
                    sequence [ instantiateAt i 
                                (process (mapFoldSCSCP neutral)) ins 
                              | ins <- unshuffle noPe inputs
                              | i   <- [1..noPe] ]
          -- mapFoldSCSCP [] = neutral
          -- use foldF :: [b] -> b which operates on whole lists?
--          mapFoldSCSCP :: (OMData a, Trans a) => a -> [a] -> a
          mapFoldSCSCP n xs = scscpFoldlRef n (map (call1R mapF) xs)
          -- store intermediate results (acc.) inside the CA system
          scscpFoldlRef :: (OMData a) => a -> [CARef] -> a
          scscpFoldlRef n [] = n
          scscpFoldlRef n xs = let nRef = call1R (Right StoreObj) neutral
                               in nRef `seq` scscpFoldlR nRef xs  
          -- arguments are stored as references (cookies):
          scscpFoldlR :: (OMData a) => CARef -> [CARef] -> a
          scscpFoldlR x []     =  call1 (Right RetrieveObj) x
          scscpFoldlR x (y:ys) = let x' = (call2R foldF x y)
                                 in  x' `seq` scscpFoldlR x' ys
          -- arguments are real data:
          scscpFoldl :: (OMData a, Trans a) => CARef -> [a] -> a
          scscpFoldl x []     = call1 (Right RetrieveObj) x
          scscpFoldl x (y:ys) = let x' = (call2R foldF x y)
                                in  x' `seq` scscpFoldl x' ys

-- zipWith -- UNTESTED!!!
parZipWithSCSCP   :: (Trans a, Trans b, Trans c, OMData a, OMData b, OMData c) =>
                     CAName ->   -- (a -> b -> c) 
                     [a] ->      -- input list 1 
                     [b] ->      -- input list 2 
                     [c]         -- result list 
parZipWithSCSCP f inputs1 inputs2 = shuffle subRs
    where subRs = runParallel $ do 
                    sequence [ instantiateAt i (process (uncurry scscpBlock)) (ins1, ins2)
                              | ins1 <- unshuffle noPe inputs1
                              | ins2 <- unshuffle noPe inputs2
                              | i   <- [1..noPe] ]

          scscpBlock xs ys = zipWith (call2 f) xs ys

# else /* not parallel */

mapSCSCP :: (OMData a, OMData b) =>  -- (Trans a, Trans b, OMData a, OMData b) =>
               CAName ->   -- (a -> b) 
               [a] ->      -- input, 
               [b]         -- result
mapSCSCP f = map (call1 f)
             -- where singleton x = [x]
foldSCSCP :: (OMData b) =>   -- (Trans b, OMData b) =>
                   CAName ->   -- (b -> b -> b) 
                   b -> [b] -> -- neutral, input, 
                   b           -- result
foldSCSCP foldF neutral inputs = scscpFoldl inputs
    where {- subRs = unsafePerformIO $ do 
                    sequence [ instantiateAt i 
                                 (process scscpFoldl) 
                                 ins 
                              | ins <- unshuffle noPe inputs
          -- non-optimised, moves accumulator around

-- HWL HACK
foldl' :: (a->b->a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = y `seq` foldl' f y xs
 where y = f a x 

#endif

-- Eden boilerplate code... 
unshuffle :: Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
 where takeEach :: Int -> [a] -> [a]
       takeEach n [] = []
       takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)
shuffle :: [[b]] -> [b]
-- shuffle = concat . transpose
   -- this impl. sequentially evaluation one input list after
   -- the other
-- for Eden we need a version, that produces the first outputs as fast
--  as possible, i. e. evaluates all input lists concurrently:
shuffle xxs
	| and (map null xxs) = []
	| otherwise = (mymaphead xxs) ++ ( shuffle (map mytail xxs))
		 where mymaphead [] = []
		       mymaphead ([]:xxs) = mymaphead xxs
		       mymaphead ((x:xs):xxs) = x : mymaphead xxs
		       mytail [] = []
		       mytail xs = tail xs

{-
takeEach :: Int -> [a] -> [[a]]
takeEach n [] = []
takeEach n xs = let (start,rest) = splitAt n xs
                in start : takeEach n rest

-}

call2' :: (OMData a) =>
         CAName -> a -> a -> a
call2' name x y = fromOM (callSCSCP name [toOM x, toOM y])

-- sad, sequential versions of the skeletons
mapSCSCP :: (OMData a, OMData b, Trans b) => 
            CAName ->   -- (a -> b) 
            [a] ->      -- input, 
            [b]         -- result
mapSCSCP mapF = map (call1 mapF)


mapFoldSCSCP :: (OMData a, OMData b, Trans a, Trans b) => 
                CAName ->   -- (a -> b) 
                CAName ->   -- (b -> b -> b) 
                b -> [a] -> -- neutral, input, 
                b           -- result
mapFoldSCSCP mapF foldF neutral inputs = foldl' (call2' foldF) neutral (map (call1 mapF) inputs)

mapFold1SCSCP  :: (OMData a, OMData b, Trans b) => 
                  CAName ->   -- (a -> b) 
                  CAName ->   -- (b -> b -> b) 
                  [a] ->      -- input, 
                  b           -- result
mapFold1SCSCP mapF foldF inputs = foldl1' (call2' foldF) (map (call1 mapF) inputs)


zipWithSCSCP   :: (OMData a, OMData b, OMData c) => 
                  CAName ->   -- (a -> b -> c) 
                  [a] ->      -- input list 1 
                  [b] ->      -- input list 2 
                  [c]         -- result list 
zipWithSCSCP f = zipWith (call2 f) 


