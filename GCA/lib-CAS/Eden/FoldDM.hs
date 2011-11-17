{-# OPTIONS -cpp #-}
module FoldDM (fold_map_dm, using, spine, whnfspine, unshuffle) where 

import Eden

#ifndef BAD_E6
import System.IO.Unsafe
import Control.Monad
#endif

infixr 2 `using` 

using x s = s x `seq` x

spine [] = ()
spine (x:xs) = spine xs

whnfspine [] = ()
whnfspine (x:xs) = x `seq` whnfspine xs

unshuffle :: Int -> [a] -> [[a]]
unshuffle n xs = [takeEach n (drop i xs) | i <- [0..n-1]]
 where takeEach :: Int -> [a] -> [a]
       takeEach n [] = []
       takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)

parMap_dm :: (Trans b) => (a -> b) -> [a] -> [b]
#ifdef BAD_E6
parMap_dm f xs = let insts = [ createProcess (process (\() -> f x)) () 
			       | x <- xs] `using` whnfspine
		 in map deLift insts
#else
parMap_dm f xs = unsafePerformIO (zipWithM instantiate 
		                  [ process (\() -> f x) | x <- xs]
				  (repeat ())
		                  )
#endif

fold_map_dm :: Trans b => Int -> (a->b) -> (b->b->b) -> b -> [a] -> b
fold_map_dm thr
 | noPe > thr = fmDM (noPe-1) 
 | otherwise  = fmDM noPe
 
fmDM :: Trans b => Int ->  (a->b) -> (b->b->b) -> b -> [a] -> b
fmDM np f comb cero tasks = foldl comb cero subresults
 where subresults = parMap_dm (\xs -> foldl comb cero (map f xs)) sublists
       sublists   = [ (unshuffle np tasks)!!i | i <- [0,1..np-1]]
{-
[createProcess (worker pid np f comb cero tasks) () | pid <- [0,1..np-1]] `using`  whnfspine
		    -- JB: use createProcess instead of (***)

worker pid np f comb cero tasks = process (\() -> foldl' comb cero subresults)
 where subresults = map f (extract pid np tasks)
       extract i np ts = (unshuffle np ts) !! i
-}

foldl' :: Trans a => (a->b->a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = y `seq` foldl' f y xs
 where y = f a x 

--  LocalWords:  Infixr
