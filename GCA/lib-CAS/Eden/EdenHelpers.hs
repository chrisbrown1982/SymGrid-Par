-- JB: new module joining all helper functions
--  in the old Eden distribution...
-- this is the place to optimize (-eeager) process instantiation
--   (nothing to do, eagerInstList has already been optimized!)

module EdenHelpers where

import Eden

-- BaseProcs.lhs
-- Helper functions frequently used for splitting and joining streams:
-- ===================================================================

{- bresenham computes [i1, ..., ip] such that i1 + ... + ip = n
		and | ij - ik | <= 1, for all 1 <= j,k <= n
    (from computer graphics for printing smooth lines)
-}
bresenham :: Int -> Int -> [Int]
bresenham n p = take p (bresenham1 n)
              where bresenham1 m = (m `div` p) : bresenham1 ((m `mod` p)+ n)

splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = f bh xs
		where bh = bresenham (length xs) n
		      f [] [] = []
		      f [] _  = error "some elements left over"
		      f (t:ts) xs = hs : (f ts rest)
				  where (hs,rest) = splitAt t xs

splitAtN :: Int -> [a] -> [[a]]
splitAtN n [] = []
splitAtN n xs = ys : splitAtN n zs
	      where (ys,zs) = splitAt n xs

takeEach :: Int -> [a] -> [a]
takeEach n [] = []
takeEach n (x:xs) = x : (takeEach n (drop (n-1) xs))

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

-- mergeRunsBy:
--  use a binary combination scheme to merge the each runs into one,
--  this ensures that the smallest runs are merged first.
mergeRunsBy :: (a -> a -> Bool) -> [[a]] -> [a]
mergeRunsBy _ [] = []
mergeRunsBy _ [xs] = xs
{- different behaviour of zipWith in Haskell98!!!
mergeRunsBy cmd [xs,ys] = mergeRunsBy2 cmd xs ys
mergeRunsBy cmp xxs = mergeRunsBy cmp
			(zipWith (mergeRunsBy2 cmp) (odds) (evens))
		    where odds = takeEach 2 xxs
			  evens = takeEach 2 (drop 1 xxs)-}
mergeRunsBy cmp xxs = mergeRunsBy2 cmp odds evens
		    where odds =  mergeHalf xxs
			  evens = mergeHalf (drop 1 xxs)
			  mergeHalf = (mergeRunsBy cmp) . (takeEach 2)
mergeRunsBy2 cmp [] xs = xs
mergeRunsBy2 cmp xs [] = xs
mergeRunsBy2 cmp (x:xs) (y:ys)
	| cmp y x = y : (mergeRunsBy2 cmp (x:xs) ys)
	| otherwise = x : (mergeRunsBy2 cmp xs (y:ys))
----------------------------------------------------------------------
-- TLProcesses:
---------------

eagerInstList :: (Trans a, Trans b) =>
		 [Process a b ] -> [a] -> [b]
eagerInstList ps xs = let insts = zipWith createProcess ps xs
		      in tlList insts

---
-- tlList: force the instantiation of the element processes
--	     and return the list of results
--tlList :: {-Eval a =>-} [Lift a] -> [a]
-- tlList insts = foldr1 (seq) insts
tlList insts = forceWHNFSpine insts
	       `seq` (map deLift insts)

-- forceWHNFSpine: evaluate all elements of the list to WHNF
forceWHNFSpine :: {-Eval a => -}[a] -> ()
forceWHNFSpine [] = ()
forceWHNFSpine (x:xs) = x `seq` forceWHNFSpine xs

-- tlLList: the same for an list of lists
-- tlLList :: {-Eval a => -}[[Lift a]] -> [[a]]
-- tlLList insts = foldr1 (seq) (foldr1 (seq) insts)
tlLList insts = forceWHNFSpine (map forceWHNFSpine insts)
	       `seq` (map (map deLift) insts)

--------------------------------------------------------------

-- JB: unknown type in Haskell 98...
void = ()

-----------------------------------------------------------------------------
-- HWL: moved in here from FoldDM etc

spine [] = ()
spine (x:xs) = spine xs

whnfspine [] = ()
whnfspine (x:xs) = x `seq` whnfspine xs
