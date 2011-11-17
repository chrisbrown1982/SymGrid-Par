\jbcomment{draft mode off \horiz}
\begin{code}
{-
parmap and mapReduce skeletons, directly inlined from JB PhD
(edi-skel1.lhs)

Use cpp to create a compiling version:

$> cpp ParMap.pp.lhs ParMap.lhs && eden-6.8 -parpvm -c --make ParMap.lhs
-}
module ParMap where

import System.IO.Unsafe
import Control.Monad
import Control.Concurrent
import Data.List

import ParPrim
import Eden
import Edi hiding (noPe,selfPe)
import qualified Edi


\end{code}
Now inline all code from the chapter via cpp:

#include "edi-skel1.tex"

The rest: helpers and additional definitions
\begin{code}
-- Eden parmap, other version:
parMapEden2 :: (Trans a, Trans b) => (a -> b) -> [a] -> [b]
parMapEden2 f xs = unsafePerformIO ( mapM (instantiate (process f)) xs )

-- Eden parmap, placement:
parMapAt :: (Trans a, Trans b) => [Int] -> (a -> b) -> [a] -> [b]
parMapAt places f xs 
   = unsafePerformIO ( 
       zipWithM (\pe x -> instantiateAt pe (process f) x) 
                places xs 
      )


-- version with process parameter instead of function:
parMap' :: (Trans a, Trans b) => (Process a b) -> [a] -> [b]
parMap' proc xs = unsafePerformIO (mapM (instantiate proc) xs)

-- Eden process farm (np processes, each working on a whole sublist)
farm :: (Trans a, Trans b) => 
               Int -> (Int -> [a] -> [[a]]) -- n, distribute
    -> ([[b]] -> [b])        -- combine
    -> Process [a] [b]       -- worker process
    -> [a] -> [b]            -- what to do
farm np distr combine p inputs = 
       combine (parMap' p (distr np inputs))

parmapfarm :: (Trans a, Trans b) => Int -> (a -> b) -> [a] -> [b]
-- in text:
-- parmapfarm np f xs = unSplit ({\it parMap*} (map f) (split np xs))
parmapfarm np f xs = unSplit (parMapEdi (map f) (split np xs))
split = unshuffle
unSplit = shuffle

#ifdef OWNHELPERS
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
#endif

--------------------------------------------------------------
-- monadic Edi versions using primitive operations only:
-- Attention: Both versions do not communicate in streams, but single data!!
parMapIO, parMapIO2 :: (NFData a,NFData b) => (a -> b) -> [a] -> IO [b]
-- version which communicates evaluated input in extra thread
parMapIO _   []   = return []
parMapIO f (x:xs) = do (chR,r)   <- createC
                      (chcI,cI) <- createC
                      spawnProcessAt 0 (proc chR chcI)
                      fork (sendNF cI x)
                      rest <- parMapIO f xs
                      return (r:rest)
   where proc chR chcI = do (cI, input) <- createC
                            sendNF chcI cI
                            sendNF chR (f input)

-- version which communicates unevaluated input inside process abstraction
parMapIO2 _   []   = return []
parMapIO2 f (x:xs) = do (chan,result) <- createC
                       spawnProcessAt 0 (sendNF chan (f x)) 
                       rs <- parMapIO2 f xs
                       return (result:rs)

-- version communicating in streams
parMapIOStream :: (NFData a, NFData b) => (a -> [b]) -> [a] -> IO [[b]]
parMapIOStream _   []   = return []
parMapIOStream f (x:xs) = do (chR,r)   <- createC
                            (chcI,cI) <- createC
                            spawnProcessAt 0 (proc chR chcI)
                            fork (sendNF cI x)
                            rest <- parMapIO f xs
                            return (r:rest)
   where proc chR chcI = do (cI, input) <- createC
                            sendNF chcI cI
                            sendNFStream chR (f input)
           ------
\end{code}