{-# OPTIONS_GHC -cpp -XParallelListComp -XScopedTypeVariables #-}
-- Time-stamp: <Sat Jun 26 2010 03:18:15 Stardate: Stardate: [-28]3280.27 hwloidl>
--
-- cash: the computer algebra shell
-- This is the start of a ghci-based shell, using SCSCP calls for computer algebra
-- Compiling sequentially picks the (older) SCSCP_API.hs module.
-- Compiling in parallel picks the multi-threaded client ParSCSCP.hs module.
-----------------------------------------------------------------------------

-- are we talking directly to a GAP server or a Haskell-side Coordination server

module Cash where

import Date

#undef GAP_SERVER

#undef  RUN_BUILTINS
#undef  RUN_POLYS 
#undef  RUN_SUMEULER 
#undef  RUN_SUMEULER_PAR 

import Network
import System
import System.Exit
import System.IO
import System.IO.Unsafe
-- import Time
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as C

import SCSCP_API
import HS_SCSCP
import HS2SCSCP

-- examples how to use it
-- import SCSCP_Ex -- cut down version of SCSCP_Examples

#ifdef HAVE_SKELETONS
-- abstractions over patterns of parallel coordination
import CA_Skeletons
#endif

-- These are services, known to the client from the start
import BaseServices

#ifdef __PARALLEL_HASKELL__ 
-- import EdenHelpers -- helper functions
-- import FoldDM
import Eden
#endif

-- import Poly
-- import TestPolys
-- import Karatsuba 

-- boilerplate setup, including pre-shared info
#if 0
main :: IO()
main = do
        args <- getArgs
        let portNum = if null args 
                         then 12321
                         else fromInteger (read (head args))
        doClient portNum
#endif

ngoq = initServer (server "localhost" (Just 12321))

-------------------------------------------------------
-- Util fcts (from ParSCSCP.hs)

call0 :: CAName -> OMObj 
call0 name = unsafePerformIO (putStrLn $ "call0 of "++(show name)) `seq`   
             (callSCSCP name [])

call1 :: (OMData a, OMData b) =>
         CAName -> a -> b
call1 name x  = fromOM (callSCSCP name [toOM x])

call2 :: (OMData a, OMData b, OMData c) =>
         CAName -> a -> b -> c
call2 name x y = fromOM (callSCSCP name [toOM x,toOM y])

mkRand n = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n] )

mult p1 p2 = callSCSCP scscp_CS_KaratsubaStr_x [p1, p2]

resultant p1OM_str p2OM_str = callSCSCP scscp_CS_Resultant ( map toOM [p1OM_str, p2OM_str] )

-------------------------------------------------------
-- purely Haskell side code

-- 
fact :: Integer -> Integer
fact 0 = 1
fact n = n*(fact (n-1))

factAcc :: Integer -> Integer -> Integer
factAcc 0 acc = acc
factAcc n acc = factAcc (n-1) (n*acc)

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

-----------------------------------------------------------------------------

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

-- client for calling Euler totient function 
-- currently only works with the purely Haskell dummyServer
-- GAP server works fine up to Phi function, but needs name scscp_WS_Phi 
-- polynomials only supported for the Haskell server
doClient :: PortNumber -> IO ()
doClient portNum = do
                    --  init ...
	            putStrLn ("starting up client, opening port " ++ show portNum)
                    initServer (server "localhost" (Just portNum))
#ifdef RUN_BUILTINS
                    -------------------------------------------------------
                    -- ask for available services
	            putStrLn $ "Request for GetServiceDescr ..."
	            let fName = Right GetServiceDescr
                    let args = []
                    let resOM = callSCSCP fName args
                    putStrLn $ "Reply: "++(show resOM)
                    -------------------------------------------------------
# ifdef GAP_SERVER
                    -- ask for available services
	            putStrLn $ "Request for GetTransientCD ..."
	            let fName = Right GetTransientCD
                    let args = map toOM ["scscp_transient_1"]
                    let resOM = callSCSCP fName args
                    putStrLn $ "Reply: "++(show resOM)
# endif
                    -------------------------------------------------------
                    -- ask for available services
	            putStrLn $ "Request for GetAllowedHeads ..."
	            let fName = Right GetAllowedHeads
                    let args = []
                    let resOM = callSCSCP fName args
                    putStrLn $ "Reply: "++(show resOM)
                    -------------------------------------------------------
# ifdef GAP_SERVER
                    -- ask for available services
	            putStrLn $ "Request for GetSignature ..."
	            let fName = Right GetSignature 
                    let args = map toOM ["scscp_transient_1", "WS_Phi"] -- HWL: BROKEN: wrong encoding of args; should be in OMS
                    let resOM = callSCSCP fName args
                    putStrLn $ "Reply: "++(show resOM)
# endif
#endif
                    -------------------------------------------------------
                    -- do the computation ...
	            putStrLn $ "Running phi 12 ..."
	            let fName = 
#  ifdef GAP_SERVER
                                scscp_WS_Phi 
#  else
                                scscp_CS_Phi
#  endif
                    let args = map toOM [12::Int]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)
#if 0
	            putStrLn $ "Running factorial 5 ..."
	            let fName = 
#  ifdef GAP_SERVER
                                scscp_WS_Factorial
#  else
                                scscpFact
#  endif
                    let args = map toOM [5::Int]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)
#endif
                    -- do the computation ...
#ifdef RUN_SUMEULER
                    let n = 87
	            putStrLn $ "Running sumEulerWithSCSCP "++(show n)++" ..."
                    x <- sumEulerWithSCSCP n
	            putStrLn $ "Result: "++(show x)
	            putStrLn $ "Running sumEuler_seq "++(show n)++" ..."
	            let y = sumEuler_seq n
	            putStrLn $ "Result: "++(show y)
                    putStrLn $ "Are the two results the same: "++(show (x==y))
#endif
                    -----------------------------------------------------------------------------         
                    -- do the computation ...
                    -- shutdown
                    releaseServer

