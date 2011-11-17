{-# OPTIONS_GHC -cpp -XParallelListComp -fglasgow-exts #-}
-- Time-stamp: <Tue Jun 29 2010 00:05:01 Stardate: Stardate: [-28]3294.60 hwloidl>
--
-- SCSCP client with several small (seq and par) applications, choosable on the commandline.
-- E.g.  ./testClient 12321 sumEulerPar 8000 4000
-- Minimal test:
-- ./testClient 12321 fib 11
-- Parallelism tested with
-- ever popular sumEuler
-- ./testClient 12321 sumEuler2 13000 11000
-- ./testClient 12321 sumEulerClassic 13000 11000
-- this performs GAP calls on ranges; good for demo
-- ./testClient 12321 sumEulerPar 40 10
-- ./testClient 12321 sumEulerPar 8000 2000
-- polynomial computations:
-- ./testClient 12321 kara 10
-- ./testClient 12321 karaPar 10
-- ./testClient 12321 resultant 300 
-- ./testClient 12321 GB 3
-- skeletons:
-- ./testClient 12321 parMap WS_Phi 87 88 89
-- ./testClient 12321 parMapFold WS_Phi WS_Plus 0 87 88 89
-- ./testClient 12321 parMapFold1 WS_Phi WS_Plus 87 88 89
-- ./testClient 12321 parZipWith WS_Plus 1 2 3 99 98 97
-- ./testClient 12321 parMapFold WS_Res WS_Plus 0 92 93 94
-----------------------------------------------------------------------------

-- are we talking directly to a GAP server or a Haskell-side Coordination server
#undef GAP_SERVER
#define HAVE_GLASGOW_EXTS 1

import Maybe
import Network
import System
import System.Exit
import System.IO
import System.IO.Unsafe
-- import Time
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as C

#ifdef __PARALLEL_HASKELL__ 
import ParSCSCP
#else 
import SCSCP_API
#endif
import HS_SCSCP
import HS2SCSCP

#if 0 && defined(HAVE_GLASGOW_EXTS)
-- see: http://hackage.haskell.org/packages/archive/tuple/0.2.0.1/doc/html/src/Data-Tuple-Curry.html
import Data.Tuple.Curry
#endif

-- import HACK_PreShared
import BaseServices

-- import the type for arithmetic
import SGPTypes

#ifdef __PARALLEL_HASKELL__ 
-- import EdenHelpers -- helper functions
-- import FoldDM
import Eden
#endif

#ifdef __PARALLEL_HASKELL__ 
ardbeg_servers = [ SCSCPServer "localhost" (PortNumber (26132+i)) Nothing
                 | i <- [1..4] ]
#endif

#ifdef HAVE_GLASGOW_EXTS
tests :: [ (String, Maybe PortNumber, [Int],          Int,   Int -> Int -> IO ()) ]
--            name, req port,     default args, arity, how to run it
tests = [ ("builtins",    Nothing,                 [],   	0, \ _ _ -> runBuiltins)
        , ("phi",         Nothing,                 [89], 	1, \ x _ -> runPhi x)
        , ("fib",         Nothing,                 [12], 	1, \ x _ -> runFib x)
        , ("fact",        Nothing,                 [5],  	1, \ x _ -> runFact x)
        , ("polys",       Just  26133, [20], 	1, \ x _ -> runPolys x) 
	, ("SumEuler",    Just  12321, [8000,100], 2,  runSumEulerPar) -- demo version
        , ("sumEuler",    Nothing,                 [87], 	1, \ x _ -> runSumEuler x)
	, ("sumEuler2",   Just  12321, [9900,8800], 2,  runSumEuler2)
	, ("sumEulerPar",    Just  12321, [8000,100], 2,  runSumEulerPar)
	, ("sumEulerShuffle", Just  12321, [8000,100],   2,  runSumEulerShuffle)
        , ("sumEulerClassic", Just 12321, [2200, 1100], 2, runSumEulerClassic)
        , ("run2Id",      Just  12321, [5, 87], 2, \ fId n -> run2Id fId n)
        , ("run2",        Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("run1",        Just  12321, []{-unused-}, 1, \ _ _ -> error "Unused")
        , ("t",           Just  26135, [200, 300], 2, runT)
        , ("parMap",      Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("parMap_",     Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("parMapFold",  Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("parMapFold1", Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("parZipWith",  Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("map",      Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("map_",     Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("mapFold",  Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("mapFold1", Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
        , ("zipWith",  Just  12321, []{-unused-}, 2, \ _ _ -> error "Unused")
--        , ("run2Wrapper", Just  12321, [87, 89], 2, \ m n -> run2Wrapper m n)
        , ("kara",        Just  12321, [20], 1, \ x _ -> runKaratsuba x)
	, ("karaPar",     Just  12321, [20], 1, \ x _ -> runKaratsubaPar x)
	, ("karaPar0",     Just  12321, [20], 1, \ x _ -> runKaratsubaPar0 x)
        , ("resultant",   Just  12321, [300], 1, \ x _ -> runResultant x)
        , ("resultant2",  Just  12321, [300], 1, \ x _ -> runResultant2 x)
        , ("GB",   Just  12321, [5], 1, \ x _ -> runGB x)
        , ("parOrbit", Just 12321, [], 0, error "unused")
        , ("parOrbitFin", Just 12321, [], 0, error "unused")
        ]

{- 
tests :: [ forall a, (String, Int, a -> IO ()) ]
--                      name,  arity, how to run it
-- could use curryN from Data.Tuple.Curry instead
tests = [ ("builtins", 1, uncurryN runBuiltins)
        , ("polysGB", 1,  uncurryN runPolysGB)
        , ("testPolysGB", 1, uncurryN mainTestPolys)
        , ("polys", 1, \ x _ -> uncurryN runPolys) 
        , ("sumEuler", 1, \ x _ -> uncurryN runSumEuler)
	, ("sumEuler2", 2,  uncurryN runSumEuler2)
	, ("sumEulerPar", 2,  uncurryN runSumEulerPar)
        , ("sumEulerClassic", 2, uncurryN runSumEulerClassic)
        -- , ("run2", 3, \ f m n -> run2 (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", "WS_Plus")) [1,m] [1,n])
        , ("kara", 1, \ x _ -> uncurryN runKaratsuba)
	, ("karaPar", 1, \ x _ -> uncurryN runKaratsubaPar)
        ]
-}

-----------------------------------------------------------------------------
-- generic aux fcts

fst5 (a,_,_,_,_) = a

lookupTest name = let res = filter ((==name).fst5) tests
                  in if null res
                       then Nothing
                       else Just (head res)

callN n f xs = if n/=(length xs)
                 then error $ "This test requires "++(show n)++" arguments, but is given "++(show (length xs))++" namely "++(show xs)
                 else case length xs of
		       0 -> f 0{-unused-} 0{-unused-}
		       1 -> f (xs!!0) 0{-unused-}
                       2 -> f (xs!!0) (xs!!1)
                       _ -> error $ "callN: only up to 2 arguments supported"

{- version with curryN
callN n f xs = if n/=(length xs)
                 then error $ "This test requires "++(show n)++" arguments"
                 else case n of
		       1 -> curryN f (xs!!0)
                       2 -> curryN f (xs!!0) (xs!!1)
                       3 -> curryN f (xs!!0) (xs!!1) (xs!!2)
                       _ -> error $ "CallN: only up to 3 arguments supported"
-}

#endif
                            
-----------------------------------------------------------------------------
-- boilerplate setup, including pre-shared info
#ifndef CASH
main :: IO()
main = do
        args <- getArgs
        when (null args) $ do
         putStrLn ("Usage: testClient <port> <what>...")
         putStrLn ("where <what> is one of: "++(show (map fst5 tests)))
         exitFailure
        let (portNum, args') = (fromInteger (read (head args)), tail args)
        set_portnum portNum
        doClient portNum args'
#endif

-----------------------------------------------------------------------------

-- client for calling a given (by name) test fct on the given port
-- extracts the necessary info from the global variable tests

#ifdef HAVE_GLASGOW_EXTS
doClient :: PortNumber -> [String] -> IO ()
doClient portNum args = do
                    --  init ...
	            let test = head args
		    let th = lookupTest test
	            when (isNothing th) $
                        error $ "Unknown test "++test
                    let (Just (name, port, defaults, arity, fct)) = th
		    when (isJust port && (fromJust port) /= (get_portnum True)) $
                      error $ test++" not supported on port "++(show (get_portnum True))++"; it needs port "++(show (fromJust port))
	            case name of {
                     "run2" ->   -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args1) ++ " and " ++ (show args2))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args)
                            args' :: [Integer]
                            args' = map read (drop 2 args)
	                    [args1,args2] = mkArgs f args'
                        run2 (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", f)) args1 args2 
                        releaseServer
                    ; "run1" ->   -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args1) ++ " and " ++ (show args2))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args)
                            args1 :: [Integer]
                            args1 = map read (drop 2 args)
                        run1 (Left ("scscp_transient_1", f)) (map toOM args1)
                        releaseServer
	            ;  "parOrbit" ->  -- special case: expect the name of the fct as next arg, then values
                      do
                        putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let setSize :: Int
                            setSize = read (args!!1)
                            -- args' :: [Int]
                            -- args' = map read (drop 2 args)

                        runOrbit [Left ("scscp_transient_1", "WS_Fib1"), Left ("scscp_transient_1", "WS_Fib2"), Left ("scscp_transient_1", "WS_Fib3")] [[1]] setSize
                        -- runParMap (Left ("scscp_transient_1", f)) args'
                        releaseServer
                    ;  "parOrbitFin" ->  -- special case: expect the name of the fct as next arg, then values
                      do
                        putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        -- let setSize :: Int
                        --  setSize = read (args!!1)
                            -- args' :: [Int]
                            -- args' = map read (drop 2 args)
  
                        runOrbitFin [Left ("scscp_transient_1","WS_Fin1"),Left ("scscp_transient_1","WS_Fin2")] [MatrixRow [Mul (PrimEl 3) 0,Power (PrimEl 3) 0,Power (PrimEl 3) 1]] 0

                        -- runOrbit [Left ("scscp_transient_1", "WS_Fib1"), Left ("scscp_transient_1", "WS_Fib2"), Left ("scscp_transient_1", "WS_Fib3")] [[1]] setSize
                        -- runParMap (Left ("scscp_transient_1", f)) args'
                        releaseServer
                    ; "parMap" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif

                        let f = head (drop 1 args)
                            args' :: [Int]
                            args' = map read (drop 2 args)
                        runParMap (Left ("scscp_transient_1", f)) args'
                        releaseServer
	            ; "parMap_" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args)
                            args' :: [Int]
                            args' = map read (drop 2 args)
                        runParMap' (Left ("scscp_transient_1", f)) args'
                        releaseServer
	            ; "parMapFold" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            g = head (drop 2 args) -- fct to fold
                            arg :: Int
                            arg = read (head (drop 3 args)) -- neutral element
                            args' :: [Int]
                            args' = map read (drop 4 args) -- argument list
                        runParMapFold (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", g)) arg args'
                        releaseServer
	            ; "parMapFold1" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            g = head (drop 2 args) -- fct to fold
                            args' :: [Int]
                            args' = map read (drop 3 args) -- argument list
                        runParMapFold1 (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", g)) args'
                        releaseServer
	            ; "parZipWith" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            args1, args2 :: [Int]
                            (args1,args2) = splitAt (((length args)-2) `div` 2) (map read (drop 2 args)) -- argument lists
                        runParZipWith (Left ("scscp_transient_1", f)) args1 args2
                        releaseServer
                    -- sad, sequential versions of the skeletons
	            ; "map" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args)
                            args' :: [Int]
                            args' = map read (drop 2 args)
                        runMap (Left ("scscp_transient_1", f)) args'
                        releaseServer
	            ; "map_" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args)
                            args' :: [Int]
                            args' = map read (drop 2 args)
                        runMap' (Left ("scscp_transient_1", f)) args'
                        releaseServer
	            ; "mapFold" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            g = head (drop 2 args) -- fct to fold
                            arg :: Int
                            arg = read (head (drop 3 args)) -- neutral element
                            args' :: [Int]
                            args' = map read (drop 4 args) -- argument list
                        runMapFold (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", g)) arg args'
                        releaseServer
	            ; "mapFold1" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            g = head (drop 2 args) -- fct to fold
                            args' :: [Int]
                            args' = map read (drop 3 args) -- argument list
                        runMapFold1 (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", g)) args'
                        releaseServer
	            ; "zipWith" ->  -- special case: expect the name of the fct as next arg, then values
	              do
	                putStrLn ("starting up client, opening port " ++ show portNum) --  ++ "; launching service " ++ f ++ " on argument lists " ++ (show args'))
#ifdef __PARALLEL_HASKELL__
                        initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                        initServer (server "localhost" (Just portNum))
#endif
                        let f = head (drop 1 args) -- fct to map
                            args1, args2 :: [Int]
                            (args1,args2) = splitAt (((length args)-2) `div` 2) (map read (drop 2 args)) -- argument lists
                        runZipWith (Left ("scscp_transient_1", f)) args1 args2
                        releaseServer
                    ; _ -> 
	            	-- std case
                      do
                    	let args' :: [Int]
                    	    args' = map read (drop 1 args)
                    	let args'' = if arity==length args'
                    	               then args'
                    	               else defaults
	            	putStrLn ("starting up client, opening port " ++ show portNum)
#ifdef __PARALLEL_HASKELL__
                    	-- NO: this would connect directly to ardbeg servers
                    	-- initServers ardbeg_servers 
                    	initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                    	initServer (server "localhost" (Just portNum))
#endif
		    	putStrLn $ "Calling "++test++" with arguments "++(show args'')
                    	callN arity fct args''
                    	-- shutdown
                    	releaseServer
                    }
#else
-- ngoq ngo'
doClient :: PortID -> [String] -> IO ()
doClient portNum what = do
                    --  init ...
	            putStrLn ("starting up client, opening port " ++ show portNum)
#ifdef __PARALLEL_HASKELL__
                    -- NO: this would connect directly to ardbeg servers
                    -- initServers ardbeg_servers 
                    initServer False{-async?-} (server "localhost" (12321)) -- (server "localhost" portNum)
#else
                    initServer (server "localhost" (Just portNum))
#endif
	            let args = tail what
		    when ("builtins" `elem` what) $
	             runBuiltins 0
		    when ("fib" `elem` what) $ do
                     let n = if (null args)
                                  then 12
                                  else (read (args!!0))::Int
                     runFib n
		    when ("polysGB" `elem` what) $
                     runPolysGB 0
		    when ("testPolysGB" `elem` what) $ do
                     let n = if (null args)
                                  then 5
                                  else (read (args!!0))::Int
                     mainTestPolys n
		    when ("polys" `elem` what) $ do
                     let n = if (null args)
                                  then 20::Int
                                  else (read (args!!0))::Int
                     runPolys n
		    when ("sumEuler" `elem` what) $ do
                     let [m,n] = if (null args)
                                  then [8000,100]
                                  else (map (read args))::[Int]
                     runSumEulerPar m n
		    when ("sumEuler" `elem` what) $ do
                     let [m,n] = if (null args)
                                  then [8000,100]
                                  else (map (read args))::[Int]
                     runSumEulerPar m n
		    when ("sumEuler2" `elem` what) $ do
                     let (m,n) = if (length args < 2)
                                  then (9900, 8800)
                                  else ((read (args!!0))::Int, (read (args!!1))::Int)
                     runSumEuler2 m n
		    when ("sumEulerPar" `elem` what) $ do
                     let (n,c) = if (null args)
                                  then (8000, 100)
                                  else ((read (args!!0))::Int, (read (args!!1))::Int)
                     runSumEulerPar n c
		    when ("sumEulerShuffle" `elem` what) $ do
                     let (n,c) = if (null args)
                                  then (8000, 100)
                                  else ((read (args!!0))::Int, (read (args!!1))::Int)
                     runSumEulerShuffle n c
		    when ("sumEulerClassic" `elem` what) $ do
                     let (from, to) = if (null args)
                                  	then (2200::Int, 1100::Int)
                                  	else ((read (args!!0))::Int, (read (args!!1))::Int)
                     runSumEulerClassic from to
		    when ("run2" `elem` what) $ do
                     let (f,m,n) = if (length args < 3)
                                  then ("WS_sumEulerClassic", 9900, 8800)
                                  else ((args!!0)::String, (read (args!!1))::Int, (read (args!!2))::Int)
                     run2 (Left ("scscp_transient_1", f)) (Left ("scscp_transient_1", "WS_Plus")) [1,m] [1,n]
                     -- run2 scscp_WS_sumEulerClassic scscp_HS_Plus [m] [n]
		    when ("kara" `elem` what) $ do
                     let n = if (null args)
                                  then 20::Int
                                  else (read (args!!0))::Int
                     runKaratsuba n
		    when ("karaPar" `elem` what) $ do
                     let n = if (null args)
                                  then 20::Int
                                  else (read (args!!0))::Int
                     runKaratsubaPar n
		    when ("karaPar0" `elem` what) $ do
                     let n = if (null args)
                                  then 20::Int
                                  else (read (args!!0))::Int
                     runKaratsubaPar0 n
                    -- shutdown
                    releaseServer
#endif

-------------------------------------------------------
-- this code doesn't exist
portnum_GLOBAL :: MVar PortNumber
portnum_GLOBAL = unsafePerformIO (newMVar 0)
-- ^^^^ ============================================= GLOBAL CONSTANT

set_portnum :: PortNumber -> IO ()
set_portnum port =  modifyMVar_ portnum_GLOBAL (\ i -> return (port))

get_portnum :: Bool -> PortNumber
get_portnum x = if x 
                      then unsafePerformIO (readMVar portnum_GLOBAL)
	              else 0

-----------------------------------------------------------------------------

runBuiltins = do
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

runPhi :: Int -> IO ()
runPhi n = do
                    -------------------------------------------------------
                    -- do the computation ...
                    -- let n = 12
	            putStrLn $ "Running phi "++(show n)++" (Haskell-side)..."
	            let fName = if get_portnum True == 12321
                                  then scscp_CS_Phi
                                  else if get_portnum True == 12322
                                         then scscp_HS_Phi 
                                         else scscp_WS_Phi 
                    let args = map toOM [n]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

runFib :: Int -> IO ()
runFib n = do
                    -------------------------------------------------------
                    -- do the computation ...
                    -- let n = 12
	            putStrLn $ "Running fib "++(show n)++" (GAP-side)..."
	            let fName = if get_portnum True == 12321
                                  then scscp_CS_Fib
                                  else if get_portnum True == 12322
                                         then scscp_HS_Fib
                                         else scscp_WS_Fibonacci
                    let args = map toOM [n]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

runFact :: Int -> IO ()
runFact n = do
                    -------------------------------------------------------
                    -- do the computation ...
                    -- let n = 12
	            putStrLn $ "Running factorial "++(show n)++" ..."
	            let fName = if (get_portnum True) == 12321
                                  then scscp_CS_Factorial
                                  else if get_portnum True == 12322
                                         then scscp_HS_Factorial
                                         else scscp_WS_Factorial
                    let args = map toOM [n::Int]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

-- SCSCP calls for each application
runSumEuler :: Int -> IO ()
runSumEuler n = do
	            putStrLn $ "Running sumEulerWithSCSCP "++(show n)++" ..."
                    x <- sumEulerWithSCSCP n
	            putStrLn $ "Result: "++(show x)
	            putStrLn $ "Running sumEuler_seq "++(show n)++" ..."
	            let y = sumEuler_seq n
	            putStrLn $ "Result: "++(show y)
                    putStrLn $ "Are the two results the same: "++(show (x==y))


runSumEuler2 :: Int -> Int -> IO ()
runSumEuler2 m n = do 
                    putStrLn $ "Launching sumEuler2 on "++(show m)++" "++(show n)++", i.e. exactly 2 processes, coordinated by the server ..."
                    let fName = scscp_CS_SumEuler2
                    -- let args = map toOM [80::Int, 20::Int]
                    -- let args = map toOM [800::Int, 400::Int]
                    let args = map toOM [m, n]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

decodeWS :: Int -> Int -> (Int, CAName, [Int], [Int])
decodeWS 1 n =  (2, scscp_WS_Plus, [n,n+1], [n+2,n+3])
decodeWS 2 n =  (1, scscp_WS_Factorial, [n], [n+1])
decodeWS 3 n =  (1, scscp_WS_Phi, [n], [n+1])
decodeWS 4 n =  (1, scscp_WS_Fibonacci, [n], [n+1])
decodeWS 5 n =  (1, scscp_WS_sumEulerClassic, [1,n], [1,n+1])
decodeWS 11 n = (0, scscp_WS_RandomPolynomial, [n], [n+1])
decodeWS 12 n = (0, scscp_WS_RandomPolynomialAsString, [], [])
{- FIXME: literal polynomial-as-string inputs
decodeWS 13 n = (1, scscp_WS_Polynomial2String, )
decodeWS 14 n = (2, scscp_WS_KaratsubaStr)
decodeWS 15 n = (2, scscp_WS_KaratsubaStr_x)
decodeWS 16 n = (2, scscp_WS_Karatsuba)
-}
decodeWS fId _ = error $ "No service with encoding "++(show fId)++"; omit command line arguments to pick defaults"

-- make 2 argument lists, to be passed to 2 instances of the service (1st arg)
mkArgs "WS_sumEulerClassic" [m,n] = [map toOM [1,m], map toOM [1,n]]
mkArgs "WS_Res" [m,n] = [map toOM [m], map toOM [n]]
mkArgs "WS_resultant" [m,n] = unsafePerformIO $
                                do
                                 putStrLn $ "mkArgs: p1: building random poly of degree "++(show m)
                                 let p1OM = callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [m] )
          	                 putStrLn $ "polynomial: p1: "++(show  p1OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show m)
                                 let p2OM = callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [m] )
          	                 putStrLn $ "polynomial: p2: "++(fromOM p2OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show n)
                                 let p3OM = callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
          	                 putStrLn $ "polynomial: p3: "++(fromOM p3OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show n)
                                 let p4OM = callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
          	                 putStrLn $ "polynomial: p4: "++(fromOM p4OM)
                                 return [[p1OM, p3OM], [p2OM,p4OM]]
mkArgs _ [m,n] = [[toOM m], [toOM n]]

-- test generation of random polynomials
runT :: Int -> Int -> IO ()
runT m n = -- mkArgs "WS_resultant" [m,n] = -- unsafePerformIO $
                                do
                                 putStrLn $ "mkArgs: p1: building random poly of degree "++(show m)
                                 let p1OM = callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [m] )
          	                 putStrLn $ "polynomial: p1: "++(show  p1OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show m)
                                 let p2OM = callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [m] )
          	                 putStrLn $ "polynomial: p2: "++(fromOM p2OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show n)
                                 let p3OM = callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [n] )
          	                 putStrLn $ "polynomial: p3: "++(fromOM p3OM)
                                 putStrLn $ "mkArgs: building random poly of degree "++(show n)
                                 let p4OM = callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [n] )
          	                 putStrLn $ "polynomial: p4: "++(fromOM p4OM)
                                 -- return [[p1OM, p2OM], [p3OM,p4OM]]
                                 return ()


-- OK: ./testClient 12321 run2Id 5 87
-- the Int encoding of the function is a restriction of the testClient only
-- but not a restriction of the CoordinationServer
run2Id :: Int -> Int -> IO ()
run2Id fId n = do
             let (arity, fWS@(Left (cd_f,f)), xs, ys) = decodeWS fId n
             putStrLn $ "Launching run2Id with service "++(show f)++" on inputs "++(show xs)++" and "++(show ys)++", i.e. exactly 2 processes, coordinated by the server ..."
             let fName = scscp_CS_ParProcesses2
             let args = [toOM f, toOM f, toOM (map toOM xs), toOM (map toOM ys)]
             let resOM = callSCSCP fName args
             let x = (fromOM resOM) :: Int
	     putStrLn $ "Result: "++(show x)
-- run2Id _ _ = error "run2Id: expecting proper CANames as first two arguments"

-- arbitrary service
-- run2 :: CAName -> CAName -> [Int] -> [Int] -> IO ()
run2 :: CAName -> CAName -> [OMObj] -> [OMObj] -> IO ()
run2 (Left (cd_f,f)) (Left (cd_g,g)) xs ys = do 
                    putStrLn $ "Launching run2 with service "++(show f)++" on "++(show xs)++" and service "++(show g)++" on "++(show ys)++", i.e. exactly 2 processes, coordinated by the server ..."
                    let fName = scscp_CS_ParProcesses2
                    let args = [toOM f, toOM g, toOM xs, toOM ys]  -- toOM (map toOM xs), toOM (map toOM ys)]
                    let resOM = callSCSCP fName args
                    let x = (fromOM resOM) :: Integer
	            putStrLn $ "Result: "++(show x)
run2 _ _ _ _ = error "run2: expecting proper CANames as first two arguments"

run1 :: CAName -> [OMObj] -> IO ()
run1 (Left (cd_f,f)) xs' = do 
                    putStrLn $ "Launching run1 with service "++(show f)++" on "++(show xs')++", i.e. passing 1 service through coordination server down to CAS ..."
                    let fName = scscp_CS_ParProcesses1
                    let args = (toOM f):xs'  -- toOM (map toOM xs), toOM (map toOM ys)]
                    let resOM = callSCSCP fName args
                    let x = (fromOM resOM) :: Integer
	            putStrLn $ "Result: "++(show x)
run1 _ _ = error "run1: expecting proper CANames as first two arguments"


{-
run2Wrapper :: Int -> Int -> IO ()
run2Wrapper m n = do
	            let f = scscp_WS_sumEulerClassic
	            let xs = [1,m]
	            let ys = [1,n]
                    run2 f f xs ys
-}

-- sad, sequential versions of the skeletons
-- TODO: don't shun higher-order functions
runMap :: CAName -> [Int] -> IO ()
runMap (Left (cd_f,f)) xs = do 
                    putStrLn $ "Launching runMap with service "++(show f)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_Map
                    let args = [toOM f, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let xs = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show xs)

runMap' :: CAName -> [Int] -> IO ()
runMap' (Left (cd_f,f)) xs = do 
                    putStrLn $ "Launching runMap' with FIXED service resultantOM ..." -- "++(show f)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_Map'
                    let args = [toOM f, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let xs = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show xs)

-- parallel fold-of-map skeleton
runMapFold :: CAName -> CAName -> Int -> [Int] -> IO ()
runMapFold (Left (cd_f,f)) (Left (cd_g,g)) x xs = do 
                    putStrLn $ "Launching runParMapFold with map service "++(show f)++" and fold service "++(show g)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_MapFold
                    let args = [toOM f, toOM g, toOM x, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show z)

-- parallel fold-of-map skeleton (non-empty list)
runMapFold1 :: CAName -> CAName -> [Int] -> IO ()
runMapFold1 (Left (cd_f,f)) (Left (cd_g,g)) xs = do 
                    putStrLn $ "Launching runParMapFold1 with map service "++(show f)++" and fold service "++(show g)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_MapFold1
                    let args = [toOM f, toOM g, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show z)

-- parallel fold-of-map skeleton (non-empty list)
runZipWith :: CAName -> [Int] -> [Int] -> IO ()
runZipWith (Left (cd_f,f)) xs ys = do 
                    putStrLn $ "Launching runParZipWith with zip service "++(show f)++" on "++(show xs)++" and "++(show ys)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ZipWith
                    let args = [toOM f, toOM (map toOM xs), toOM (map toOM ys)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show z)

-- parallel map skeleton
runParMap :: CAName -> [Int] -> IO ()
runParMap (Left (cd_f,f)) xs = do 
                    putStrLn $ "Launching runParMap with service "++(show f)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ParMap
                    let args = [toOM f, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let xs = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show xs)

-- parallel orbit implementation
-- added by CMB 14/07/2010
runOrbitFin :: [CAName] -> [Arith] -> Int -> IO ()
runOrbitFin ys xs setSize = do
                putStrLn $ "Running Orbit on Finite Fields " ++ ( show xs ) ++ " (the workers call Gap-side functions)"
                let fname = scscp_CS_Orbit
                let args = [toOM (map toOM (getFunFromCA ys)), toOM (map toOM xs), toOM setSize]
                let resOM = callSCSCP fname args
                let x = (fromOM resOM) :: [Arith]
                putStrLn $ "Result: " ++ (show x)
                     where
                       getFunFromCA :: [CAName] -> [String]
                       getFunFromCA [] = []
                       getFunFromCA ( (Left (cd_f, f)):xs) = f : getFunFromCA xs

-- parallel orbit implementation
-- added by CMB 14/07/2010
runOrbit :: [CAName] -> [[Int]] -> Int -> IO ()
runOrbit ys xs setSize = do
                putStrLn $ "Running Orbit " ++ ( show xs ) ++ " (the workers call Gap-side functions)"
                let fname = scscp_CS_Orbit
                let args = [toOM (map toOM (getFunFromCA ys)), toOM (map toOM xs), toOM setSize]
                let resOM = callSCSCP fname args
                let x = (fromOM resOM) :: [[Int]]
                putStrLn $ "Result: " ++ (show x)
                     where
                       getFunFromCA :: [CAName] -> [String]
                       getFunFromCA [] = []
                       getFunFromCA ( (Left (cd_f, f)):xs) = f : getFunFromCA xs



runParMap' :: CAName -> [Int] -> IO ()
runParMap' (Left (cd_f,f)) xs = do 
                    putStrLn $ "Launching runParMap' with FIXED service resultantOM ..." -- "++(show f)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ParMap'
                    let args = [toOM f, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let xs = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show xs)

-- parallel fold-of-map skeleton
runParMapFold :: CAName -> CAName -> Int -> [Int] -> IO ()
runParMapFold (Left (cd_f,f)) (Left (cd_g,g)) x xs = do 
                    putStrLn $ "Launching runParMapFold with map service "++(show f)++" and fold service "++(show g)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ParMapFold
                    let args = [toOM f, toOM g, toOM x, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show z)

-- parallel fold-of-map skeleton (non-empty list)
runParMapFold1 :: CAName -> CAName -> [Int] -> IO ()
runParMapFold1 (Left (cd_f,f)) (Left (cd_g,g)) xs = do 
                    putStrLn $ "Launching runParMapFold1 with map service "++(show f)++" and fold service "++(show g)++" on "++(show xs)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ParMapFold1
                    let args = [toOM f, toOM g, toOM (map toOM xs)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show z)

-- parallel fold-of-map skeleton (non-empty list)
runParZipWith :: CAName -> [Int] -> [Int] -> IO ()
runParZipWith (Left (cd_f,f)) xs ys = do 
                    putStrLn $ "Launching runParZipWith with zip service "++(show f)++" on "++(show xs)++" and "++(show ys)++", i.e. running each application of the service in parallel, coordinated by the server ..."
                    let fName = scscp_CS_ParZipWith
                    let args = [toOM f, toOM (map toOM xs), toOM (map toOM ys)]
                    let resOM = callSCSCP fName args
                    let z = (fromOM resOM) :: [Int]
	            putStrLn $ "Result: "++(show z)


runSumEulerShuffle :: Int -> Int -> IO ()
runSumEulerShuffle n c = do 
                    putStrLn $ "Launching parallel sumEulerShuffleSCSCP "++(show n)++" "++(show c)++", coordinated by the server ..."
                    let fName = scscp_CS_SumEulerShuffle
                    -- let args = map toOM [80::Int, 20::Int]
                    -- let args = map toOM [800::Int, 400::Int]
                    let args = map toOM [n, c]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

runSumEulerPar :: Int -> Int -> IO ()
runSumEulerPar n c = do 
                    putStrLn $ "Launching parallel sumEulerParSCSCP "++(show n)++" "++(show c)++", coordinated by the server ..."
                    let fName = scscp_CS_SumEulerPar
                    -- let args = map toOM [80::Int, 20::Int]
                    -- let args = map toOM [800::Int, 400::Int]
                    let args = map toOM [n, c]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

-- OK: ./testClient 12321 sumEulerClassic 9000 8000
runSumEulerClassic :: Int -> Int -> IO ()
runSumEulerClassic m n = do
                    putStrLn $ "Launching parallel sumEulerClassic on  [1,"++(show m)++"] and [1,"++(show n)++"] in parallel ..."
                    let fName = scscp_CS_SumEulerClassic -- scscp_WS_sumEulerClassic
                    -- let args = map toOM [22000::Int, 21000::Int]
                    let args = map toOM [m, n]
                    let resOM = callSCSCP fName args
                    -- print the result
                    let x = (fromOM resOM) :: Int
	            putStrLn $ "Result: "++(show x)

-- OK; tested with ./testClient 26133 polys 13
runPolys :: Int -> IO ()
runPolys n = do
                    -- to be run on GAP server
                    putStrLn $ "Using GAP server for polynomials computations (only RandomPoly and Karatsuba for now..."
	  	    putStrLn $ "RandomPolynomial: generating random polynomials of degree "++(show n)++"..."
      	  	    let fName = scscp_WS_RandomPolynomial
          	    let args  = map toOM [n] -- FIXED: [20::Int]
          	    let p1OM  = callSCSCP fName args
          	    putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
                    let p1OM_str :: String
                        p1OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p1OM]
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++(p1OM_str)
          	    let args  = map toOM [n] -- FIXED: [20::Int]
          	    let p2OM  = callSCSCP fName args
          	    -- putStrLn $ "Random polynomial p2 (as (show p)): "++(show p2OM)
          	    putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
                    let p2OM_str :: String
                        p2OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p2OM]
          	    putStrLn $ "Random polynomial p2 (as converted to string): "++(p2OM_str)
          	    putStrLn $ "Generated 2 polynomials; now calling Karatsuba multiplication ..."
                    let fName = scscp_WS_KaratsubaStr_x
                    let args  = map toOM [p1OM_str, p2OM_str]
          	    let p12OM  = callSCSCP fName args
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    putStrLn $ "Result polynomial of p1*p2 generated; length as string: "++(show (length (show p12OM)))
                    let p12OM_str :: String
                        p12OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p12OM]
          	    putStrLn $ "Result polynomial of p1*p2 (as converted to string): "++(p12OM_str)
                    -----------------------------------------------------------------------------         
{-
	  	    putStrLn $ "RandomPolynomial: generating more random polynomials ..."
      	  	    let fName = scscp_CS_RandomPolynomial
          	    let args  = map toOM [n] -- FIXED: [20::Int]
          	    let p3OM  = callSCSCP fName args
          	    -- putStrLn $ "Random polynomial p3 (as (show p)): "++(show p3OM)
          	    putStrLn $ "Random polynomial p3 generated; length as string: "++(show (length (show p3OM)))
                    let p3OM_str :: String
                        p3OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p3OM]
          	    putStrLn $ "Random polynomial p3 (as converted to string): "++(p3OM_str)
          	    let p4OM  = callSCSCP fName args
          	    -- putStrLn $ "Random polynomial p4 (as (show p)): "++(show p4OM)
          	    putStrLn $ "Random polynomial p3 generated; length as string: "++(show (length (show p4OM)))
                    let p4OM_str :: String
                        p4OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p4OM]
          	    putStrLn $ "Random polynomial p4 (as converted to string): "++(p4OM_str)
                    let fName = scscp_CS_KaratsubaStr_x
                    let args  = map toOM [p3OM_str, p4OM_str]
          	    let p34OM  = callSCSCP fName args
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    putStrLn $ "Result polynomial of p3*p4 generated; length as string: "++(show (length (show p34OM)))
                    let p34OM_str :: String
                        p34OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p34OM]
          	    putStrLn $ "Result polynomial of p1*p2 (as converted to string): "++(p34OM_str)
	            -----------------------------------------------------------------------------
                    -- TODO: use scscpKaratsuba to mix GAP and .hs computations
                    let fName = scscp_CS_KaratsubaStr_x -- was: scscp_WS_KaratsubaGAPstr
                    let args  = map toOM [p12OM_str, p34OM_str]
          	    let p1234OM = callSCSCP fName args
           	    putStrLn $ "Result polynomial of p1*p2*p3*p4 generated; length as string: "++(show (length (show p34OM)))
                    let p1234OM_str :: String
                        p1234OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p1234OM]
          	    putStrLn $ "Result polynomial of p1*p2*p3*p4 (as converted to string): "++(p1234OM_str)         	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result of WS_Karatsuba p1_str p2_str (as (show p)): "++(show p03OM)	   
-}

-- ok
runKaratsubaGAP :: Int -> IO ()
runKaratsubaGAP n = do
                    putStrLn $ "Using GAP server to run karatsuba multiplications on polynomials ..."
          	    let p1OM  = callSCSCP scscp_WS_RandomPolynomial ( map toOM [n] )
          	    putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
                    let p1OM_str :: String
                        p1OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p1OM]
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++(p1OM_str)
          	    let p2OM  = callSCSCP scscp_WS_RandomPolynomial ( map toOM [n] )
          	    -- putStrLn $ "Random polynomial p2 (as (show p)): "++(show p2OM)
          	    putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
                    let p2OM_str :: String
                        p2OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p2OM]
          	    putStrLn $ "Random polynomial p2 (as converted to string): "++(p2OM_str)
          	    putStrLn $ "Generated 2 polynomials; now calling Karatsuba multiplication ..."
          	    let p12OM  = callSCSCP scscp_WS_KaratsubaStr_x ( map toOM [p1OM_str, p2OM_str] )
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    putStrLn $ "Result polynomial of p1*p2 generated; length as string: "++(show (length (show p12OM)))
                    let p12OM_str :: String
                        p12OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p12OM]
          	    putStrLn $ "Result polynomial of p1*p2 (as converted to string): "++(p12OM_str)
                    -----------------------------------------------------------------------------         
	  	    putStrLn $ "RandomPolynomial: generating more random polynomials ..."
          	    let p3OM  = callSCSCP scscp_WS_RandomPolynomial ( map toOM [n] )
          	    putStrLn $ "Random polynomial p3 generated; length as string: "++(show (length (show p3OM)))
          	    -- putStrLn $ "Random polynomial p3 (as (show p)): "++(show p3OM)
                    let p3OM_str :: String
                        p3OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p3OM]
          	    putStrLn $ "Random polynomial p3 (as converted to string): "++(p3OM_str)
          	    let p4OM  = callSCSCP scscp_WS_RandomPolynomial ( map toOM [n] )
          	    putStrLn $ "Random polynomial p4 generated; length as string: "++(show (length (show p4OM)))
                    let p4OM_str :: String
                        p4OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p4OM]
          	    putStrLn $ "Random polynomial p4 (as converted to string): "++(p4OM_str)
          	    -- putStrLn $ "Random polynomial p4 (as (show p)): "++(show p4OM)
          	    let p34OM  = callSCSCP scscp_WS_KaratsubaStr_x ( map toOM [p3OM_str, p4OM_str] )
          	    putStrLn $ "Result polynomial of p3*p4 generated; length as string: "++(show (length (show p34OM)))

                    let p34OM_str :: String
                        p34OM_str = fromOM $ p34OM -- callSCSCP scscp_WS_Polynomial2String [p34OM]
          	    putStrLn $ "Result polynomial of p3*p4 (as converted to string): "++(p34OM_str)

	            -----------------------------------------------------------------------------
          	    let p1234OM = callSCSCP scscp_WS_KaratsubaStr_x ( map toOM [p12OM_str, p34OM_str] )
           	    putStrLn $ "Result polynomial of p1*p2*p3*p4 generated; length as string: "++(show (length (show p34OM)))
                    let p1234OM_str :: String
                        p1234OM_str = fromOM $ p1234OM -- callSCSCP scscp_WS_Polynomial2String [p1234OM]
          	    putStrLn $ "Result polynomial of p1*p2*p3*p4 (as converted to string): "++(p1234OM_str)
{- CHECKME: this code causes a loop 
                    let p1234OM_str :: String
                        p1234OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p1234OM]
          	    putStrLn $ "Result polynomial of p1*p2*p3*p4 (as converted to string): "++(p1234OM_str)
-}


runKaratsuba :: Int -> IO ()
runKaratsuba n = do
                    -- to be run on Coordination server
                    putStrLn $ "Using Coordination server to run sequential karatsuba multiplications on polynomials ..."
          	    let p1OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n] )
          	    -- putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++((fromOM p1OM)::String)
          	    let p2OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n+1] )
          	    -- putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
          	    putStrLn $ "Random polynomial p2 (as converted to string): "++((fromOM p2OM)::String)
          	    putStrLn $ "Generated 2 polynomials; now calling Karatsuba multiplication ..."
          	    let p12OM  = callSCSCP scscp_CS_KaratsubaStr_x [p1OM, p2OM] 
          	    -- putStrLn $ "Result polynomial of p1*p2*p3*p4 generated; length as string: "++(show (length (show p1234OM)))
          	    putStrLn $ "Result polynomial of p1*p2 (as converted to string): "++((fromOM p12OM)::String)

-- ok
runKaratsubaPar :: Int -> IO ()
runKaratsubaPar n = do
                    -- to be run on Coordination server
                    putStrLn $ "Using Coordination server to run parallel karatsuba multiplications on polynomials ..."
          	    let p1OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n] )
          	    -- putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++((fromOM p1OM)::String)
          	    let p2OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n+1] )
          	    -- putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
          	    putStrLn $ "Random polynomial p2 (as converted to string): "++((fromOM p2OM)::String)
          	    let p3OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n+2] )
          	    -- putStrLn $ "Random polynomial p3 generated; length as string: "++(show (length (show p3OM)))
          	    putStrLn $ "Random polynomial p3 (as converted to string): "++((fromOM p3OM)::String)
          	    let p4OM  = callSCSCP scscp_CS_RandomPolynomialAsString  ( map toOM [n+3] )
          	    -- putStrLn $ "Random polynomial p4 generated; length as string: "++(show (length (show p4OM)))
          	    putStrLn $ "Random polynomial p4 (as converted to string): "++((fromOM p4OM)::String)
          	    putStrLn $ "Generated 4 polynomials; now calling Karatsuba multiplication ..."
          	    let p1234OM  = callSCSCP scscp_CS_KaratsubaPar [p1OM, p2OM, p3OM, p4OM] 
          	    -- putStrLn $ "Result polynomial of p1*p2*p3*p4 generated; length as string: "++(show (length (show p1234OM)))
          	    putStrLn $ "Result polynomial of p1*p2*p3*p4 (as converted to string): "++((fromOM p1234OM)::String)

-- as above but using polynomial, rather than string representation
-- fails at the moment
runKaratsubaPar0 :: Int -> IO ()
runKaratsubaPar0 n = do
                    -- to be run on Coordination server
                    putStrLn $ "Using Coordination server to run parallel karatsuba multiplications on polynomials ..."
          	    let p1OM  = callSCSCP scscp_CS_RandomPolynomial  ( map toOM [n] )
                    let p1OM_str :: String
                        p1OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p1OM]
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++(p1OM_str)
          	    -- putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
          	    -- putStrLn $ "Random polynomial p1 (as converted to string): "++((fromOM p1OM)::String)
          	    let p2OM  = callSCSCP scscp_CS_RandomPolynomial  ( map toOM [n+1] )
          	    -- putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
          	    -- putStrLn $ "Random polynomial p2 (as converted to string): "++((fromOM p2OM)::String)
          	    let p3OM  = callSCSCP scscp_CS_RandomPolynomial  ( map toOM [n+2] )
          	    -- putStrLn $ "Random polynomial p3 generated; length as string: "++(show (length (show p3OM)))
          	    -- putStrLn $ "Random polynomial p3 (as converted to string): "++((fromOM p3OM)::String)
          	    let p4OM  = callSCSCP scscp_CS_RandomPolynomial  ( map toOM [n+3] )
          	    -- putStrLn $ "Random polynomial p4 generated; length as string: "++(show (length (show p4OM)))
          	    -- putStrLn $ "Random polynomial p4 (as converted to string): "++((fromOM p4OM)::String)
          	    putStrLn $ "Generated 4 polynomials; now calling Karatsuba multiplication ..."
          	    let p1234OM  = callSCSCP scscp_CS_KaratsubaPar [p1OM, p2OM, p3OM, p4OM] 
          	    -- putStrLn $ "Result polynomial of p1*p2*p3*p4 generated; length as string: "++(show (length (show p1234OM)))
                    let p1234OM_str :: String
                        p1234OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p1234OM]
          	    putStrLn $ "Result polynomial of p1*p2*p3*p4 (as converted to string): "++(p1234OM_str)

runResultant :: Int -> IO ()
runResultant n = do
                    -- to be run on GAP server
                    putStrLn $ "Using GAP server to run resultant computation on two random polynomials ..."
          	    let p1OM_str :: String  = fromOM $ callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
--           	    putStrLn $ "Random polynomial p1 generated; length as string: "++(show (length (show p1OM)))
--                     let p1OM_str :: String
--                         p1OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p1OM]
          	    putStrLn $ "Random polynomial p1 (as converted to string): "++(p1OM_str)
          	    let p2OM_str :: String  = fromOM $ callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n+1] )
          	    -- putStrLn $ "Random polynomial p2 (as (show p)): "++(show p2OM)
--           	    putStrLn $ "Random polynomial p2 generated; length as string: "++(show (length (show p2OM)))
--                     let p2OM_str :: String
--                         p2OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p2OM]
          	    putStrLn $ "Random polynomial p2 (as converted to string): "++(p2OM_str)
          	    putStrLn $ "Generated 2 polynomials; now calling resultant computation ..."
          	    let p12OM_str :: String  = fromOM $ callSCSCP scscp_CS_Resultant ( map toOM [p1OM_str, p2OM_str] )
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    -- putStrLn $ "Resultant for p1 p2 generated; length as string: "++(show (length (show p12OM)))
                    -- let p12OM_str :: String
                    --     p12OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p12OM]
          	    putStrLn $ "Resultant of p1 p2 (as converted to string): "++(p12OM_str)

runResultant2 :: Int -> IO ()
runResultant2 n = do
                    -- to be run on coordination server
                    putStrLn $ "Using coordination server to run resultant computations ..."
          	    let p12OM_str :: String  = fromOM $ callSCSCP scscp_CS_Resultant2 ( map toOM [n] )
          	    putStrLn $ "Resultant of p1 p2 (as converted to string): "++(p12OM_str)

runGB :: Int -> IO ()
runGB 0 = do -- special case: input 0 means pick fixed input
                    -- to be run on GAP server
                    putStrLn $ "Running sequential GroebnerBasis computation with fixed inputs ..."
                    -- see GAP manual sec 64.17
          	    let p1OM_str :: String  = "x_1^2+x_2^2+x_3^2-1"
          	    let p2OM_str :: String  = "x_1^2+x_3^2-x_2"
          	    let p3OM_str :: String  = "x_1-x_2"
          	    putStrLn $ "p1 = " ++ p1OM_str
          	    putStrLn $ "p2 = " ++ p2OM_str
          	    putStrLn $ "p3 = " ++ p3OM_str
          	    putStrLn $ "Now calling GroebnerBasis computation ..."
          	    let p123OM_str :: String  = fromOM $ callSCSCP scscp_CS_GroebnerBasis [toOM (3::Int), toOM [p1OM_str, p2OM_str, p3OM_str] ]
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    -- putStrLn $ "Resultant for p1 p2 generated; length as string: "++(show (length (show p12OM)))
                    -- let p12OM_str :: String
                    --     p12OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p12OM]
          	    putStrLn $ "GroebnerBasis of p1, p2, p3 (as converted to string): "++(p123OM_str)

runGB n = do
                    -- to be run on GAP server
                    putStrLn $ "Running sequential GroebnerBasis computation with random polynomials of degree "++(show n)++" fixed inputs ..."
                    -- see GAP manual sec 64.17
          	    let p1OM_str :: String  = fromOM $ callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
          	    let p2OM_str :: String  = fromOM $ callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
          	    let p3OM_str :: String  = fromOM $ callSCSCP scscp_CS_RandomPolynomialAsString ( map toOM [n] )
          	    putStrLn $ "p1 = " ++ p1OM_str
          	    putStrLn $ "p2 = " ++ p2OM_str
          	    putStrLn $ "p3 = " ++ p3OM_str
          	    putStrLn $ "Now calling GroebnerBasis computation ..."
          	    let p123OM_str :: String  = fromOM $ callSCSCP scscp_CS_GroebnerBasis [toOM (3::Int), toOM [p1OM_str, p2OM_str, p3OM_str] ]
          	    -- let p = (read str) :: [Poly]
          	    -- putStrLn $ "Result polynomial of p1*p2 (as (show p)): "++(show p12OM)
          	    -- putStrLn $ "Resultant for p1 p2 generated; length as string: "++(show (length (show p12OM)))
                    -- let p12OM_str :: String
                    --     p12OM_str = fromOM $ callSCSCP scscp_CS_Polynomial2String [p12OM]
          	    putStrLn $ "GroebnerBasis of p1, p2, p3 (as converted to string): "++(p123OM_str)

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
  let fName = if (get_portnum True == 12321) 
                then scscp_CS_Phi
		else if (get_portnum True == 12322)
                       then scscp_HS_Phi
                       else scscp_WS_Phi
  let args  = map toOM [n]
  let resOM = callSCSCP fName args
  -- print the result
  let x = (fromOM resOM) :: Int
  return x

sumEulerWithSCSCP :: Int -> IO Int
sumEulerWithSCSCP n = do
   xs <- mapM eulerWithSCSCP [1..n]
   return (sum xs)

-----------------------------------------------------------------------------
-- evaluate exactly 2 euler numbers in parallel
{-
fun2SCSCP :: Int -> Int -> IO Int
fun2SCSCP m n =  do
                       -- let l = length xs `seq` sum' xs
                       let ns = (mkList n) 
		       putStrLn $ "sumEuler2SCSCP "++(show m)++" "++(show n)++" ..."
                       -- let x = deLift $ createProcess (process (\n -> unsafePerformIO (eulerWithSCSCP n))) m
                       x <- eulerWithSCSCP m
                       let y = deLift $ createProcess (process (\n -> unsafePerformIO (eulerWithSCSCP n))) n
                       return (x+y)
-}                      

-----------------------------------------------------------------------------
-- handling polynomials (see PolyGCD.hs)

---------------
-- encoding polynomials: 
{-
This is X^2 + 2.
  <OMA>
    <OMS name="DMP" cd="polyd1"/>
    <OMA>
      <OMS name="poly_ring_d_named" cd="polyd1"/>
      <OMS name="Q" cd="fieldname1"/>
      <OMV name="X"/>
    </OMA>
    <OMA>
      <OMS name="SDMP" cd="polyd1"/>
      ------
      <OMA>
      <OMS name="term" cd="polyd1"/>
	<OMI> 1 </OMI>
	<OMI> 2 </OMI>
      </OMA>
      <OMA>
	<OMS name="term" cd="polyd1"/>
	<OMI> 2 </OMI>
	<OMI> 0 </OMI>
      </OMA>
      ------
    </OMA>
  </OMA>
</OMOBJ>

The polynomial is encoded as pairs of coefficients and degree
-}
encodePoly :: [(Int,Int)] -> OMObj
encodePoly []    = encodePoly [(0,0),(0,0)]
encodePoly coeff = OM "OMA" [] (prefix ++ write coeff ++ suffix)
    where prefix = "<OMS name=\"DMP\" cd=\"polyd1\"/>" ++ 
                     "<OMA>" ++ 
                      "<OMS name=\"poly_ring_d_named\" cd=\"polyd1\"/>" ++
                      "<OMS name=\"Z\" cd=\"setname1\"/>" ++
                      "<OMV name=\"X\"/>" ++
                     "</OMA>" ++
                     "<OMA><OMS name=\"SDMP\" cd=\"polyd1\"/>"
                     -- terms go here
          suffix =   "</OMA>"
          writeInt = writeOMObj . toOM
          write [] = ""
          write ((coeff,deg):rest) = 
              "<OMA>" ++ termSym ++ writeInt coeff ++ writeInt deg ++ "</OMA>" 
              ++ write rest
          termSym ="<OMS name=\"term\" cd=\"polyd1\"/>"




{- GAP example polynomials:

gap> R := PolynomialRing(Rationals ,1);;
gap> SetOpenMathDefaultPolynomialRing(R);
gap> f:=RandomPol(Rationals,2);
-1/2*x_1^2+3*x_1+4
gap> OMPrint(f);
<OMOBJ>
        <OMA>
                <OMS cd="polyd1" name="DMP"/>
                <OMA id="polyringApukoif2tGBDl8YC" >
                        <OMS cd="polyd1" name="poly_ring_d_named"/>
                        <OMS cd="setname1" name="Q"/>
                        <OMV name="x_1"/>
                </OMA>
                <OMA>
                        <OMS cd="polyd1" name="SDMP"/>
                        <OMA>
                                <OMS cd="polyd1" name="term"/>
                                <OMA>
                                        <OMS cd="nums1" name="rational"/>
                                        <OMI>-1</OMI>
                                        <OMI>2</OMI>
                                </OMA>
                                <OMI>2</OMI>
                        </OMA>
                        <OMA>
                                <OMS cd="polyd1" name="term"/>
                                <OMI>3</OMI>
                                <OMI>1</OMI>
                        </OMA>
                        <OMA>
                                <OMS cd="polyd1" name="term"/>
                                <OMI>4</OMI>
                                <OMI>0</OMI>
                        </OMA>
                </OMA>
        </OMA>
</OMOBJ>
gap> g:=RandomPol(Rationals,2);
-x_1^2+1/2
gap> OMPrint(g);
<OMOBJ>
        <OMA>
                <OMS cd="polyd1" name="DMP"/>
                <OMA id="polyringApukoif2tGBDl8YC" >
                        <OMS cd="polyd1" name="poly_ring_d_named"/>
                        <OMS cd="setname1" name="Q"/>
                        <OMV name="x_1"/>
                </OMA>
                <OMA>
                        <OMS cd="polyd1" name="SDMP"/>
                        <OMA>
                                <OMS cd="polyd1" name="term"/>
                                <OMI>-1</OMI>
                                <OMI>2</OMI>
                        </OMA>
                        <OMA>
                                <OMS cd="polyd1" name="term"/>
                                <OMA>
                                        <OMS cd="nums1" name="rational"/>
                                        <OMI>1</OMI>
                                        <OMI>2</OMI>
                                </OMA>
                                <OMI>0</OMI>
                        </OMA>
                </OMA>
        </OMA>
</OMOBJ>
-}
