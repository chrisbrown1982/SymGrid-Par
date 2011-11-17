{-# OPTIONS_GHC -cpp -XParallelListComp -XPatternSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-} 
-- Time-stamp: <Mon Jun 28 2010 23:04:14 Stardate: Stardate: [-28]3294.38 hwloidl>
--
-- (c) 03/2010 Hans-Wolfgang Loidl (hwloidl@cs.st-andrews.ac.uk)
-- (c) 01/2009 Jost Berthold (jb@cs.st-andrews.ac.uk)
--
-- This is the top-level file for the SymGrid-Par Coordination Server.
-- The Coordination server orchestrated the (parallel) execution of a
-- program, usually trigger by an SCSCP call. It uses a version of 
-- parallel Haskell, currently Eden, to realise the parallelism.
-- The main work is done through SCSCP calls to connected CA servers.
-- 
-- For an overview of the architecture see:
--   http://www.cs.st-andrews.ac.uk/~hwloidl/SCIEnce/SymGrid-Par/demo.pdf
-- 
-- The Coordination Server has two roles:
-- It acts as a server, receiving SCSCP requests from the outside.
-- It acts as a client, sending SCSCP request for computations in CAS.
-- Its main role is to coordinate the (parallel) execution, preferably
-- using skeletons to manage the parallel tasks.
-- As an extension to this functionality, it might also interact
-- with CAS using the bespoke, pipe-based communication.
--
-----------------------------------------------------------------------------

module Main where

import Date 

#undef CASH

#define SUMEULER_SCSCP 1

-- general libraries
import Char
import Maybe
import List
-- general networking stuff
import Network
-- general system stuff
import System
import System.IO
import qualified System.Process      -- only for launching CAS servers if requested
import System.Environment(getEnv)
import System.Console.GetOpt
#ifdef __PARALLEL_HASKELL__ 
import System.IO.Unsafe
#endif
-- various monads
import Control.Monad
import Control.Monad.State
import Control.Concurrent
#if __GLASGOW_HASKELL__ > 610
import qualified Control.OldException as C -- catching severe errors 
                              -- => shutting down the server
#else
import qualified Control.Exception as C -- catching severe errors 
                              -- => shutting down the server
#endif
import qualified IO as IO

-- this contains the main SCSCP functionality
import ParSCSCP
import HS_SCSCP
import HS2SCSCP

-- seq interface (old)
-- import SCSCP_API

-- examples how to use it
import SCSCP_Examples

#ifdef __PARALLEL_HASKELL__ 
-- import EdenHelpers -- helper functions
-- import FoldDM
import Control.Parallel.Eden hiding (release)
#endif

-- abstractions over patterns of parallel coordination
import CA_Skeletons

-- these are some basic known services
import BaseServices

import SGPTypes

-----------------------------------------------------------------------------
-- state of the Coordination server
data CS_State = CS_State { supported :: [(CAName, [OMObj] -> Either String OMObj)] 
                         , service_description :: OMObj
                         }

-- the Coordination server monad
type CSM a = State CS_State a

emptyState = CS_State [] (constructServiceDescr "Haskell SCSCP CoordinationServer" -- name
                                                "0.1" -- version 
                                                "Early prototype" -- description
                          ) -- TODO: use coordinationServerInit here

-----------------------------------------------------------------------------
-- config info

#ifdef __PARALLEL_HASKELL__ 
defaultHost    = "localhost"
defaultPortNum = 26133

-- ardbeg_servers = mk_servers' "ardbeg" 26132

mk_servers' host port = [ SCSCPServer host (PortNumber (port+i)) Nothing
                        | i <- [1..4] ]

mk_servers srvs = [ SCSCPServer host (PortNumber port) Nothing
                  | (host,port) <- srvs ]
#endif
data Args = Args { startup_test :: Bool, noLoop :: Bool, do_launch_servers :: Bool, verbose :: Bool, debug :: Bool, version :: Bool, help :: Bool, config_file :: Maybe String }

initArgs = Args { startup_test = False, noLoop = False, do_launch_servers = False, verbose = False, debug = False, version = False, help = False, config_file = Nothing }

optDescrs :: [OptDescr (Args -> Args)]
optDescrs =
  [ Option ['?','h'] ["help"]        (NoArg (\ args -> args { help = True }))     "show usage information"
  , Option ['v'] ["verbose"]         (NoArg (\ args -> args { verbose = True }))     "be verbose"
  , Option ['d'] ["debug"]           (NoArg (\ args -> args { debug = True }))     "debugging information"
  , Option ['V'] ["version"]         (NoArg (\ args -> args { version = True }))     "show version"
  , Option ['s'] ["startup-test"]    (NoArg (\ args -> args { startup_test = True }))     " startup test"
  , Option ['C'] ["launch-servers"]  (NoArg (\ args -> args { do_launch_servers = True }))     " launch CAS servers via shell command"
  , Option ['c'] ["config","sgprc"]  (ReqArg (\ opts -> \ args -> args { config_file = Just opts }) "configuration file") "read configuration for file for local CA servers and launch them"
  , Option ['l'] ["nl","noloop"]     (NoArg (\ args -> args { noLoop = True }))     " only one iteration of server (useful for generating parallelism profiles)"
  ]

usage :: [String] -> IO a
usage errs = ioError (userError (concat errs ++ usageInfo header optDescrs))
    where header = "\nUsage: CoordinationServer_mp <port> [options]"

version_info :: String
version_info = (List.concat (List.intersperse " " [sys_name, release, "built at", date, "for",  hw_os, "on", host_name, "installed in", sgp_root]))

processArgs argv = do
	                let (os, non_opt, errs) = getOpt Permute optDescrs argv
                        let args = applyAll initArgs os
	                when (help args) $
                         usage errs
                        return (args, non_opt, errs)

applyAll :: Args -> [(Args -> Args)] -> Args
applyAll a [] = a
applyAll a (f:fs) = applyAll (f a) fs

#ifndef CASH
main :: IO()
main  = do 
          args' <- getArgs
	  (opts, args, errs) <- processArgs args'
          when (version opts)  (ioError (userError version_info))
          when (help opts)     (usage [])
          -- args <- getArgs
          let portNum = if null args  -- server side: port of top-level client to listen to
                         then 12321
                         else fromInteger (read (head args))
          --  init ...
#ifdef __PARALLEL_HASKELL__ 
          -- putStrLn $ "Initialising parallel client on port " ++ (show portNum') ++ "..."
          servers <- readConfig opts
          let (ports,hosts,cas) = unzip3 servers 
          let srvs = mk_servers (zip ports hosts)
          when (do_launch_servers opts) $
             launch_cas_servers opts servers
          when (verbose opts) $
             putStrLn $ show_config srvs
          initServers srvs
#else
          -- putStrLn $ "Initialising sequential PAR_SCSCP client (async) on port " ++ (show portNum') ++ "..."
	  let srv = server defaultHost defaultPortNum
          when (verbose opts) $
             putStrLn $ show_config [srv] -- "Server configuration: "++(show srv)
          initServer True{-async-} srv
#endif
#ifdef __PARALLEL_HASKELL__ 
          -- doing some parallel computation on startup, just to test the system
          when (startup_test opts) $ do
              putStrLn $ "Testing parallel setup first ..."
              putStrLn $ "Computing sumEulerShuffleSCSCP 60 30 ..."
              res <- sumEulerShuffleSCSCP 60 30
              putStrLn $ "Result: "++(show res)
              putStrLn $ "Testing parallel setup first ..."
              putStrLn $ "Computing sumEulerParMapSCSCP 60 30..."
              res <- sumEulerParMapSCSCP 60 30
              putStrLn $ "Result: "++(show res)
#endif
          putStrLn $ "Installing all services ..."
          let init_st = install_all 
	  coordServer portNum opts init_st
#endif

-- config file is defined by env var SGPRC or local .sgprc
readConfig opts = 
 catch (do
  name <- if (isNothing (config_file opts))
            then catch (getEnv "SGPRC") (\ e -> return ".sgprc")
            else return (fromJust (config_file opts))
  str <- readFile name
  let wss = map words (lines str)
  let srvs = map ( \ ws -> if (length ws < 2) || (not (all Char.isDigit (head (tail ws))))  -- ignoring additional fields
                                then error $ "Illegal line in config file "++name++":\n"++(show (unwords ws))++"\nshould be: host port"
                                else (ws!!0, fromIntegral ((read (ws!!1)) :: Int), ws!!2) ) wss
  return srvs)
 (\e -> do 
         -- when (debug opts) $ putStrLn $ "Warning: no .sgprc file found; using test config: "++(show srvs)
         if (IO.isDoesNotExistError e) 
          then do
                when (debug opts) $ putStrLn $ "Config file does not exist; using test setup of localhost:26133 and localhost:26134"
                return ()
          else putStrLn $ "Error trying to read config file: "++(show e)
         let test_srvs = [(defaultHost, defaultPortNum, "GAP"), (defaultHost, defaultPortNum+1, "GAP")] -- test setup
	 return test_srvs
 )

show_config srvs =  "Server configuration:\n"++(unlines (map show srvs))

-- launch CAS servers; only for GAP and only on localhost for now
launch_cas_servers opts [] = return ()
launch_cas_servers opts ((h,p,s):srvs) 
  | h /= "localhost" = error "launch_servers: Can only launch CAS on localhost for now"
  | s /= "GAP"       = error "launch_servers: Can only launch GAP as CA for now"
  | otherwise        = do root <- catch (getEnv "SGP_ROOT") (\ e -> return ".")
                          let str = "cd "++root++"/lib ; "++root ++ "/bin/gapd.sh -p "++(show p)++" -t "++root++"/lib/sgp_server.g"
                          when (debug opts) $ putStrLn ("Launching: "++str)
                          -- this assumes that we start a demon, which returns immediately
                          ph <- System.Process.runCommand str
                          rc <- System.Process.waitForProcess ph
                          when (debug opts) $ putStrLn ("Return code is : "++(show rc))
                          launch_cas_servers opts srvs

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- This is the top-level server function
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

coordServer :: PortNumber -> Args -> CS_State -> IO ()
coordServer portNum opts st = do
 traceM (".. starting coordination server on port " ++ (show portNum))
 sock <- listenOn (PortNumber portNum)
 C.catch (do (h,name,num) <- accept sock
	     hSetBuffering h NoBuffering
	     traceM ".. someone just connected ..."
	     hPutStrLn h (writePI (coordServerInit portNum))
	     answer <- hGetLine h
	     traceM (".. client sent " ++ answer )
             let str = (writePI (Version "1.2"))
	     hPutStrLn h str
	     traceM (".. server responds with "++str)
	     scscpInteract h st
	     traceM (".. server quits interaction "++str)
	     hPutStrLn h (writePI (Quit (Just "Quitting")))
	     sClose sock
	     traceM ".. exiting ...")
      (\(e::C.Exception) -> 
          do traceM (".. something went wrong...\n" ++ show e)
             sClose sock    
             return ())     
 unless (noLoop opts) $
   coordServer portNum opts st -- iterate

coordServerInit :: PortNumber -> SCSCP_PI
coordServerInit portNum = Init { piInitName= "CoordinationServer"
                      	       , piInitV   = "0.3"
                      	       , piInitID  = "coordinationServer:" ++ show portNum
                      	       , piInitSCSCPs  = words ("1.2 1.2beta 1.2.345")}

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- This is the main server loop
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

scscpInteract :: Handle -> CS_State -> IO()
scscpInteract h st = do 
         traceM ".. interact: expecting some input"
         -- expect a PCall message, enclosed in PIs
         (between,pi) <- splitAtPI h
         when (not (null between)) 
           (traceM ("WARNING: ignored input before PI: " ++ between))
         case pi of 
           Quit _ -> do traceM ".. received quit message"
                        return ()
           Start  -> do (input,pi) <- splitAtPI h
                        case pi of 
                          Cancel -> do traceM ".. canceled, start over"
                                       scscpInteract h st
                          End -> do 
                            let msg = readSCSCPMsg input
                            traceM ".. received the following:"
                            traceM (show msg)
			    answer <- processThis msg st
                            -- threadDelay 1000 
                            hPutStrLn h (writePI Start)
                            hPutStrLn h (writeSCSCPMsg answer)
                            hPutStrLn h (writePI End)
                          Quit _ -> do traceM ".. received quit message"
                                       return ()
                          other -> traceM (".. unexpected PI after Start: " 
                                             ++ show other)
           other -> traceM (".. unexpected PI (ignoring it): " 
                              ++ show other)
         scscpInteract h st -- start over

-- worker functions processing all possible inputs
processThis :: SCSCPMsg -> CS_State -> IO SCSCPMsg 
processThis (PCall id name dat opts) st
    -- | name `elem` supportedNames  -- hardwired names
    -- EVIL CODE DUPLICATION; merge this with the branch below
    | name `elem` map fst supported_caStandard -- standard service (needs st as arg)
       = do  putStrLn {-traceM-} (".. supported CA STANDARD call message for "++(show name))
             C.catch (let Just f = lookup name supported_caStandard
		          r = f st dat  -- only difference to case below: st as arg
		      in case r of
	               Left msg  -> do traceM $ ".. this error while processing: "++msg
                                       return $ 
	                                  PTerminated id (CAMsg msg) 
                                                      Nothing Nothing
	               Right res -> do putStr ".. successful, result: "
                                       traceM (show res)
                                       return $
	                                 PResult res id Nothing Nothing
		  ) (\(e::C.Exception) ->    do traceM (".. exception:\n" ++ show e)
                              		     	return $ 
  	                      		     	   PTerminated id (CAMsg (show e)) 
                              		     	               Nothing Nothing
		    )    
    -- End of EVIL CODE DUPLICATION
    | name `elem` map fst (supported st) -- is the name known in the CS state
	= do traceM (".. supported call message for "++(show name))
             C.catch (let Just f = lookup name (supported st)
		          r = f dat  -- EXEC the fct on the server
		      in case r of
	               Left msg  -> do traceM $ ".. this error while processing: "++msg
                                       return $ 
	                                  PTerminated id (CAMsg msg) 
                                                      Nothing Nothing
	               Right res -> do putStr ".. successful, result: "
                                       traceM (show res)
                                       return $
	                                 PResult res id Nothing Nothing
		  ) (\(e::C.Exception) ->   do traceM (".. exception:\n" ++ show e)
                              		       return $ 
  	                      		        PTerminated id (CAMsg (show e)) 
                              		                    Nothing Nothing
		    )
processThis anyMessage _
    = do traceM $ ".. unsupported call message: "++(show anyMessage)
         return $ PTerminated (callID anyMessage) 
                              (CAMsg "CoordinationServer") 
			      Nothing Nothing

-------------------------------------------------------------
-- main server functionality starts here
-------------------------------------------------------------
-- these are now installed via installSCSCPprocedure below
-- assoc lists of services to Haskell functions, defined here, over OMObjs

supported_local = [ (scscp_HS_Phi            , phiOM             ) -- , "Integer -> Integer"                         )
                  , (scscp_WS_Phi	     , fwdPhiOM        	 ) -- , "Integer -> Integer"                         )
                  , (scscp_CS_Phi	     , phiOM        	 ) -- , "Integer -> Integer"                         )
		  , (scscp_CS_Fib            , fibOM             ) -- , "Integer -> Integer"                         )
                  , (scscp_HS_Plus	     , plusOM            ) -- , "Integer -> Integer"                         )
                  , (scscp_HS_Factorial	     , factOM          	 ) -- , "Integer -> Integer"                         ) 
                  , (scscp_HS_FactorialAcc   , factAccOM       	 ) -- , "Integer -> Integer -> Integer"              )
#if 0
                  , (scscp_HS_ProductPoly    , productPolyOM   	 ) -- , "Poly -> Poly -> Poly"                       ) 
                  , (scscp_HS_SumPoly	     , sumPolyOM       	 ) -- , "Poly -> Poly -> Poly"                       ) 
                  , (scscp_HS_DifferencePoly , differencePolyOM	 ) -- , "Poly -> Poly -> Poly"                       ) 
                  , (scscp_HS_QuotientPoly   , quotientPolyOM  	 ) -- , "Poly -> Poly -> Poly"                       ) 
#endif
		  ]						 


#ifdef __PARALLEL_HASKELL__
-- NB: not all parallel, in fact
supported_par = [ (scscp_CS_SumEulerShuffle              , sumEulerShuffleSCSCPOM  ) -- , "")
           	, (scscp_CS_KaratsubaPar		 , karatsubaParOM          ) -- , "")
           	, (scscp_CS_KaratsubaPar0		 , karatsubaPar_no_string_OM  ) -- , "")
           	, (scscp_CS_KaratsubaStr_x		 , karatsubaGAPOM_x        ) -- , "")
           	, (scscp_CS_Karatsuba		         , karatsubaGAPOM          ) -- , "")
           	, (scscp_CS_KaratsubaStr		 , karatsubaGAPOM_         ) -- , "")
                , (scscp_CS_Resultant2                   , resultant2OM          )
          	, (scscp_CS_Resultant		         , resultantGAPOM          ) -- , "")
          	, (scscp_CS_Res		         , resGAPOM          ) -- , "")
          	, (scscp_CS_GroebnerBasis		 , groebnerBasisGAPOM           ) -- , "")
           	, (scscp_CS_SumEulerClassic		 , sumEulerClassicOM       ) -- , "")
           	, (scscp_CS_SumEuler2		         , sumEuler2OM             ) -- , "")
           	, (scscp_CS_SumEuler	                 , sumEulerParOM             ) -- , "") -- demo version
           	, (scscp_CS_SumEulerPar	                 , sumEulerParOM             ) -- , "")
           	, (scscp_CS_ParProcesses1		 , parProcesses1OM         ) -- , "")
           	, (scscp_CS_ParProcesses2		 , parProcesses2OM         ) -- , "")
           	, (scscp_CS_ParList		         , parListOM               ) -- , "")
           	, (scscp_CS_ParMap		         , parMapOM                ) -- , "")
           	, (scscp_CS_ParMap'		         , parMap'OM                ) -- , "")
           	, (scscp_CS_ParMapFold		         , parMapFoldOM            ) -- , "")
           	, (scscp_CS_ParMapFold1		         , parMapFold1OM            ) -- , "")
           	, (scscp_CS_ParZipWith		         , parZipWithOM            ) -- , "")
           	, (scscp_CS_Map		         , mapOM                ) -- , "")
           	, (scscp_CS_Map'		         , map'OM                ) -- , "")
           	, (scscp_CS_MapFold		         , mapFoldOM            ) -- , "")
           	, (scscp_CS_MapFold1		         , mapFold1OM            ) -- , "")
           	, (scscp_CS_ZipWith		         , zipWithOM            ) -- , "")
           	, (scscp_CS_RandomPolynomial		 , randomPolyOM            ) -- , "")
           	, (scscp_CS_RandomPolynomialAsString	 , randomPolyAsStringOM    ) -- , "")
           	, (scscp_CS_Polynomial2String	         , polynomial2StringOM     ) -- , "")
                , (scscp_CS_Orbit                        , parOrbitOM            ) -- added by CMB 14/07/2010
                , (scscp_CS_HMI				 , parHMIOM              )
                ]
#endif

supported_all :: [(CAName, [OMObj] -> Either String OMObj)] -- , String)]
--            name of service, worker function,              type as string (unused for now)
supported_all = {- supported_caStandard ++ -} supported_local ++ supported_par 
-- zip supportedNames supportedFs

supportedNames = map fst supported_all
supportedFcts  = map snd supported_all

installSCSCPprocedure :: (CAName, [OMObj] -> Either String OMObj) -> CS_State -> CS_State
installSCSCPprocedure x@(service, fct) (CS_State xs sd) = CS_State (xs++[x]) sd

install_all = foldr installSCSCPprocedure emptyState supported_all

-------------------------------------------------------------
-- and the details...

caStandardNames :: [CAName]
caStandardNames = map Right 
		  [ GetAllowedHeads -- returns "symbol_sets" 
                  , GetSignature    -- returns "signature"
                    -- (OMSymbol,minarg,maxarg,(list of) symbol_sets )
                    --      JB: pTerminated if not supported? Guess so.
                  , GetTransientCD  -- returns server-specific Content Dictionary
                    --      (should prefix SCSCP_transient_)
                  , GetServiceDescr -- returns "service_description", 
		    -- containing 3 Strings: 
                    --      CA system name, version, and descriptive text
                  , StoreObj           -- computes an object, stores it and returns CARef
                  , RetrieveObj   -- takes ref, returns the OMObj
                  , UnbindObj     -- deletes a CARef'ed object from the server
		  ]

caStandardOps = [ handleMe ((\ (Right op) -> op) op) | op <- caStandardNames ]
caStandardTys = take (length caStandardNames) (repeat "CAStandardName -> [OMObj] -> Either String OMObj")

supported_caStandard = zip caStandardNames caStandardOps -- caStandardTys

-----------------------------------------------------------------------------
-- OMData instances for some std Haskell data structures etc

-- HWL HACK: uses String and underlying show/read; works only for Haskell side
instance (Show a, Read a, OMData a, Show b, Read b, OMData b) => OMData (Either a b) where
    toOM x    = OM "OMSTR" [] (encodeXML (show x))
    fromOM (OM "OMSTR" [] txt) = read txt
    fromOM other = error ("cannot treat " ++ show other ++ " as Either")

-- HWL HACK: uses String and underlying show/read; works only for Haskell side
instance (Show a, Read a, OMData a, Show b, Read b, OMData b) => OMData (a, b) where
    toOM x    = OM "OMSTR" [] (encodeXML (show x))
    fromOM (OM "OMSTR" [] txt) = read txt
    fromOM other = error ("cannot treat " ++ show other ++ " as (,)")

-- HWL HACK: uses String and underlying show/read; works only for Haskell side
instance OMData CAStandardName where
    toOM x    = OM "OMSTR" [] (encodeXML (show x))
    fromOM (OM "OMSTR" [] txt) = read txt
    fromOM other = error ("cannot treat " ++ show other ++ " as CAStandardName")

-----------------------------------------------------------------------------
-- handler functions for builtin services

handleMe :: CAStandardName -> CS_State -> [OMObj] -> Either String OMObj
handleMe GetAllowedHeads st _ = Right (toOM (List.concat (List.intersperse "," (map show (map fst (supported st)))))) -- TODO: return a proper symbol_set
handleMe GetSignature _ fs   = Left  ("GetSignature not implemented")
{-
                              Right ( toOM -- UNCHECKED
                              [ case lookup ((fromOM f)::CAName) supported_tys of {
                                  Nothing  -> Left  ("GetSignature not implemented")
                                ; (Just ty_str) -> Right ty_str }
                              | f <- fs ])
-}
handleMe GetTransientCD _ _ = Left ("GetTransientCD not implemented")
handleMe GetServiceDescr st _ = Right (service_description st)
handleMe StoreObj _ _    = Left ("StoreObj not implemented")
handleMe RetrieveObj _ _ = Left ("RetrieveObj not implemented")
handleMe UnbindObj _ _   = Left ("UnbindObj not implemented")

-- constructServiceDescr etc defined in HS_SCSCP

-----------------------------------------------------------------------------
-- worker functions for concrete computations

--------------
-- Euler totient function, computed on Haskell-side, returned as OMObj

phiOM :: [OMObj] -> Either String OMObj
phiOM []  = Left $ "phiOM: no argument"
phiOM [x] = Right (toOM (dumbPhi (fromOM x)))
phiOM xs  = Left $ "phiOM: too many arguments"++(show xs)

dumbPhi :: Integer -> Integer
dumbPhi n | n < 2     = 0
          | otherwise = fromIntegral $ 
                        length [ p | p <- [1..n-1], gcd n p == 1 ]

--------------
--  

plusOM :: [OMObj] -> Either String OMObj
plusOM [] = Right (toOM (0::Integer))
plusOM [x] -- arg. will be a list!
    = let nums :: [Integer] -- fixing the type
          nums = fromOM x
      in Right (toOM (sum nums))

plus xs = let nums :: [Integer] -- fixing the type
	      nums = map fromOM xs
          in Right (toOM (sum nums))

---------------
-- Haskell side factorial

factOM :: [OMObj] -> Either String OMObj
factOM [n'] = let n :: Integer
	          n = fromOM n'
              in Right (toOM (fact n))

fact :: Integer -> Integer
fact 0 = 1
fact n = n*(fact (n-1))

factAccOM :: [OMObj] -> Either String OMObj
factAccOM [m',n'] = let m :: Integer
	                m = fromOM m'
                        n :: Integer
	                n = fromOM n' 
                    in Right (toOM (factAcc m n))

factAcc :: Integer -> Integer -> Integer
factAcc 0 acc = acc
factAcc n acc = factAcc (n-1) (n*acc)

---------------
-- GAP side fib

fibOM :: [OMObj] -> Either String OMObj
fibOM args = Right (callSCSCP scscp_WS_Fibonacci args)

-----------------------------------------------------------------------------

{-
doBinOpOM :: (Poly -> Poly -> Poly) -> [OMObj] -> Either String OMObj
-- productPolyOM _ = error "productPolyOM not implemented"
doBinOpOM f [p1',p2'] = let str1 :: String
                      	    str1 = fromOM p1'
                      	    str2 :: String
                      	    str2 = fromOM p2'
                      	    p1 :: Poly
                      	    p1 = read str1
                      	    p2 :: Poly
                      	    p2 = read str2
                      	in  Right (toOM (show (f p1 p2)))

productPolyOM :: [OMObj] -> Either String OMObj
-- productPolyOM _ = error "productPolyOM not implemented"
productPolyOM ps = doBinOpOM product_Poly ps

sumPolyOM :: [OMObj] -> Either String OMObj
-- productPolyOM _ = error "sumPolyOM not implemented"
sumPolyOM ps = doBinOpOM sum_Poly ps

differencePolyOM :: [OMObj] -> Either String OMObj
-- productPolyOM _ = error "sumPolyOM not implemented"
differencePolyOM ps = doBinOpOM difference_Poly ps

quotientPolyOM :: [OMObj] -> Either String OMObj
quotientPolyOM ps = doBinOpOM quotient_Poly ps

-- for Haskell side Karatsuba code
karatsubaOM :: [OMObj] -> Either String OMObj
karatsubaOM [p1',p2'] = let str1 :: String
                      	    str1 = fromOM p1'
                      	    str2 :: String
                      	    str2 = fromOM p2'
                      	    p1 :: [Poly]
                      	    p1 = read str1
                      	    p2 :: [Poly]
                      	    p2 = read str2
                      	in  Right (toOM (show (karatsuba p1 p2)))
-}


-----------------------------------------------------------------------------
-- main worker functions, operating on OMObjs

-- for GAP side Karatsuba code
karatsubaParOM :: [OMObj] -> Either String OMObj
karatsubaParOM args = Right (unsafePerformIO (karatsubaStrSCSCP args))

karatsubaPar_no_string_OM :: [OMObj] -> Either String OMObj
karatsubaPar_no_string_OM args = Right (unsafePerformIO (karatsubaSCSCP args))

-- these are forwarded to the underlying GAP server
fwdPhiOM :: [OMObj] -> Either String OMObj
fwdPhiOM args = Right (callSCSCP scscp_WS_Phi args)

karatsubaGAPOM :: [OMObj] -> Either String OMObj
karatsubaGAPOM args = Right (callSCSCP scscp_WS_Karatsuba args)

karatsubaGAPOM_ :: [OMObj] -> Either String OMObj
karatsubaGAPOM_ args = Right (callSCSCP scscp_WS_KaratsubaStr args)

karatsubaGAPOM_x :: [OMObj] -> Either String OMObj
karatsubaGAPOM_x args = Right (callSCSCP scscp_WS_KaratsubaStr_x args)

randomPolyOM  :: [OMObj] -> Either String OMObj
randomPolyOM args = Right (callSCSCP scscp_WS_RandomPolynomial args)

randomPolyAsStringOM  :: [OMObj] -> Either String OMObj
randomPolyAsStringOM args = Right (callSCSCP scscp_WS_RandomPolynomialAsString args)

polynomial2StringOM  :: [OMObj] -> Either String OMObj
polynomial2StringOM args = Right (callSCSCP scscp_WS_Polynomial2String args)

resultantGAPOM :: [OMObj] -> Either String OMObj
resultantGAPOM args = Right (callSCSCP scscp_WS_Resultant args)

resGAPOM :: [OMObj] -> Either String OMObj
resGAPOM args = Right (callSCSCP scscp_WS_Res args)

groebnerBasisGAPOM :: [OMObj] -> Either String OMObj
groebnerBasisGAPOM args = Right (callSCSCP scscp_WS_GroebnerBasis args)


#ifdef __PARALLEL_HASKELL__

resultant2OM  :: [OMObj] -> Either String OMObj
-- runResultantOM :: Int -> IO ()
resultant2OM [n']= let n :: Int
                       n = fromOM n'
          	       p1OM_str :: String  = fromOM $ callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [n] )
          	       p2OM_str :: String  = fromOM $ callSCSCP scscp_WS_RandomPolynomialAsString ( map toOM [n] )
          	       p12OM  = callSCSCP scscp_WS_Resultant ( map toOM [p1OM_str, p2OM_str] )
                       p12OM_str :: String
                       p12OM_str = fromOM $ callSCSCP scscp_WS_Polynomial2String [p12OM]
		   in Right (toOM p12OM_str)

sumEulerShuffleSCSCPOM :: [OMObj] -> Either String OMObj
sumEulerShuffleSCSCPOM [n',c'] = let n :: Int
                                     n = fromOM n'
                                     c :: Int
                                     c = fromOM c'
                                 in Right (toOM (unsafePerformIO (sumEulerShuffleSCSCP n c)))

sumEulerClassicOM :: [OMObj] -> Either String OMObj
sumEulerClassicOM ns = Right (toOM (unsafePerformIO (sumEulerClassicSCSCP ((map fromOM ns)::[Int]))))

sumEuler2OM :: [OMObj] -> Either String OMObj
sumEuler2OM [m',n'] =       let m :: Int
                                m = fromOM m'
                                n :: Int
                                n = fromOM n'
                            in Right (toOM (unsafePerformIO (sumEuler2SCSCP m n)))

sumEulerParOM :: [OMObj] -> Either String OMObj
sumEulerParOM ns | length ns /= 2 = error $ "PANIC: sumEulerPar expects 2 arguments, but is given "++(show (length ns))
                 | otherwise      = Right (toOM (unsafePerformIO (sumEulerParSCSCP n c)))
                                    where n = fromOM (ns!!0) :: Int 
                                          c = fromOM (ns!!1) :: Int 

-----------------------------------------------------------------------------
-- skeleton interfaces
-- these are fairly verbose and could use abstraction, and common helper functions!!

-- directly passing through 
parProcesses1OM :: [OMObj] -> Either String OMObj
parProcesses1OM (f':xs')           =  let f :: String
                            	          f = fromOM f'
                            	      in Right (unsafePerformIO (parProcesses1 f xs'))


parProcesses2OM :: [OMObj] -> Either String OMObj
parProcesses2OM [f', g', xs', ys'] =  let f :: String
                            	          f = fromOM f'
                            	          g :: String
                            	          g = fromOM g'
                            	          xs :: [Integer]
                            	          xs = fromOM xs'
                            	          ys :: [Integer]
                            	          ys = fromOM ys'
                            	      in Right (toOM (unsafePerformIO (parProcesses2 f g xs ys)))

{- TODO: generalise the above to a version, calling this worker function:
parProcesses :: (OMData a) => [CAName] -> ([a] -> a) -> [[a]] -> IO a
-}

-- apply a services to all elements of a list in parallel
parListOM :: [OMObj] -> Either String OMObj
parListOM [f', xs'] =  let f :: String
                           f = fromOM f'
                           xs :: [Int]
                           xs = fromOM xs'
                       in Right (toOM (unsafePerformIO (parListSCSCP f xs)))

-- apply a service, given as name without CD, to all elements of a list in parallel
parMapOM :: [OMObj] -> Either String OMObj
parMapOM [f', xs'] =   let f :: String
                           f = fromOM f'
                           xs :: [Int]
                           xs = fromOM xs'
                           zs :: [Int]
                           zs = (parMapSCSCP (Left ("scscp_transient_1",f)) xs)
                       in Right (toOM zs)

-- standard fold-of-map skeleton, i.e. Haskell: foldl g x (map f xs)
parMapFoldOM :: [OMObj] -> Either String OMObj
parMapFoldOM [f', g', x', xs'] =  let f :: String
                       	       	      f = fromOM f'
                       	       	      g :: String
                       	       	      g = fromOM g'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                       	       	      x :: Int
                       	       	      x = fromOM x'
                                      z :: Int
                                      z = (parMapFoldSCSCP (Left ("scscp_transient_1",f)) (Left ("scscp_transient_1",g)) x xs)
                       	       	  in Right (toOM z)

-- as above, but on a non-empty list without a neutral element
parMapFold1OM :: [OMObj] -> Either String OMObj
parMapFold1OM [f', g', xs'] =     let f :: String
                       	       	      f = fromOM f'
                       	       	      g :: String
                       	       	      g = fromOM g'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                                      z :: Int
                                      z = (parMapFold1SCSCP (Left ("scscp_transient_1",f)) (Left ("scscp_transient_1",g)) xs)
                       	       	  in Right (toOM z)

-- standard zipWith skeleton
parZipWithOM :: [OMObj] -> Either String OMObj
parZipWithOM [f', xs', ys'] =     let f :: String
                       	       	      f = fromOM f'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                       	       	      ys :: [Int]
                       	       	      ys = fromOM ys'
                                      z :: [Int]
                                      z = (parZipWithSCSCP (Left ("scscp_transient_1",f)) xs ys)
                       	       	  in Right (toOM z)

-- not working at the moment; just tinkering here!
parMap'OM :: [OMObj] -> Either String OMObj
parMap'OM (f':xs') =   let f :: String
                           f = fromOM f' -- TODO: lookup of fct; needs top level lookup!!
                           -- xs :: [Int]
                           -- xs = fromOM xs'
                           -- zs :: [Int]
                           zs = parmapfarm noPe ( \ x -> fromRight (resultant2OM [x]) ) xs'
                           -- zs = (parMapSCSCP (Left ("scscp_transient_1",f)) xs)
                           z = (head zs)
                       in z `seq` Right z  -- Right (toOM zs)


fromRight (Right x) = x
fromRight (Left _) = error "fromRight: found Left"

-- sad, sequential versions of the above skeletons (mainly for testing)

-- apply a service, given as name without CD, to all elements of a list in parallel
mapOM :: [OMObj] -> Either String OMObj
mapOM [f', xs'] =      let f :: String
                           f = fromOM f'
                           xs :: [Int]
                           xs = fromOM xs'
                           zs :: [Int]
                           zs = (mapSCSCP (Left ("scscp_transient_1",f)) xs)
                       in Right (toOM zs)

map'OM :: [OMObj] -> Either String OMObj
map'OM _ = error "map'OM: not implemented"


-- added by CMB 17/07/2010
-- standard call to the Orbit workpool
parOrbitOM :: [OMObj] -> Either String OMObj
parOrbitOM [f, xs, set] = 
                     let f' :: [String]
                         f' = fromOM f
                         xs' :: [Arith]
                         xs' = fromOM xs
                         set' :: Arith
                         set' = fromOM set
                         z :: [Arith]
 -- runOrbit [Left ("scscp_transient_1", "WS_Fib1"), Left ("scscp_transient_1", "WS_Fib2"), Left ("scscp_transient_1", "WS_Fib3")] [[1]] setSize
                         --z = (parOrbitSCSCP [Left ("scscp_transient_1", "WS_Fib1"), Left ("scscp_transient_1", "WS_Fib2"), Left ("scscp_transient_1", "WS_Fib3")] [xs'] set')
                         z = (parOrbitSCSCP (map (\x -> (Left ("scscp_transient_1", x))) f') xs' set')
                     in Right (toOM z)

parHMIOM :: [OMObj] -> Either String OMObj
parHMIOM [h,f,g,ps,m] =
                   let h' :: String
                       h' = fromOM h
                       f' :: String
                       f' = fromOM f
                       g' :: String
                       g' = fromOM g
                       ps' :: [Integer]
                       ps' = fromOM ps
                       m' :: Arith
                       m' = fromOM m
                       -- v' :: Arith
                       -- v' = fromOM v
                       z :: Arith
                       z = parMHISCSCP (Left ("scscp_transient_1", h'))
                                       (Left ("scscp_transient_1", f'))
                                       (Left ("scscp_transient_1", g'))

                                       ps' m'
                   in Right (toOM z)





-- standard fold-of-map skeleton, i.e. Haskell: foldl g x (map f xs)
mapFoldOM :: [OMObj] -> Either String OMObj
mapFoldOM [f', g', x', xs'] =     let f :: String
                       	       	      f = fromOM f'
                       	       	      g :: String
                       	       	      g = fromOM g'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                       	       	      x :: Int
                       	       	      x = fromOM x'
                                      z :: Int
                                      z = (mapFoldSCSCP (Left ("scscp_transient_1",f)) (Left ("scscp_transient_1",g)) x xs)
                       	       	  in Right (toOM z)

-- as above, but on a non-empty list without a neutral element
mapFold1OM :: [OMObj] -> Either String OMObj
mapFold1OM [f', g', xs'] =        let f :: String
                       	       	      f = fromOM f'
                       	       	      g :: String
                       	       	      g = fromOM g'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                                      z :: Int
                                      z = (mapFold1SCSCP (Left ("scscp_transient_1",f)) (Left ("scscp_transient_1",g)) xs)
                       	       	  in Right (toOM z)

-- standard zipWith skeleton
zipWithOM :: [OMObj] -> Either String OMObj
zipWithOM [f', xs', ys'] =        let f :: String
                       	       	      f = fromOM f'
                       	       	      xs :: [Int]
                       	       	      xs = fromOM xs'
                       	       	      ys :: [Int]
                       	       	      ys = fromOM ys'
                                      z :: [Int]
                                      z = (zipWithSCSCP (Left ("scscp_transient_1",f)) xs ys)
                       	       	  in Right (toOM z)

-----------------------------------------------------------------------------

#endif

-- Haskell-side sumEuler code now only in Haskell-server, as it should be

#ifdef __PARALLEL_HASKELL__ 
karatsubaStrSCSCP :: [OMObj] -> IO OMObj
karatsubaStrSCSCP [p1OM,p2OM,p3OM,p4OM] = do  -- FIXED: 4 polys at the moment!!
   -- let fName = scscpKaratsubaGAP
   -- let args  = [p1OM, p2OM]
   let p12OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_KaratsubaStr_x xs))) [p1OM, p2OM]
   let p34OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_KaratsubaStr_x xs))) [p3OM, p4OM]
   let p1234OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_KaratsubaStr_x xs))) [p12OM, p34OM]
   return p1234OM

karatsubaSCSCP :: [OMObj] -> IO OMObj
karatsubaSCSCP [p1OM,p2OM,p3OM,p4OM] = do  -- FIXED: 4 polys at the moment!!
   -- let fName = scscpKaratsubaGAP
   -- let args  = [p1OM, p2OM]
   let p12OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_Karatsuba xs))) [p1OM, p2OM]
   let p34OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_Karatsuba xs))) [p3OM, p4OM]
   let p1234OM = deLift $ createProcess (process (\ xs ->  (callSCSCP scscp_WS_Karatsuba xs))) [p12OM, p34OM]
   return p1234OM
#endif

-------------------------------------------------------

