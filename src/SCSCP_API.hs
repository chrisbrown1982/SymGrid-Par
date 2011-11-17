{-# OPTIONS -cpp -XScopedTypeVariables -XPatternSignatures #-}
{-
Time-stamp: <Tue Apr 13 2010 22:44:14 Stardate: Stardate: [-28]2914.52 hwloidl>

Sequential SCSCP interface 
-}


module SCSCP_API (
     SCSCPServer(..)
   , defaultServer, mockingServer, server
   , startServer, stopServer
   , initServer, releaseServer
   , exchangeSCSCP
   , callSCSCP
   , module HS_SCSCP -- required data types 

     -- below: mostly internal. for now, export all for ghci debugging
   , module HS2SCSCP
   , checkServerState
   , readBlock, readUntilPI, splitAtPI, 
   ) where

-- date, release etc info
import Date

-- our SCSCP framework. 
import HS2SCSCP
import HS_SCSCP
-- maybe redundant, reading in XML:
-- import SCSCP_DTD

-- library stuff:
import System.IO         -- handles and I/O
import System.IO.Unsafe(unsafePerformIO)

import Control.Monad      -- evaluation and execution control throughout...
import Control.Parallel.Strategies -- MUST be parallel-1.xx
import Control.Concurrent -- MVar and threadDelay
#if __GLASGOW_HASKELL__ > 610
import Control.OldException as C -- catching severe errors 
                              -- => shutting down the server
#else
import Control.Exception as C -- catching severe errors 
                              -- => shutting down the server
#endif

import Network -- for communication with the SCSCP server

import Foreign.StablePtr -- given a pointer, prevents its bound object
                         -- to be affected by the garbage collector 
                         -- useful to simulate a "imperative persitent 
                         -- state" in the interface. See subsquent.
import Monitor           -- An "external state" using the FFI.
import Foreign.Ptr(nullPtr) -- integrity checks only

#ifdef __PARALLEL_HASKELL__
#warning System.Process used here. Status: experimental
import System.Process  -- forking an own SCSCP server process
#endif

import System.Environment(getEnv)

#ifdef __PARALLEL_HASKELL__
import Control.Parallel.Eden(Trans) -- Enabling remote processes in a functional style.
-- import Edi         -- lower level control, spawning remote IO(),
                   -- synchronising via messages
#endif

#ifdef DEBUG
import Debug.Trace
#else
-- this eliminates the trace calls when compiling with -O or -O2
{-# RULES "notrace" forall a b. trace a b = b #-}
trace _ x = x
#endif
traceM :: String -> IO ()
traceM msg = trace msg (return ()) 


-- we follow the document "SCSCP-1.2.pdf", thus:
#define CLIENTVERSION "1.2"

--------------------------------------------------
-- SCSCP servers:

data SCSCPServer = SCSCPServer 
                   { scscpHost :: String
                   , scscpPort :: PortID 
                   -- system exec. path, commands to start scscp
                   , startData :: Maybe (String,[String])
                   }

-- our own dummyServ.hs
dummyServ = server "localhost" (Just 12321)

-- port number taken from SCSCP-1.2 spec., assuming localhost
defaultServer = SCSCPServer "localhost" (PortNumber 26133) Nothing

-- testing in st.andrews
mockingServer = server 
                  "chrystal.mcs.st-andrews.ac.uk" 
                  Nothing --std.port

issel = server "issel.math.tu-berlin.de" Nothing 

server :: String -> Maybe PortNumber -> SCSCPServer
server name Nothing     = defaultServer{scscpHost=name}
server name (Just port) = defaultServer{scscpHost=name,
                                        scscpPort=PortNumber port}

-- we might want to start our own instance from Haskell, and will
-- store the handle in just the same way as if it was an already
-- started server which we only use.
startServer :: IO SCSCPServer -- start own server (fork process)
stopServer  :: IO ()          -- stop own server (kill process)
startServer = error "start server: not implemented"
stopServer  = error "stop server: not implemented"

-----------------------------------------------------

-- reads the global MVar pointer, returns whether ptr!=nullPtr.
-- True means, server is initialised and listening.
checkServerState :: IO Bool
checkServerState = do checkStP <- creadMVarPointer
                      let checkP = castStablePtrToPtr checkStP
                      return (checkP /= nullPtr)

-- pass error to someone inside the IO monad.
passError :: String -> IO a
passError = return . error

-- on severe errors, release server, complain, and pass error 
severeError :: String -> IO a
severeError msg = do 
  traceM "severeError"
  check <- checkServerState
  when check (trace "shutting down in severeError" $
              C.catch releaseServer (\(e::Exception)  -> trace (show e) $ return ()))
  putStrLn "Severe error, shutting down the server connection"
  passError msg


-- start server communication. Connects to a given server, exchanges
-- init. messages, then stores a server handle in the global state.
initServer  :: SCSCPServer -> IO ()
initServer srv = withSocketsDo $ 
                 do check <- checkServerState 
                    if check 
                       then do traceM "busy... aborting"
                               passError "init: HS interface already busy"
                       else do
                    syncVar <- newEmptyMVar
                    traceM "connecting..."
                    hdl <- connectTo (scscpHost srv) (scscpPort srv)
-- THIS LEADS TO MISFUNCTIONALITY IN THE KANT SERVER
--                    hSetBuffering hdl NoBuffering
                    -- exchange init messages
                    traceM "exchanging init. messages..."
                    -- we expect an init message at first, nothing before.
                    (_,serverInit) <- splitAtPI hdl
                    case serverInit of 
                      (Init n v iD _) -> traceM $ "server " ++ n ++ '(':v
                                                   ++ "),session " ++ iD
                      other            -> do traceM "unexpected message"
                                             fail "unexpected server init"
                    let vs = piInitSCSCPs serverInit -- versions
                        versionCheck = CLIENTVERSION `elem` vs
                    traceM ("Version check. Server supports: " ++ unwords vs
                            ++ '\n':CLIENTVERSION 
                            ++ (if versionCheck then " " else " NOT ")
                            ++ "supported.")
                           
                    -- answer with our version (anyway, let server abort)
                    -- woraround for libkant 4.0
                    if versionCheck 
                      then hPutStrLn hdl (writePI (Version CLIENTVERSION))
                      else let bogusV = Version (last vs)
                           in trace ("answering version: " ++ show bogusV)
                              (hPutStrLn hdl (writePI (Version (last vs))))
                    -- expect server "ready" answer, otherwise fail
                    hFlush hdl
                    (_,answer) <- splitAtPI hdl
                    case answer of 
                      Quit Nothing  -> do traceM ("server quit." )
                                          fail "server quit unexpectedly."
                      Quit (Just m) -> do traceM ("server quit with " ++ m )
                                          fail ("server quit: " ++ m)
                      Version str   -> traceM "server sez ready"
                      other         -> do traceM ("unexpected server message"
                                                  ++ show other)
                                          fail "unexpected server answer."
                    hPutStrLn hdl "\n"
                    hFlush hdl
                    -- write out MVar with handle
                    putMVar syncVar hdl
                    ptr <- newStablePtr syncVar
                    cwriteMVarPointer ptr
                    traceM "server set up to communicate"

-- terminate communication with the server which has been started by
-- initServer.
releaseServer :: IO ()
releaseServer = do check <- checkServerState
                   if not check 
                        then traceM "no server, nothing to stop..."
                        else do
                   syncP   <- creadMVarPointer
                   syncVar <- deRefStablePtr syncP
                   -- invalidating the MVar pointer
                   cdeleteMVarPointer
                   -- Now, wait here for the handle in the MVar.
                   hdl <- takeMVar syncVar
                   C.catch (do 
                     -- terminating the server session by a message
                     hPutStrLn hdl (writePI (Quit Nothing))
                     -- Possible race condition: in the meantime,
                     -- others may still be using the server... queued
                     -- waiting for the handle as well. We pass them
                     -- an error closure instead of the correct handle
                     -- (should not happen in a correct program IMHO).
                           ) (\(e::Exception) -> trace ("release failed" ++ show e) $ 
                                    return ())
                   putMVar syncVar (error "server stopped prematurely")

{-

connection initiation as described in SCSCP-1.2.pdf:

server sends: 
  <?scscp service_name="A" service_version="B" 
          service_id="C" scscp_versions="D" ?>

where A is the name of the service (simply a String) 

      B is the version, seems to be numbers and suffixes, like
           4.5.1beta

      C is a unique identifier for the session (???generated by the
           server, OK... )

      D is a space-separated list of strings (digits,letters,dot)
           which describe scscp versions 

then client sends:
  <?scscp version="CLIENTVERSION" ?>

where CLIENTVERSION *should* be one of the supported versions
      announced by the server in list D

then server sends:
  <?scscp version="SERVERVERSION" ?>

where SERVERVERSION confirms the CLIENTVERSION (same string)
    If the CLIENTVERSION is not supported, the server may 
    terminate the session by sending 
  <?scscp quit reason="blabla" ?> instead

Then server and client communicate in sequences of OMOBJ messages,
separated by 
  <?scscp start ?>
   ...OMOBJ...
  <?scscp end   ?>

For canceling a started message,
  <?scscp cancel ?> 
   may terminate the transaction at any time in-between <?scscp start ?>
   and receiving the total of an OMOBJ.

The SCSCP interrupt is signalled by sending SIGUSR2 to the
server. (???How that, if it is a remote service???)

-}

--------------------------------------------------------
-- Processing instruction stuff: top-level stuff here for 
-- convenience, specification-dependent parts in HS_SCSCP 
-- (parsePI and according data type)

-- read in characters from handle, until "?>" appears.
-- return all characters read, including the final "?>"
readUntilPI :: Handle -> IO String
readUntilPI h = trace ".. starting to readUntilPI" (accum "")
    where accum :: String -> IO String
          accum acc = do 
            c <- hGetChar h
            if c /= '?' then (accum (c:acc))
                        else -- trace "q.mark!" 
                             (qMark (c:acc))
          qMark :: String -> IO String
          qMark acc = do
            c <- hGetChar h
            case c of 
              '?' -> qMark (c:acc)
              '>' -> trace ".. finished reading until PI" 
                     (return (reverse (c:acc)))
              other -> accum (c:acc)

-- extract one processing instruction of form "<?scscp ... ?> from a
-- stream received from a handle, deliver the prefix string as well:
splitAtPI :: Handle -> IO (String,SCSCP_PI)
splitAtPI h = do traceM "splitAtPI"
                 input <- readUntilPI h
                 traceM $ "Input (splitAPI): "++(show input)
                 let splitLoc = boyerMoore "<?scscp" input
		 case splitLoc of 
		   Nothing -> -- not an scscp PI. try again.
		              trace "not for me... retry" 
			      (splitAtPI h)
                   Just pos -> do
                     let (pre,pi) = splitAt pos input
                         thePI    = parsePI pi
                     return (pre,thePI)

------------------------------------------------------
-- OMOBJ message exchange between client and server:

-- read a "block" of SCSCP, starting by PI Start, ended by PI Cancel
-- or PI End. Deliver message in-between (or nothing if canceled).
readBlock :: Handle -> IO (Maybe String)
readBlock h = do traceM "readBlock"
                 (pre,startpi) <- splitAtPI h
		 -- kant server workaround
		 case startpi of 
		    Other msg -> do traceM ("received this:\n" 
		    	      	    	    ++ pre 
                                            ++ "and scscp PI " ++ msg)
		    	      	    readBlock h
                    Start ->  do 
                      (block,stoppi) <- splitAtPI h
                      traceM ("received " ++ show block)
                      case stoppi of
                        Cancel -> trace "canceled" $
                                  return Nothing
                        End    -> return (Just block)
                        Quit reason -> trace ("unexpected PI: Quit with reason " ++ show reason)
                                       return Nothing
                        other  -> trace ("unexpected PI: " ++ show other)
                                  return Nothing
                    other -> do traceM ("unexpected PI (start):\n"
		    	     	        ++ writePI other) 
				return Nothing

-- Kant server namespace hack: replace all tag prefixes "<OM:" by "<"
removeNamespaceOM :: String -> String
removeNamespaceOM [] = []
removeNamespaceOM [x] = [x]
removeNamespaceOM [x,y] = [x,y]
removeNamespaceOM [x,y,z] = [x,y,z]
removeNamespaceOM [x,y,z,a] = [x,y,z,a]
removeNamespaceOM ('<':'O':'M':':':rest) = '<':removeNamespaceOM rest
removeNamespaceOM ('<':'/':'O':'M':':':rest) = '<':'/':removeNamespaceOM rest
removeNamespaceOM (a:b:c:d:e:other) = a:removeNamespaceOM (b:c:d:e:other)

-- uses the (hopefully) stored server handle to 
exchangeSCSCP :: SCSCPMsg -> IO SCSCPMsg
exchangeSCSCP (PResult _ _ _ _) 
    = passError "PResult from client to server?"
exchangeSCSCP (PTerminated _ _ _ _) 
    = passError "PTerminate from client to server?"
exchangeSCSCP pcall 
 = do traceM ".. scscp message exchange... "
      check <- checkServerState
      if not check then error "no server initialised" 
                   else do
      ptr <- creadMVarPointer
      syncVar <- deRefStablePtr ptr
      hdl <- takeMVar syncVar
      traceM $ (".. using handle "++(show hdl)++" to send "++(show pcall))
      open <- (hIsOpen hdl)
      ready <- (hReady hdl)
      traceM $ (".. handle Open? "++(show open)++"; handle ready? "++(show ready))
      C.catch 
       (do -- synchronised:
         hPutStrLn hdl (writePI Start)
	 hFlush hdl
         hPutStrLn hdl (writeSCSCPMsg pcall)
	 hFlush hdl
         hPutStrLn hdl (writePI End)
	 hFlush hdl
         output <- readBlock hdl
         traceM $ (".. raw reply to send of "++(show (callID pcall))++" is "++(show output))
         putMVar syncVar hdl
         case output of 
           Just s  -> let msg = readSCSCPMsg (removeNamespaceOM s)
                      in msg `seq` -- to catch error here
                         return msg 
           Nothing -> return (PTerminated "no-ID" 
                               (SystemError "no output received") 
                               Nothing Nothing)
       ) (\(e::Exception) -> putMVar syncVar hdl >> 
                severeError ("msg. exchange failed with " ++ show e))

-- TODO: write an asynchronous method.
-- * creates an empty MVar(OMObj) for the result
--   (mechanism might as well use MVar (SCSCPMsg) )
-- * takes handle, writes out PCall
-- * frees server handle
-- * (on success only:) 
--    - registers call_ID and MVar in some global store
--    - creates a closure res = [unsafePerformIO] (readMVar resultVar)
--    - returns this closure
-- a (global) answer-reading thread reacts on all answer as follows:
-- * reads answer, extracts call_ID
-- * looks up result MVar in global store
-- * on success: stores the reply

-- bogus-functional interface for PCalls. Name and Arg.list
callSCSCP :: CAName -> [OMObj] -> OMObj
callSCSCP fName args = unsafePerformIO $ do
      traceM ("callSCSCP function.....d  " ++ show fName ++ "args: " ++ (show args))
      call_ID <- newID
      let pcall = PCall call_ID fName args defaultProcOptions
      answer <- exchangeSCSCP pcall
      if (callID answer /= call_ID) 
        then do traceM ("wrong callID " ++ callID answer 
                        ++ ", expected " ++ call_ID)
                severeError ("received wrong call ID!!! "++"expected: "++(show call_ID)++" received: "++(show (callID answer))++"\nin the following message\n"++(show answer))
                -- severeError ("received wrong call ID!!!")
        else case answer of 
               PTerminated _ err t m 
                   -> return (error (errTypeName err ++ "." 
                                     ++ errText err))
               PResult res _ t m     -> return res

call2 :: (OMData a, OMData b, OMData c) =>
         CAName -> a -> b -> c
--       ------ 
--     still ugly :( 

-- use Template Haskell to generate these guys
call2 name x y = answer
   where theTrace = "\nsent " ++ (show res) 
         res = toOM y
         answer = (fromOM (callSCSCP name [toOM x,res]))

