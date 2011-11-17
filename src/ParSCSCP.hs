{-# OPTIONS -cpp #-}
{-# OPTIONS -XParallelListComp -XPatternSignatures -XScopedTypeVariables #-}
{-
Time-stamp: <Sat Apr 24 2010 00:47:33 Stardate: Stardate: [-28]2964.95 hwloidl>

Parallel version of the SCSCP_API. This is the main version used in SymGrid-Par.
-}

module ParSCSCP 
{-
   ( SCSCPServer(..)
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
   ) 
-}
     where
#ifndef __PARALLEL_HASKELL__
#warning Building ParSCSCP sequentially???
#endif

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
import Control.Parallel.Strategies  -- MUST be parallel-1.xx
import Control.Concurrent -- MVar and threadDelay
#if __GLASGOW_HASKELL__ > 610
import Control.OldException as C -- catching severe errors 
                              -- => shutting down the server
#else
import Control.Exception as C -- catching severe errors 
                              -- => shutting down the server
#endif
import List
import Network -- for communication with the SCSCP server

import Foreign.StablePtr -- given a pointer, prevents its bound object
                         -- to be affected by the garbage collector 
                         -- useful to simulate a "imperative persitent 
                         -- state" in the interface. See subsquent.
import Monitor           -- An "external state" using the FFI.
import Foreign.Ptr(nullPtr) -- integrity checks only

import System.Process  -- forking an own SCSCP server process

import System.Environment(getEnv)

#if __GLASGOW_HASKELL__ > 610
import GHC.IO.Handle -- for hDuplicate, concurrent reply thread
#else
import GHC.Handle -- for hDuplicate, concurrent reply thread
#endif

#if defined(__PARALLEL_HASKELL__) || defined(USE_PAR_SCSCP)
import Control.Parallel.Eden(NFData(..), Trans) 
import Control.Parallel.Eden.EdenConcHs hiding (noPe)
import Control.Parallel.Eden.ParPrim hiding (noPe)
import Control.DeepSeq
import Edi
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
-- one of these will be stored on each running instance 
-- need to provide information about 
data SCSCPServer  
    = -- async. server. When creating such a server, a reply thread
      -- should run and fill the replies in. Anybody writing to the
      -- handle should put an MVar for the result in the replyQ.
      AsyncServer { scscpHost :: String
                  , scscpPort :: PortID 
                  , scscpProc :: Maybe ProcessHandle
                  , scscpHandle :: Handle
                  , replyQ :: [ (CallID, MVar SCSCPMsg)] 
                  , replyT :: ThreadId
                  -- for reply thread, MVar takes result will usually
                  -- be a queue, but we provide it as a list for
                  -- safety and to enable out-of-order server
                  -- replies. Using List.lookup, should be efficient
                  -- when queue-like processing most of the times..
                  }
    -- sync. server, keep connection blocked until result arrives
    | SyncServer  { scscpHost :: String
                  , scscpPort :: PortID 
                  , scscpProc :: Maybe ProcessHandle
                  , scscpHandle :: Handle }
    -- not started / connected yet
    | SCSCPServer { scscpHost :: String
                  , scscpPort :: PortID 
                  , startCmd  :: Maybe String
                  }
--       deriving (Show) -- does not work: ProcessHandles, MVars, PortID

instance Show SCSCPServer where 
#ifdef DEBUG
    -- verbose version; useful for debugging
    show srv = let PortNumber port = scscpPort srv
               in ("SCSCPServer " ++ scscpHost srv
                   ++ "(port " ++ show port ++ ", "
                   ++ if not (started srv) 
                      then "not started)" 
                      else if isAsync srv 
                             then "async, " 
                             else "sync, "
                           ++ case scscpProc srv of
                                Nothing -> "not local)"
                                Just p  -> "local process)"
                  )
#else 
    show srv = let PortNumber port = scscpPort srv
               in "SCSCPServer " ++ scscpHost srv ++" port " ++ show port
#endif


#ifdef __PARALLEL_HASKELL__
instance NFData SCSCPServer where 
    rnf srv = rnf (scscpHost srv, scscpPort srv)
-- need this as well, but we keep it simple
instance NFData PortID where
    rnf (PortNumber n) = rnf ((fromIntegral n)::Int)
    rnf (Service s   ) = rnf s
    rnf (UnixSocket s) = rnf s
#endif

-- predicates and selectors based on pattern match go here:  
started :: SCSCPServer -> Bool
started (SCSCPServer _ _ _) = False
started _ = True 

isAsync :: SCSCPServer -> Bool
isAsync (AsyncServer _ _ _ _ _ _) = True
isAsync _ = False



------------------------------------
-- fixed servers:

defaultServer = SCSCPServer "localhost" (PortNumber 26133) Nothing
-- port number taken from SCSCP-1.2 spec., assuming localhost

server :: String -> PortNumber -> SCSCPServer
server name port = defaultServer{scscpHost=name,
                                 scscpPort=PortNumber port}

-- our own dummyServ.hs, with default port 12321
dummyServ = SCSCPServer "localhost" 
                        (PortNumber 12321) 
                        (Just "$(HOME)/dummyServ")
-- testing in st.andrews
mockingServer = server
                  "chrystal.mcs.st-andrews.ac.uk" 
                  26133 --std.port

-- berlin... 
issel port = server "issel.math.tu-berlin.de" port 

--------------------------------------------
-- code below should not depend on no. of fields in server d.s.
-- (using only named fields and helpers above)

-----------------------------------------------------

-- reads the global MVar pointer, returns whether ptr!=nullPtr.
-- True means, server is initialised and listening.
checkServerState :: IO Bool
checkServerState = do checkStP <- creadMVarPointer
                      let checkP = castStablePtrToPtr checkStP
                      if (checkP == nullPtr) 
                         then return False -- no server at all 
                         else do syncVar <- deRefStablePtr checkStP
                                 srv <- readMVar syncVar
                                 return (started srv)

-- pass error to someone inside the IO monad. Used whenever we dont
-- want to fail inside here, but make the caller fail (due to bad
-- function arguments or such)
passError :: String -> IO a
passError = return . error

-- on severe errors, release server, complain, and pass error 
severeError :: String -> IO a
severeError msg = do 
  traceM "severeError"
  check <- checkServerState
  when check (trace "shutting down in severeError" $
              C.catch releaseServer (\e
                                        -> trace (show e) $ 
                                           return ()))
  putStrLn "Severe error, shutting down the server connection"
  passError msg

{-


Refactoring note: This should eventually go into HS_SCSCP, since it
relates to specification stuff. Equally, the init method should be
broken up into spec.specific and independent stuff, if possible.

Problem: we use all types from HS_SCSCP and the methods separated into
HS2SCSCP.  Need to reorganise module hierarchy. Worth a thought in the
long run, though.

---------------------------------------------------------

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

-- start server communication. Connects to a given server, exchanges
-- init. messages, then stores a server handle in the global state.
-- parameter "async" creates a server with asynchronous communication
-- (reply thread).
initServer  :: Bool -> SCSCPServer -> IO (MVar SCSCPServer)
initServer async srv | started srv 
    = fail "init: server assumed started"
initServer async srv = withSocketsDo $           -- generic socket init
                 do check <- checkServerState	 -- get local info about the server
                    when check $
                            do traceM "busy... aborting"
                               fail "init: HS interface already busy"

                    syncVar <- newEmptyMVar      -- main mutex
                    traceM (".. connecting to "++(show srv))
                    hdl <- connectTo (scscpHost srv) (scscpPort srv) -- listen to specifc host:port
                    str <- hShow hdl
                    traceM $ ".. generated this handle to srv "++(show srv)++": "++str

-- THIS LEADS TO MISFUNCTIONALITY IN THE KANT SERVER
                    hSetBuffering hdl NoBuffering
                    -- exchange init messages
                    traceM ".. exchanging init. messages..."
                    -- we expect an init message at first, nothing before.
                    (prefix,serverInit) <- splitAtPI hdl
                    traceM (".. prefix="++(show prefix)++"; serverInit="++(show serverInit))
                    case serverInit of 
                      (Init n v iD _) -> traceM $ ">> server " ++ n ++ '(':v
                                                   ++ "),session " ++ iD
                      other            -> trace (">> unexpected message"
                                                 ++ show prefix 
                                                 ++ show serverInit) $
                                          fail "unexpected server init."
                    let vs = piInitSCSCPs serverInit -- versions
                        versionCheck = CLIENTVERSION `elem` vs
                    traceM ("Version check. Server supports: " ++ unwords vs
                            ++ '\n':CLIENTVERSION 
                            ++ (if versionCheck then " " else " NOT ")
                            ++ "supported.")
                           
                    -- answer with our version (anyway, let server abort)
                    -- workaround for libkant 4.0
                    if versionCheck 
                      then hPutStrLn hdl (writePI (Version CLIENTVERSION))
                      else let bogusV = Version (last vs)
                           in trace ("<< answering version: " ++ show bogusV)
                              (hPutStrLn hdl (writePI bogusV))
                    -- expect server "ready" answer, otherwise fail
                    hFlush hdl
                    (_,answer) <- splitAtPI hdl
                    case answer of 
                      Quit Nothing  -> do traceM (">> server quit." )
                                          fail "server quit unexpectedly."
                      Quit (Just m) -> do traceM (">> server quit with " ++ m )
                                          fail ("server quit: " ++ m)
                      Version str   -> traceM ">> server says ready"
                      other         -> do traceM (">> unexpected server message"
                                                  ++ show other)
                                          fail "unexpected server answer."
                    hPutStrLn hdl "\n"
                    hFlush hdl
                    {-
                       synchronisation on top level is via	syncVar :: MVar SCSCPserver
                       main data structure is			replyQ  :: [ (CallID, MVar SCSCPMsg)] 
                       replyThread listens to a socket and puts the result into
                       the MVar of the callId attached to the message 
                    -}
                    if async -- start replyThread, then write server
                      then let replyThread h = do 
                                 -- read an answer from CAS
                                 b <- readBlock h
                                 case b of 
                                   Nothing  -> replyThread h
                                   Just msgStr -> do 
                                    let msg = readSCSCPMsg msgStr
                                        cID = callID msg
                                    -- reserve server from MVar
                                    srv <- takeMVar syncVar
                                    -- read and modify queue
                                    let q  = replyQ srv
                                        (rV,newQ) = myLookup q cID        
                                    traceM $ ".. received this reply (async): "++(show b)
                                    case rV of 
                                     Nothing  -> putMVar syncVar srv
                                     Just var -> do putMVar var msg
                                                    putMVar syncVar 
                                                      srv{replyQ = newQ}
                                    replyThread h -- iterate this
                           in do 
                                 str1 <- hShow hdl
                                 traceM $ ".. in replyThread (async): current handle is "++str1
                                 dupH <- hDuplicate hdl 
                                 str2 <- hShow dupH
                                 traceM $ ".. in replyThread (async): duplicated handle is "++str2
                                 -- create 2nd handle (duplicate)
                                 -- TODO this doesn't work at the moment
                                 tid <- forkIO (replyThread dupH)
                                 putMVar syncVar (AsyncServer 
                                                  (scscpHost srv) 
                                                  (scscpPort srv) 
                                                  Nothing hdl [] tid)
                             -- TODO depends on Server data structure
                      else -- write server
                          putMVar syncVar (SyncServer 
                                           (scscpHost srv) 
                                           (scscpPort srv) 
                                           Nothing hdl)
                             -- TODO depends on Server data structure
                    -- store server externally (global)
                    ptr <- newStablePtr syncVar
                    cwriteMVarPointer ptr
                    traceM ".. server set up to communicate"
                    return syncVar

myLookup :: [(CallID,MVar SCSCPMsg)] -> CallID 
         -> (Maybe (MVar SCSCPMsg),[(CallID,MVar SCSCPMsg)])
myLookup q i = myL [] q
   where myL acc [] = (Nothing,reverse acc)
         myL acc ((j,var):rest) | j == i = (Just var,reverse acc ++ rest)
                                | otherwise = myL ((j,var):acc) rest


-- a (global) answer-reading thread reacts on all answer as follows:
-- * reads answer, extracts call_ID
-- * looks up result MVar in global store
-- * on success: stores the reply


#ifdef __PARALLEL_HASKELL__
-- Eden: connect to a few servers (ensure placement on different PEs,
-- refuse to connect to more than PEs )
-- We use Edi functionality here, IO-monadic anyway

initServers :: [SCSCPServer] -> IO Bool
initServers srvs = do
       traceM ".. initServers"
       np <- noPe
       let (sList,more) = splitAt np srvs
           l = rnf sList `seq` -- also ensuring we have the server
               length sList    -- data at hand
       when ((not . null) more) $ do
            traceM (".. initServers: too many servers, only using " ++ show l ++" namely\n "++(List.concat (List.intersperse "\n " (map show sList))))
       -- We should always connect to one server for every PE.  Nodes without
       -- adjoint server cannot be used, but this is global state and cannot
       -- be determined externally..
       when (l /= np) (error "too few servers given.") 
       -- the rest of the code allows to start less servers, though.
       traceM (".. starting " ++ show l ++ " SCSCP servers, namely\n "++(List.concat (List.intersperse "\n " (map show sList))))
       (replyCs, replies) <- createCs l
       traceM (".. created "++(show (length replyCs))++" reply channels...")
       let procs = [spawnProcessAt p (initOne rc srv) 
                    | p <- [np,np-1..1]
                    | rc <- replyCs
                    | srv <- sList ]
       traceM (".. created "++(show (length procs))++" processes ...")
       sequence_ procs
       traceM (".. initServers: got these replies: " ++ (List.concat (List.intersperse "," (map show replies))))
       return (and replies)

initOne :: ChanName' Bool -> SCSCPServer -> IO ()
initOne ch srv = C.catch 
                       -- we can use startServer here as well later.
                       -- For now, just connect to one.
		      (do -- putStrLn $ "initOne: "++(show srv)
                          initServer False srv -- AsyncServer doznwok for now :(
		          -- initServer True srv
		          idInit  -- initialise the ID supply
		          sendNF ch True) 
                      (\e
                          -> trace (show e) $ 
                             sendNF ch False)

#endif


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
                   server <- takeMVar syncVar
                   let hdl = scscpHandle server 
                   C.catch (do 
                     -- terminating the server session by a message
                     hPutStrLn hdl (writePI (Quit Nothing))
                     -- Possible race condition: in the meantime,
                     -- others may still be using the server... queued
                     -- waiting for the handle as well. We pass them
                     -- an error closure instead of the correct handle
                     -- (should not happen in a correct program IMHO).
                           ) (\e
                                 -> trace ("release failed" ++ show e) $ 
                                    return ())
                   putMVar syncVar (error "server stopped prematurely")

#ifdef __PARALLEL_HASKELL__
-- for releaseServer, we only provide a method to release on every
-- node (we cannot give a server list, they are anyway global data,
-- and releaseServer will silently abort when no server is started.
releaseServers :: IO ()
releaseServers = do np <- noPe
                    traceM ".. releasing servers on all nodes"
                    sequence_ [ spawnProcessAt i releaseServer 
                                | i <- [2..np]]
                    releaseServer

#endif

-- we might want to start our own instance from Haskell, and will
-- store the handle in just the same way as if it was an already
-- started server which we only use.
startServer :: IO SCSCPServer -- start own server (fork process)
stopServer  :: IO ()          -- stop own server (kill process)

startServer = error "start server: not implemented"
              -- code here will branch into ghc-6.10.x/ghc-6.8.3(eden)
              -- and use System.Process API 

              -- ATTENTION, BUGS LURKING: up to ghc-6.8.3, SIGPIPE
              -- kills the running program. Changed to an exception in
              -- ghc-6.10.1.

              -- External requirement: must start a server by issueing
              -- a simple shell command, and load it with the desired
              -- functions. Server name and Port should be
              -- configurable as command parameters, as well as the
              -- initialisation (loading functions).

              -- after starting the server (an OS process), we can
              -- connect to it using initServer(ASync/Sync), with
              -- localhost and the special port we have chosen.

stopServer  = do chk <- checkServerState
                 if (not chk) 
                   then trace "no started server, nothing to stop" $ 
                        return ()
                   else do 
                     syncP   <- creadMVarPointer
                     syncVar <- deRefStablePtr syncP
                     -- invalidating the MVar pointer
                     cdeleteMVarPointer
                     -- Now, wait here for the MVar.
                     server <- takeMVar syncVar
                     -- this is our dedicated server anyway.
                     let proc = scscpProc server -- process to kill 
                         tid  = replyT server    -- reply thread
                     -- kill the reply thread before the server.
                     -- Otherwise SIGPIPE terminates all of us...
                     when (isAsync server) 
                          (killThread tid) 
                     case proc of 
                       Nothing  -> trace "no pid, cannot kill this server" $
                        -- now we could send <?scscp quit ?>...
                                  return ()
                       Just pid -> 
                         -- killing server process
                         C.catch (terminateProcess pid) 
                           (\e
                               -> trace ("failed to kill process" 
                                         ++ show e) $ 
                                  return ())
                     -- to point out potential race conditions...
                     putMVar syncVar (error "server stopped prematurely")

-----------------------------------------------------
-- functions for interaction with the running server:


--------------------------------------------------------
-- Processing instruction stuff: top-level stuff here for 
-- convenience, specification-dependent parts in HS_SCSCP 
-- (parsePI and according data type)

-- read in characters from handle, until "?>" appears.
-- return all characters read, including the final "?>"
readUntilPI :: Handle -> IO String
readUntilPI h = trace "starting to readUntilPI" (accum "")
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
              '>' -> -- trace "finished reading until PI" 
                     (return (reverse (c:acc)))
              other -> accum (c:acc)

-- extract one processing instruction of form "<?scscp ... ?> from a
-- stream received from a handle, deliver the prefix string as well:
splitAtPI :: Handle -> IO (String,SCSCP_PI)
splitAtPI h = do traceM "splitAtPI"
                 input <- readUntilPI h
                 traceM (show input)
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
                        Cancel -> traceM "canceled" >>  --  $
                                  return Nothing
                        End    -> traceM "and scscp end PI" >>  --  $
                                  return (Just block)
                        other  -> traceM ("unexpected PI: " ++ show other) >>
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

------------------------------------------------------
-- MAIN communication routine

-- uses the (hopefully) stored server handle to 
exchangeSCSCP :: SCSCPMsg -> IO SCSCPMsg
exchangeSCSCP (PResult _ _ _ _) 
    = error "PResult from client to server?"
exchangeSCSCP (PTerminated _ _ _ _) 
    = error "PTerminate from client to server?"
exchangeSCSCP msg
 = do traceM ".. scscp message exchange... "
      check <- checkServerState
      if not check then error "no server initialised" 
                   else do
      ptr <- creadMVarPointer
      syncVar <- deRefStablePtr ptr
      server <- takeMVar syncVar
-- caught by checkServerState now... 
--       if (not (started server)) 
--         then error ("Server " ++ scscpHost server 
--                     ++ "is not initialised")
--         else do
      if (not (isAsync server))
        then do
	   traceM (".. sync. server: "++(show server))
           let hdl = scscpHandle server
           C.catch (do -- synchronised:
              hPutStrLn hdl (writePI Start)
	      hFlush hdl
              hPutStrLn hdl (writeSCSCPMsg msg)
	      hFlush hdl
              hPutStrLn hdl (writePI End)
	      hFlush hdl
              output <- readBlock hdl
#ifdef __PARALLEL_HASKELL__
              rnf output `seq` -- ensure all is read in
                putMVar syncVar server
#else
# warning Eden.rnf output `seq` omitted in sequential compilation
              putMVar syncVar server
#endif
              case output of 
                Just s  -> traceM "parsing ..." >> --  $ 
                           let msg = 
                                     readSCSCPMsg (removeNamespaceOM s)
                           in msg `seq` -- to catch error here
                                traceM "msg parsed" >> --  $ 
                                return msg 
                Nothing -> traceM "NO MSG parsed; sending a PTerminated" >> --  $ 
                           return (PTerminated "parallel-no-ID" 
                                   (SystemError "no output received") 
                                   Nothing Nothing)
                   ) (\e ->
                           do traceM ("error during msg. exchange" ++ show e)
                              empty <- isEmptyMVar syncVar
                              when empty (putMVar syncVar server)
                              error ("msg. exchange failed with " ++ show e))
        else do  
	   traceM (".. async. server"++(show server))
           let hdl = scscpHandle server
               rqueue = replyQ server
-- asynchronous method.
-- * creates an empty MVar(OMObj) for the result
--   (mechanism might as well use MVar (SCSCPMsg) )
-- * takes handle, writes out PCall
-- * frees server handle
-- * (on success only:) 
--    - registers call_ID and MVar in some global store
--    - creates a closure res = [unsafePerformIO] (readMVar resultVar)
--    - returns this closure
-- a (global) answer-reading thread (replyThread in initServer) reacts on all answer as follows:
-- * reads answer, extracts call_ID
-- * looks up result MVar in global store
-- * on success: stores the reply
           C.catch (do -- synchronised:
              hPutStrLn hdl (writePI Start)
	      hFlush hdl
              hPutStrLn hdl (writeSCSCPMsg msg)
	      hFlush hdl
              hPutStrLn hdl (writePI End)
	      hFlush hdl
              -- if we reach here, the message is out
              -- otherwise, restore old state
                   ) (\e
                         -> putMVar syncVar server >> 
                            error ("msg. exchange failed with " 
                                   ++ show e))
           resultVar <- newEmptyMVar
           let cID = callID msg
               newServer = server 
                           {replyQ = rqueue ++ [(cID,resultVar)]}
               readRes = unsafePerformIO (readMVar resultVar)
           putMVar syncVar newServer
           return readRes -- caller will read (and block)


-- bogus-functional interface for PCalls. Name and Arg.list
{-# NOINLINE callSCSCP #-}
callSCSCP :: CAName -> [OMObj] -> OMObj
callSCSCP fName args
    =   -- ensure we have all arguments (could otherwise end up in a nested call!)
      trace ("before eden stuff... " ++ (show args))
      (
#ifdef __PARALLEL_HASKELL__
      rnf args `seq` 
#else
# warning Eden.rnf args `seq` omitted in sequential compilation
#endif
      unsafePerformIO $ do
      traceM (".. callSCSCP function " ++ show fName ++ "args: " ++ (show args))
      call_ID <- newID
      let pcall = PCall call_ID fName args defaultProcOptions
      traceM (".. pcal = " ++ (show pcall))
      traceM ".. message with ID "
      answer <- exchangeSCSCP pcall
      traceM  (".. answer: "++(show answer))
      if (callID answer /= call_ID) 
        then do traceM ("** wrong callID " ++ callID answer 
                        ++ ", expected " ++ call_ID)
                severeError ("received wrong call ID!!! "++"expected: "++(show call_ID)++" received: "++(show (callID answer)))
        else case answer of 
               PTerminated _ err t m 
                   -> do traceM "Error."
                         error (errTypeName err ++ "." ++ errText err)
               PResult res _ t m     -> return res
      )
call0 :: CAName -> OMObj 
call0 name = unsafePerformIO (putStrLn $ "call0 of "++(show name)) `seq`   
             (callSCSCP name [])

call1 :: (OMData a, OMData b) =>
         CAName -> a -> b
--       ------ 
--     still ugly :( 
-- use Template Haskell to generate these guys
call1 name x  = fromOM (callSCSCP name [toOM x])

call2 :: (OMData a, OMData b, OMData c) =>
         CAName -> a -> b -> c
call2 name x y = res
    where res = fromOM (callSCSCP name [toOM x,toOM y])


call2'' :: (OMData a, OMData b, OMData c, Show b) =>
         CAName -> a -> b -> c
call2'' name x y = trace ("in call2, with: " ++ (show (toOM y) ++ " " ++ (show y))) res
    where res = fromOM (callSCSCP name [toOM x,toOM y])


-----------------------------------------
-- aims at parallelisation and optimisation: special version which
-- leaves the result in the Server. To do this, we have to descend
-- into callSCSCP, where options are encoded, and we need a method to
-- explicitly retrieve the data afterwards.

call1R :: OMData a => CAName -> a -> CARef
call1R name arg = callSCSCPRef name [toOM arg]

call2R :: (OMData a, OMData b) =>
          CAName -> a -> b -> CARef
call2R name x y = callSCSCPRef name [toOM x, toOM y]

{-# NOINLINE callSCSCPRef #-}
callSCSCPRef :: CAName -> [OMObj] -> CARef
callSCSCPRef fName args
    =   -- ensure we have all arguments (could otherwise end up in a
        -- nested call!)
#ifdef __PARALLEL_HASKELL__
      rnf args `seq` 
#else
# warning Eden.rnf args `seq` omitted in sequential compilation
#endif
         unsafePerformIO $ do
           traceM ("callSCSCPRef function " ++ show fName)
           call_ID <- newID
           let pcall = PCall call_ID fName args opts
               opts  = defaultProcOptions{pcResult=Just ResultRef}
           traceM "message with ID "
           answer <- exchangeSCSCP pcall
           traceM  "answer "
           if (callID answer /= call_ID) 
             then do traceM ("wrong callID " ++ callID answer 
                             ++ ", expected " ++ call_ID)
                     severeError ("received wrong call ID!!!")
             else case answer of 
                    PTerminated _ err t m 
                        -> do traceM "Error."
                              error (errTypeName err ++ "." 
                                     ++ errText err)
                    PResult res _ t m
                        -> return (fromOM res) -- ::CARef

{- NOTE: We should have a lot more type safety here!!!

Checking argument and result types is not feasible, since we would be
obliged to model all OpenMath types in the Haskell API. And functions
are usually very flexible and polymorphic in systems like GAP or
Maple.

What we can have is checked function arities when calling them.

Severals ways to achieve it:

1. introduce types for different arities, user has to give the right
   type to his functions. Must include a *-arity type, which will be a
   safety hole. 

2. Make CAName a structure with named fields, and encode the function
   arity inside CAName, check when calling functions with call<n>.
   This is more or less what SCSCP itself does, when retrieving
   information by "GetServiceDescription" or "GetSignature". Variable
   argument count is encoded by minimum/maximum values instead of one
   single, and unlimited no. of arguments is given as maximum "-1".


2 is clearly the easier solution, so we will eventually implement it.
Should not lead to too many changes: additional information should
anyway be stripped before putting the function into its PCall
structure. So we just use not CAName, but yet another type.  

Drawback: Wrong calls lead to runtime errors. The other solution would
be statically checked.

-}

-- code draft for this: 
data SCSCPFunction = Function { f_cd     :: String
                              , f_name   :: String
                              , f_minarg :: Int
                              , f_maxarg :: Int
                              }
                   | SCSCP { f_stdname :: CAStandardName
                           , f_minarg  :: Int
                           , f_maxarg  :: Int
                           }
isF :: SCSCPFunction -> Bool
isF (Function _ _ _ _) = True
isF _ = False

-- this follows an easy scheme, can be generated by Template Haskell
-- (replace the "3" by any "N", provide N+1 type variables with
-- instance OMData and include arguments x1..xN in def. pattern)
call3checked :: (OMData a, OMData b, OMData c, OMData d) => 
                SCSCPFunction -> a -> b -> c -> d
call3checked f x1 x2 x3 
    | min <= 3 && ( max == -1 || max >= 3 ) 
        = fromOM (callSCSCP caname 
                     [ toOM x1
                     , toOM x2
                     , toOM x3])
    | otherwise = error ("Arity error: function " ++ f_name f 
                         ++ " expects between " ++ show min ++ " and " 
                         ++ show max ++ " arguments (3 given).")
          where caname = if isF f then Left  (f_cd f, f_name f)
                                  else Right (f_stdname f) 
                min = f_minarg f
                max = f_maxarg f
                
