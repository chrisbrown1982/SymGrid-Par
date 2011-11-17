----------------------------------------------------------------------------
-- GHC interface to Kant (including Eden extensions)
--
-- Version 0.2
--
-- (c) 2006
--     Abdallah Al Zain <ceeatia@macs.hw.ac.uk>
--     
--
-- (c) 2003
--     Ricardo Peña-Mari <ricardo@sip.ucm.es>
--     Rafael Martínez-Torres <rmartine@fdi.ucm.es>
--
-- Based on Haskell-Interface
--
-- (c) 2000
--     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
--     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
--
----------------------------------------------------------------------------

module KantAPI
  (KantObject,                      -- Haskell representation of Kant things
   KantPoly,KantCoeffs,KantVar,KantInt, -- Renaming Types
   KantCallable(..),			    -- Class 
   nullMO,				    -- The NULL Kant-object
   nullPO,				    -- The 0 Kant-Object
   kantEval, kantEvalN,            -- evaluating a Kant function
                                     -- conversion functions for basic types:
   string2KantName, 
   string2KantExpr,
   int2KantObject, kantObject2Int,
   integer2KantObject, kantObject2Integer,
   char2KantObject, kantObject2Char,
   string2KantObject, kantObject2String, 
   showKantObject,			   -- debugging only:
#ifdef __PARALLEL_HASKELL__
   kantInitAllGum, kantTermAllGum ) where -- the initializing  calls for the distributed interface across the GUM
#else
   kantInit , kantTerm) where		   -- ditto for the only STG
#endif

/* version string used for communication with Kant, see start file */
#define VERSION "HaskellKant 0.2"


-- @menu
-- * Imports::			
-- * Interface Specification::	
-- * KantObject Type::		
-- * Classes and Instances::	
-- * Kant calls::		
-- * High level conversion routines::  
-- * Communication with Kant process::	 
-- * Low level conversion routines::  
-- * Operations on Stream of Bytes::  
-- * Lock/Unlock operations::	
-- * Debugging routines::	
-- * Settting up/down interface operations::  
-- * Basic IO operations::	
-- * Evaluation routines::	
-- * Conversion Routines::	
-- @end menu

-- @node Imports, Interface Specification
-- @section Imports

import Language.Haskell.TH.Lib
import Foreign.C.Error
-- import PrelIOBase	-- unsafePerformIO, to bypass de Monad type.
			-- "Mephisto in action"
import IO		-- debuggery...
import Control.Parallel.Strategies
--import Strategies	-- ways for rnf ( Reduced normal form)
			-- You need it in a IO() context 
-- import Posix		-- access to pipes, process
			-- Implements IPC (Inter Proces Communication)
			-- since GAP is provided as separate process
import StablePtr	-- given a pointer, prevents its bound object
			-- to be affected by the garbage collector
		        -- usefull to simulate a "imperative persitent	state" in the 
			-- interface. See subsquent.
import Monitor          -- An "external context" monitor. See FFI.
import Control.Concurrent -- Concurrent
import System.IO.Unsafe -- PrelIOBase
import System.Posix.IO
import System.Posix.Process
import System.Posix
import System.Environment
import Foreign.C.Types

import qualified NFData

#ifdef __PARALLEL_HASKELL__
import Eden		-- Enabling remote processes in a functional style.
import ParPrim
#endif



-- @node Interface Specification, KantObject Type, Imports
-- @section Interface Specification

---------------------------------------------------------------------------
-- The interface consists of the following functions
--
-- * set up/down the kant interface
--   
--   kantInitPE  / kantInit
--   kantTermPE  / kantTerm
--
-- * call a kant function "fun"
--
--    r  = kantEval  "fun" [ a ]
--   [r] = kantEvalN "fun" [ a ]
--
--    a: Int, Char, String, KantObject
--    r: KantObject
--
-- if Kant functions are called multiple times with same arguments,
-- it is more efficient to convert Int, Char, String to  KantObject before
--
-- * conversion functions
--
--   m =    int2KantObject i, i = kantObject2Int    m
--   m =   char2KantObject c, c = kantObject2Char   m
--   m = string2KantObject s, s = kantObject2String m
--   m = string2KantName s
--   m = string2KantExpr s
--
--   m: KantObject, I: i: Int, c: Char, s: String
---------------------------------------------------------------------------

-- @node KantObject Type, Classes and Instances, Interface Specification
-- @section KantObject Type

---------------------------------------------------------------------------
-- KantObjects are "chunks" coming back from calls
-- to Kant routines as a stream of bytes.
--
-- According to HM "Haskell-Kant" protocol
-- implemented in the 0.1 version:
--
--          -----------------
-- Format : | Header | Body |
--	    -----------------
--
-- Header stands for control code, while Body stands for data
--
--          ------------------------------------------------------------
-- Format : | Header | Meaning						|
--	    ------------------------------------------------------------
--	    |  in    |	Ask Kant for coding contents in Body		|
--	    |  out   |		      decoding				|
--	    |  fun n |	Ask Kant for function evaluation, Bod. are pp	|
--	    |  lst n |		ditto, for multiple results		|
--	    |  0     |	Ok from Kant, Body holds answer		|
--	    |  <0    |	NOK. Body holds reason				|
--	    |  >0    |	Ok: Body holds n results			|
--	    ------------------------------------------------------------
--
--
-- Body:
-- The basis for the KantObject constructor has been
-- simplified with respect to 0.1 version to -String-
-- instead of -ByteArray-.
-- Reasons were three:
--
-- 1.- The underlying type for
--     the original POSIX-IPC call has type String
--
--     import Posix
--     createPipe :: IO (Fd,Fd)
--     fdRead :: Fd -> ByteCount -> IO (String,ByteCount)
--
-- (We hope to read all kind of bytes, not only printable ones...)
--
-- 2.- Lectures on -fd- may block the entire threading
--     system ,so we need to promote the -fd- to -Handle-
--     the proper way from Haskell IO(), hence 
--
--		fdToHandle :: Fd -> IO (Handle)
--		hPutStrLn :: Handle -> String
--
-- 3.- Fortunately, HM was designed as a Line-Oriented Protocol (\n at end of buffer)
--     and use, so we get advantage of "automatic flushing" on the above call.
--      However, when
--
--	string <- hGetLine handle
--
--	We don't (cannot) take care any more on fine control of number of bytes,
--       as previous version 1.0
--	LINELEN 160
--	BUFFERLEN 16000
--	Up to now, no problem, but you are warned for debugging.
---------------------------------------------------------------------------
data KantObject = MAR String 

-- Some Kant usefull types aliases.
type KantPoly   = KantObject
type KantCoeffs = KantObject
type KantVar    = KantObject
type KantInt    = KantObject


-- @node Classes and Instances, Kant calls, KantObject Type
-- @section Classes and Instances

-- class of things that can be passed to Kant
-- @cindex KantCallable
class KantCallable a where
  -- convert this Haskell object into a KantObject  
  toKantObject :: a -> KantObject
  -- and the other direction, too	
  fromKantObject :: KantObject -> a
  -- write this Kant object to a file	
  writeToKant :: Handle {-Handle-} -> a -> IO ()

-- a Haskell fixed precision integer
instance KantCallable Int where
  toKantObject = int2KantObject
  fromKantObject = kantObject2Int
  writeToKant hdl i = hPutStrLn hdl (show i)

-- a Haskell arbitrary precision integer
instance KantCallable Integer where
  toKantObject = integer2KantObject
  fromKantObject = kantObject2Integer
  writeToKant hdl i = hPutStrLn hdl (show i)

-- a Haskell character
instance KantCallable Char where
  toKantObject = char2KantObject
  fromKantObject = kantObject2Char
  writeToKant hdl c = hPutStrLn hdl (show c)

-- a Haskell string
instance KantCallable String where
  toKantObject = string2KantObject
  fromKantObject = kantObject2String
  writeToKant  = hPutStrLn 

-- a Kant object
instance KantCallable KantObject where
  toKantObject = id
  fromKantObject = id
  writeToKant hdl (MAR str) = hPutStrLn hdl str

-- a KantObject can be shown 
instance Show KantObject where
  show = showKantObject

-- a KantObject can be compared
instance Eq KantObject where
  MAR str == MAR str' = str == str'

-- a KantObject can be reduced to normal form.
instance Control.Parallel.Strategies.NFData KantObject where
  rnf (MAR str) = (rnf str) 

#ifdef __PARALLEL_HASKELL__
instance NFData.NFData KantObject where
    rnf (MAR str) = (rnf str) 

instance Trans KantObject where
-- A CType (see FFI) must be transmissible
-- Need to transmit the number
-- of implicit (or extra-process) threads inside the mutex region. 
instance Control.Parallel.Strategies.NFData CInt where
instance Trans CInt where
instance NFData.NFData CInt where
--instance Eden.Trans CInt where
#endif

-- this MO means "undefined"
nullMO :: KantObject
nullMO = MAR ""

-- this MO means the "0" constant
-- may be safe traffic if you get it as constant.
nullPO :: KantPoly
nullPO = MAR "\"\"!"


---------------------------------------------------------------------------
-- @node Kant calls, High level conversion routines, Classes and Instances
-- @section Kant calls
---------------------------------------------------------------------------

-- ToDo:
--  . define a set of exceptions for possible errors on Kant side


-- Implements a Kant call;
-- name is a string; args anything KantCallable
-- Note how we enter the monad on a external context (FFI)
-- to grab the pipe handles.
-- @cindex kantEval
kantEval :: (KantCallable a) => String -> [a] -> KantObject
kantEval name args = 
  unsafePerformIO $
  do
    inpipep <- creadInPipePointer
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    kantEvalM inpipe outpipe name args

-- pure, monadic version of the Kant call
-- Take care of mylock call in order to implement
-- a mutex region
kantEvalM :: (KantCallable a) => Handle -> Handle -> String -> [a] -> IO KantObject
kantEvalM inpipe outpipe name args =                 -- <kant fct> <list of kant args>
  do
#if defined(DEBUG)
    hPutStrLn stderr ("-> kantEvalM" ++ show name ++ " args ->")
    hFlush stderr
#endif
    let 
      n = length args                  -- number of args to kant fct call
      mos = map toKantObject args     -- convert all args to KantObjects
    mylock                    `demanding` Control.Parallel.Strategies.rnf mos 
    kantStartEval outpipe n 
    kantEvalCore outpipe name mos           -- write fct and arg names
    kantEndEval			     -- write call-epilogue
    kantEvalWait inpipe                     -- wait for result
    mstr <- kantResult inpipe		     -- grab result
    myunlock `demanding` Control.Parallel.Strategies.rnf mstr
    return (MAR mstr)                        -- and pull into Haskell-land

-- Implements a Kant call; returns a list of results
-- @cindex kantEvalN
kantEvalN :: (KantCallable a) => String -> [a] -> [KantObject]
kantEvalN name args = 
  unsafePerformIO $ 
  do 
    inpipep <- creadInPipePointer 
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    kantEvalNM inpipe outpipe name args

-- pure, monadic version of the Kant call
-- Note the use of Control.Parallel.Strategies.rnf to require
-- reduced normal form (rnf), since the weak head normal forg (whnf)
-- may cause problems in an interactive-monadic context
-- @cindex kantEvalNM
{-# INLINE kantEvalNM #-}
kantEvalNM :: (KantCallable a) => Handle -> Handle -> String -> [a] -> IO [KantObject]
kantEvalNM inpipe outpipe name args =
  do 
    let 
      n = length args                  -- number of args to kant fct call
      mos = map toKantObject args     -- convert all args to KantObjects
    mylock        `demanding` Control.Parallel.Strategies.rnf mos 
    kantStartEvalN outpipe n
    kantEvalCore outpipe name mos          -- write fct and arg names
    kantEndEval			    -- write call-epilogue
    kantEvalWait inpipe                    -- wait for result
    m <- kantResultN inpipe		    -- grab result
    l <- mapM ( \ _ -> do
                  mstr <- kantNextResult inpipe   -- get next result
		  let x = (MAR mstr)
                  return x `demanding` Control.Parallel.Strategies.rnf x) [1..m]  
    myunlock	  `demanding` Control.Parallel.Strategies.rnf l
    return l				--  list elem!


---------------------------------------------------------------------------
-- @node High level conversion routines, Communication with Kant process, Kant calls
-- @section High level conversion routines
---------------------------------------------------------------------------

-- a -> KantObject

-- convert a Haskell Int to a KantObject
-- @cindex int2KantObject
int2KantObject :: Int -> KantObject
int2KantObject = unsafePerformIO . int2KantObjectM

-- Pure, monadic version of previous
-- Note how we enter the monada on a external context (FFI)
-- to grab the pipe.
-- @cindex int2KantObjectM
int2KantObjectM :: Int -> IO KantObject
int2KantObjectM n = 
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   kantInt inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n
 


-- convert a Haskell arbitrary precision integer to a KantObject
-- @cindex integer2KantObjectM
integer2KantObject :: Integer -> KantObject
integer2KantObject = unsafePerformIO . integer2KantObjectM

-- Pure, monadic version of previous
-- ditto
-- @cindex integer2KantObjectM
integer2KantObjectM :: Integer -> IO KantObject
integer2KantObjectM n =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   kantInteger inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n

-- convert a Haskell Char to a KantObject
-- @cindex char2KantObject
char2KantObject :: Char -> KantObject
char2KantObject = unsafePerformIO . char2KantObjectM


-- Pure, monadic version of previous
-- Note how we enter the monad
-- to grab the pipe from external context (FFI)
-- @cindex char2KantObjectM
char2KantObjectM :: Char -> IO KantObject
char2KantObjectM c =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   kantChar inpipe outpipe c `demanding` Control.Parallel.Strategies.rnf c



-- convert a KantObject to a Haskell String
-- @cindex string2KantExpr
string2KantExpr :: String -> KantObject
string2KantExpr = unsafePerformIO . string2KantExprM

-- Pure, monadic version of previous
-- Ditto
-- @cindex string2KantExprM
string2KantExprM :: String -> IO KantObject
string2KantExprM str = 
  do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   kantExpr inpipe outpipe str `demanding` Control.Parallel.Strategies.rnf str


-- convert a Haskell String to a Kant String 
-- @cindex string2KantObject
string2KantObject :: String -> KantObject
string2KantObject x = string2KantExpr ("\"" ++ x ++ "\"")

-- convert a Haskell String to a Kant name ( not an expression! )
-- Usually a name in Kant is used as a way
-- to implement "delayed " evaluation mode (lazy?). 
-- ( read Kant User's Guide )
-- @cindex string2KantName
string2KantName x = string2KantExpr ("`" ++ x ++ "`")



--  KantObject -> a


-- convert a KantObject to a Haskell Int
-- Ditto.
-- @cindex kantObject2Int
kantObject2Int :: KantObject -> Int
kantObject2Int mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInt inpipe outpipe mo


-- convert a KantObject to a Haskell arbitrary precision Integer
-- Ditto.
-- @cindex kantObject2Integer
kantObject2Integer :: KantObject -> Integer
kantObject2Integer mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInteger inpipe outpipe mo


-- convert a KantObject to a Haskell Char
-- ditto
-- @cindex kantObject2Char
kantObject2Char :: KantObject -> Char
kantObject2Char mo =   unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellChar inpipe outpipe mo


-- convert a KantObject to a Haskell String
-- ditto
-- @cindex kantObject2String
kantObject2String :: KantObject -> String
kantObject2String mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellString inpipe outpipe mo


---------------------------------------------------------------------------
-- @node Communication with Kant process, Low level conversion routines, High level conversion routines
-- @section Communication with Kant process
--
-- Writing a KantObject directly to the pipe connecting with the Kant proc
-- 
-- This section has been dropped from the 0.1 version.
-- writeStringToFile, writeByteArrayToFile are just:
--
-- hPutStrLn handle mstr
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- @node Low level conversion routines, Operations on Stream of Bytes, Communication with Kant process
-- @section Low level conversion routines
--
-- This section has been dropped from the 0.1 version.

-- kantExtractChar_ret , kantExtractInt_ret
-- are just
-- 
-- read (string)
-- kantEextractMO is just the MAR Constructor.
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- @node Operations on Stream of Bytes, Lock/Unlock operations , Low level conversion routines
-- @section Operations on Stream of Bytes
--
-- Basic operations on the representation of KantObjects as ByteArrays
-- Most of these routines have been (aehm) borrowed from the PackedString
-- module. However, we use a simpler type, just a ByteArray.
--
-- This section has been dropped from the 0.1 version.
---------------------------------------------------------------------------



---------------------------------------------------------------------------
-- @node Lock/Unlock operations, Debugging routines , Operations on Steam of Bytes
-- @section Debugging routines
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- lock/unlock operations
--
-- This is very important to control concurrent access
-- from several threads to a common resource:
-- the pipe to Kant
--
-- Eden execution model implies implicit threading
-- i.e, (one thread per  element on the output tuple). In 
-- the case of Concurrent Haskell, threading is explicit (forkIO())
--
-- Hence, there two available synchronization tools:
--    * Traditional Concurrent.Mvar ( Concurrent Haskell )
--    * Eden.ChanName. ( Eden )
--
-- 
-- In the case of Concurrent Haskell, we save/grab the pointers
-- to/from  a "external context (FFI)" monitor
-- in order to free the end user ("algebraic programmer") of an explicit
-- control of concurrency, I mean:
-- 
-- kantEval :: (KantCallable a) => String -> [a] -> KantObject
--
-- instead of
--
-- kantEval :: (KantCallable a) => MVar -> String -> [a] -> KantObject
--
-- The use of the IO() monad  to access
-- the "external context (FFI)" (the same to further sending of values via pipe)
-- contribute to smooth the gap between the imperative and functional world. 
-- 
-- When using Eden lenguage, we firstly thought to take advantage of latency
--
-- new (\nc n -> e ) (waiting for value available on n)
--
-- to implement synchronization 
--
-- however,  objections are clear:
-- * explicit ChanNames are designed to commuicate (explicit) processes (hence implicit threads),
-- not implicit  local threads inside a process. 
--  So, here is the trick: the only way to force this use is to pass the ChanName
-- through a peristent queue local to the PE, via "external context (FFI)".
--
-- Yes, this is a forced use of the underlying model/language.
--
-- BTW, don't try to instantiate an process with an MVar as a parameter,
-- since the distributed memory make it sense-less. (Thread Concurrency inside
-- a PE has nothing to do with other remote one). Only has sense when there is an only
-- PE (Eden-RTS is ready to support concurrent processes inside a
-- PE).
-- Even more: Technically -I- don't know how to make
-- MVar to be reduced to normal form,
-- hence it cannot be Transmissible.
---------------------------------------------------------------------------

#ifdef __PARALLEL_HASKELL__
---------------------------------------------------------------------------
--
-- Queue of Eden.ChanNames (available only for Eden)
--
-- Note how we enter the monad
-- to grab the pointer (FFI) for the queue.
---------------------------------------------------------------------------
mylock :: IO ()
mylock  = 
  do
    l <- ctestAndInc
                     -- The "ctestAndInc" calls are intended to be executed
                     -- atomically, an this is done so
                     -- at least on ghc-5.02.3 / FFI 1.0
                     -- Further implemetations/configurations may change !!!
    if (l>0) -- Have to "wait signal"
      then do
	     new (\ chn (c::CInt) -> do
#if defined(DEBUG)
		                      hPutStrLn stderr ("\t\t\t\t\t\\/WAIT(" ++ show l++")" )
		                      hFlush stderr
#endif
		                      queuep <- creadQueuePointer
                                      queue <- deRefStablePtr queuep
				      writeChan queue chn
				      return () `demanding` Control.Parallel.Strategies.rnf c)  
      else
          do
	     return()

---------------------------------------------------------------------------
-- Signal function.
---------------------------------------------------------------------------
myunlock :: IO ()
myunlock = 
  do
    l <- ctestAndDec
    if (l==0)
      then return ()
      else do 
#if defined(DEBUG)
	     hPutStrLn stderr ("\t\t\t\t\t/\\SIGNAL(" ++ show l ++")" )
             hFlush stderr
#endif
             queuep <- creadQueuePointer
             queue <- deRefStablePtr queuep
	     chn <- readChan queue
	     parfill chn (l::CInt) return()

#else

---------------------------------------------------------------------------
--
-- Traditional Concurrent.Mvar 
-- It is intended for Concurrent Haskell, but you can try
-- also for Eden.
--
-- ditto
--
---------------------------------------------------------------------------
mylock :: IO ()
mylock  = 
  do
    mvarp <- creadMvarPointer
    mvar <- deRefStablePtr mvarp
    takeMVar mvar
    return ()

myunlock :: IO ()
myunlock = 
  do
    mvarp <- creadMvarPointer
    mvar <- deRefStablePtr mvarp
    putMVar mvar ()
    return ()

#endif




-------------------------------------------------------------------
-- @node Debugging routines, Settting up/down interface operations, Lock/Unlock operations 
-- @section Debugging routines
---------------------------------------------------------------------------
-- This section has been almost dropped.
{-
#if defined(DEBUG)
isSaneMO :: KantObject -> Bool
isSaneMO (MAR str) = and (map ( \ c -> isAscii c && (not (isControl c))) str)
#endif
-}

-- length of the ByteArray
-- @cindex lengthMO
lengthMO   :: KantObject -> Int
lengthMO (MAR str) = length str

showKantObject :: KantObject -> [Char]
showKantObject mo@(MAR str) = "DBG> Size: " ++  (show (lengthMO mo)) ++ "; Contents: " ++ str


-------------------------------------------------------------------
-- @node Settting up/down interface operations, Basic IO operations, Lock/Unlock operations 
-- @section Settting up/down interface operations
---------------------------------------------------------------------------


#ifdef __PARALLEL_HASKELL__
-- This is the famous patch to solve the replication of Kant
-- at around the entire GUM...
-- Inspired on Sysman.c by Kevin Hammond,
-- but programmed at Eden level, thanks to process abstraction:
-- 
-- Replace in mind  "pvm_spawn()" by "process _ #"
-- and you will get the same idea.
--
-- Idea:
-- Eden should export the id of PE on its API (as noPE)
-- and we could check that this happens on every PE.
--
-- We have to call this as the first function on main()
-- Otherwise, if no of processs > no PE
-- things may become a chaos !!!
--

-- kantInitAllGum :: IO (Int) -- return how many PE's have been setup.
-- kantInitAllGum = do   hPutStrLn stderr ("\t====== Setting up KANT ...." ++ show Eden.noPe ++ " ")
-- 		      peNos <- mapM (\pe -> instantiateAt pe (process kantInitLocalGum) pe ) [1..Eden.noPe] 
-- 		      rnf peNos `seq` 
-- 			  return (length peNos)

kantInitAllGum :: IO (Int) -- return how many PE's have been setup.
kantInitAllGum = do
		     hPutStrLn stderr ("\t====== Setting up Kant around GUM...."++ show Eden.noPe ++ " ")
                     let a = [process (kantInitLocalGum) # (i) | i <- [1..Eden.noPe] ] `using` spine
		     return (length a) `demanding` Control.Parallel.Strategies.rnf a

-- (Should) return the id for the PE in GUM.
kantInitLocalGum :: Int -> Int
kantInitLocalGum = unsafePerformIO . kantInitLocalGumM

kantInitLocalGumM :: Int -> IO (Int) -- return the number of the PE just setup
kantInitLocalGumM i = 
#else
kantInit :: IO ()
kantInit =
#endif
 do
#ifdef __PARALLEL_HASKELL__
   -- These signal handlers seem not to bet processed by Eden
   -- Reason: I guess PVM Signaling system overwrites Eden's.
#else
   -- install signal handler to terminate Kant process properly 
   installHandler sigTERM (Catch (kantTerm)) Nothing
   installHandler sigQUIT (Catch (kantTerm)) Nothing
#endif
   -- setup communication
   -- open in main process buffered pipes to subprocess
   (inr,inw) <- createPipe   
   (outr,outw) <- createPipe 
#ifdef __PARALLEL_HASKELL__
   queue <- newChan
#else
   mvar <- newMVar ()
#endif
   -- fork process (POSIX, not Concurrent !!!)
   mpid <- forkProcesses
   case (mpid) of
     Nothing	-> do     -- redirect in subprocess stdin and stdout to pipes
	             dupTo outr stdInput
		     dupTo inw  stdOutput
 		     executeFile KANTBIN False [KANTARGS] Nothing
		     error "cannot execute file"
#ifdef __PARALLEL_HASKELL__
		     return(i)
#else
		     return()
#endif
     Just pid	-> do  -- Just ProcessID
		      threadDelay 100000 -- Arbitrary time
		      -- check that child lives
		      -- status <- getProcessStatus False False pid
                      -- By promoting these values, underlying RTS does not
                      -- block the rest of threads while reading...
		      inpipe <- fdToHandle inr
		      outpipe <- fdToHandle outw
		       --  input MUST be linebuffered (as terminals), so
                       -- automatic flushing is done when enter a line.
		      hSetBuffering inpipe LineBuffering
		      hSetBuffering outpipe LineBuffering
		      hSetBuffering stderr LineBuffering
		       -- send initialization and startup commands to  subprocess
		      kantRoot <- System.Environment.getEnv "casROOTEden"
		      -- kant read this file at the start .kantpro40/userinit.mu ; no need for init.kant 
-- 		      hPutStrLn outpipe ("read `" ++ kantRoot ++ "/init.kant`:")
		      -- start.kant is increpted in the starting call; no need for start.kant
-- 		      hPutStrLn outpipe  ("read `"++ kantRoot ++ "/start.kant`:")
 		      hPutStrLn outpipe  ("ready")
		      -- make termination sure
		      -- wait for process to be ready
		      a <- hGetLine inpipe
#if defined(DEBUG)
		      putStrLn ("\tLaunching pid...: \t" ++ (show pid))
		      putStrLn ("\tKANT SERVER :... " ++ a)
#endif
		      -- first line in protocol is name of debug file 
		      hPutStrLn outpipe (kantRoot ++ "/error." ++ show(pid))
		      --- Instead of return parameters, just record it,
		      --- into C land.
		      inpipep <- newStablePtr inpipe
		      outpipep <- newStablePtr outpipe
		      pidp <- newStablePtr pid
#ifdef __PARALLEL_HASKELL__
		      queuep <- newStablePtr queue
		      cwritePointers inpipep outpipep pidp queuep
		      return (i)
#else
		      mvarp <- newStablePtr mvar
		      cwritePointers inpipep outpipep pidp mvarp
		      return ()
#endif


#ifdef __PARALLEL_HASKELL__
-- Same as kantInitAllGum (read above)
-- You must call this function the last
-- on your main program
-- when no more processes are remaining
-- to asure 1:1 process:PE
-- Otherwise, concurrent process inside a PE
-- may become a chaos.
kantTermAllGum :: IO(Int) -- return how many PE's have been set down.
kantTermAllGum = do
 		      hPutStrLn stderr "\t====== Setting down Kant ...." 
 		      peNos <- mapM (\pe -> instantiateAt pe (process kantTermLocalGum) pe ) [1..Eden.noPe] 
 		      rnf peNos `seq` 
 		   	 return (length peNos)

-- kantTermAllGum :: IO(Int) -- return how many PE's have been set down.
-- kantTermAllGum = do
-- 		      hPutStrLn stderr ("\t====== Setting down Kant around GUM...." ++ show Eden.noPe ++ " ")
-- 		      let a = [process (kantTermLocalGum) # (i) | i <- [1..Eden.noPe] ]  `using` spine
-- 		      return (length a) `demanding` Control.Parallel.Strategies.rnf a

--
-- A hack to evaluate the list eagarly
--
spine :: [a] -> ()
spine [] = ()
spine (x:xs) = spine xs

-- (Should) return the Id of the PE.
kantTermLocalGum :: Int -> Int
kantTermLocalGum = unsafePerformIO . kantTermLocalGumM


-- Monadic version:
-- Killing the server process.		   
kantTermLocalGumM :: Int -> IO(Int)
kantTermLocalGumM i = 
#else
kantTerm :: IO()
kantTerm =
#endif
 do
   inpipep <- creadInPipePointer
   outpipep <- creadOutPipePointer
   pidp <- creadPidPointer
   inpipe <- deRefStablePtr inpipep
   outpipe <- deRefStablePtr outpipep
   pid <- deRefStablePtr pidp :: IO ProcessID
   hPutStrLn outpipe "terminate"
   hClose inpipe
   hClose outpipe
   freeStablePtr inpipep
   freeStablePtr outpipep
   threadDelay 1000000 -- sleep(1) may block the whole STG system.
#if defined(DEBUG)
   putStrLn ("\tKilling   pid... " ++ (show pid))
   putStrLn ("\tKANT SERVER :... terminate")
#endif
   signalProcess sigTERM pid
   signalProcess sigKILL pid
#ifdef __PARALLEL_HASKELL__
   return (i)
#else
   return ()
#endif


-------------------------------------------------------------------
-- @node  Basic IO operations, Evaluation routines ,Settting up/down interface operations
-- @section Debugging routines
---------------------------------------------------------------------------

----------------------------------------------------------------------------
-- This the basic IO operation on the Interface
-- Since the HM protocol is line oriented, 
-- we use the hGetLine on the handle
-- Note the handles are configured
-- to make flussing automaticaly when holding \n
------------------------------------------------------------------------- 
readKant :: Handle -> IO (String)
readKant hdl = do
	         line <- hGetLine hdl
		 return (line)

-------------------------------------------------------------------------------
--
-- Read the Header of a HM message.
--
-- Panic error: When the Kant server sends other thing
-- than an Integer, the system will abort dirtly
-- (as Haskell Prelude is designed to)
-- Be warned if you are debugging !! 
-------------------------------------------------------------------------------
readKantCode :: Handle -> IO(Int)
readKantCode hdl = do
		     line <- hGetLine hdl
		     return (read line)

-------------------------------------------------------------------
-- @node   Evaluation routines, Conversion Routines ,Basic IO operations
-- @section Debugging routines
---------------------------------------------------------------------------

----------------------------------------------------------------------------
-- start evaluation with n arguments and single result
--
-------------------------------------------------------------------------- 
kantStartEval :: Handle -> Int -> IO()
kantStartEval outpipe  n =
  do
    hPutStrLn outpipe "fun"
    hPutStrLn outpipe (show n)
    return ()

-- wait for Kant Peer answer to be available.
-- Since the subsequent IO-call -hGetLine-
-- operates on a  Handle -not a Fd-, 
-- the RTS prevents to block the rest of the threads.
-- So, we Keep it here for historical reasons.
kantEvalWait :: Handle -> IO ()
kantEvalWait inpipe =  return ()



-- This writes the fct name and the args (must have been converted to 
-- KantObjects already) to the given handle (the pipe).
-- @cindex kantEvalCore
{-# INLINE kantEvalCore #-}
kantEvalCore :: Handle -> String -> [KantObject] -> IO ()
kantEvalCore hdl name args =
  do 
    hPutStrLn hdl name
    mapM (\ arg -> (writeToKant hdl arg)) args
    return ()




------------------------------------------------------------------------------
-- kantResult
-- Reads the whole HM Message ( Header plus Body )
-- and lifts the body part
------------------------------------------------------------------------- 
kantResult :: Handle -> IO (String)
kantResult  inpipe = 
 do
   header <- readKantCode inpipe
   body <- readKant inpipe
   if (header/=0) 
      then
        do 
	  report "error in Kant function" body
	  return  body 
      else return  body 

----------------------------------------------------------------------------
-- end evaluation
-- In 0.1 version it used to commit the (fflush)
-- but in 0.2 automatic flush is achieved automatically when
-- a line is entered.
-- Kept here for historical reasons.
 ------------------------------------------------------------------------- */
kantEndEval :: IO ()
kantEndEval = return ()  




 ----------------------------------------------------------------------------
 -- start evaluation with n arguments and sequence of results
 --
 ------------------------------------------------------------------------- 
kantStartEvalN :: Handle -> Int -> IO()
kantStartEvalN outpipe  n =
  do
    hPutStrLn outpipe "lst"
    hPutStrLn outpipe (show n)
    return()


------------------------------------------------------------------------------
-- kantResultN
--
-- Same as previous, but multiple results...
-- Reads the  Header of the message,
-- if result was failure, then read also the body
-- otherwise, keep the handle ready to read the body
-- Always: 
-- Returning Header
-- To DO: Exception dealing on caller.
-- Be carefull when debuging.
-- 
------------------------------------------------------------------------- 
kantResultN :: Handle -> IO(Int)
kantResultN inpipe=
 do
   header <- readKantCode inpipe
   if (header<0)  -- Error in service, just log it.
     then
       do
	 body <- readKant inpipe   
	 report "error in Kant function" body
         return (header)
     else   return (header)  
   
 ------------------------------------------------------------------------------
 -- kantNextResult
 -- Assumed handle ready,
 -- reads subsequent body calls.
 ------------------------------------------------------------------------- */
kantNextResult :: Handle -> IO (String)
kantNextResult  inpipe = 
 do
   partialBody <- readKant inpipe
   return ( partialBody) 


-------------------------------------------------------------------
-- @node    Conversion Routines, , Evaluation routines,
-- @section Debugging routines
---------------------------------------------------------------------------
-- Protocol HM works in this way.
-- 1.- Sending header (in,out)
-- 2.- Sending body "[4,4,x^2]"
-- 3.- Receiving header (0,-1...)
-- 4.- Receiving body (KantObject)
--
-- WARNING: TODO: Exception on Kant side treatment.

----------------------------------------------------------------------------
-- convert Haskell string to Kant object
-- ------------------------------------------------------------------------- */
kantExpr:: Handle -> Handle -> String -> IO (KantObject)
kantExpr inpipe outpipe expr =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe expr
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- If not requiring rnf
	                                    -- there will be problems
                                            -- on a monadic IO() context
                                            -- blocking...
   if (header/=0)
    then
      do
       report "error in converting Haskell expression" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert Haskell int to Kant integer
-- ditto.
------------------------------------------------------------------------- */

kantInt::  Handle -> Handle -> Int -> IO (KantObject)
kantInt inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Int" body
       return  (MAR body)
    else
       return  (MAR body)



----------------------------------------------------------------------------
-- convert an arbitrary Haskell integer to Kant integer
-- ditto
------------------------------------------------------------------------- */
kantInteger::  Handle -> Handle -> Integer -> IO (KantObject)
kantInteger inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Integer" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert an arbitrary Haskell char to Kant char
-- ditto
------------------------------------------------------------------------- */
kantChar::  Handle -> Handle -> Char -> IO (KantObject)
kantChar inpipe outpipe char =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show char)
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header/=0)
    then
      do
       report "error in converting Haskell Char" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert Kant Object to Haskell string
--
--------------------------------------------------------------------------- */
haskellString ::Handle -> Handle -> KantObject -> IO(String)
haskellString inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Kant String" body
       return  (body)
    else
       return  (body)


----------------------------------------------------------------------------
-- convert Kant string to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInt ::Handle -> Handle -> KantObject -> IO(Int)
haskellInt inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Kant Int" body
       return  (read body)
    else
       return  (read body)

 ----------------------------------------------------------------------------
-- convert Kant Integer to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInteger ::Handle -> Handle -> KantObject -> IO(Integer)
haskellInteger inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Kant Int" body
       return  (read body)
    else
       return  (read body)

----------------------------------------------------------------------------
-- convert Kant Char to Haskell string
--
------------------------------------------------------------------------- */
haskellChar ::Handle -> Handle -> KantObject -> IO(Char)
haskellChar inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readKantCode inpipe
   body <- readKant inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Kant Int" body
       return  ( head body)
    else
       return  ( head body)



----------------------------------------------------------------------------
-- This routine tries to aid the programmer to debug 
-- in case of blocking the system
--
-- 2004-06-02
-- BTW, in a recent lecture by Rita Logen, I understood
-- Haskell Language, as is, cannot understand formatted strings
-- i.e "Expecting %1 fomr type %2 " ( as C does)
-- 
-- So, This is the problem:
-- As C language has been almost dropped from the interface
-- we are wasting information from Kant messages.
-- info is usually a formatted string.
------------------------------------------------------------------------- */
report:: String -> String -> IO()
report key info = do
		   hPutStrLn stderr ("kant interface error: "++key++"("++info++")")
--                 TODO:
-- If you want to keep the message on a given file,
-- try to redirect standard error to a any given file.

forkProcesses :: IO (Maybe ProcessID)
forkProcesses = do
    pid <- c_fork
    case (pid) of
      -1 -> throwErrno "forkProcess"
      0  -> return Nothing
      _  -> return (Just pid)
