----------------------------------------------------------------------------
-- GHC interface to MuPad (including Eden extensions)
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

module MuPadAPI
  (MuPadObject,                      -- Haskell representation of MuPad things
   MuPadPoly,MuPadCoeffs,MuPadVar,MuPadInt, -- Renaming Types
   MuPadCallable(..),			    -- Class 
   nullMO,				    -- The NULL MuPad-object
   nullPO,				    -- The 0 MuPad-Object
   mupadEval, mupadEvalN,            -- evaluating a MuPad function
                                     -- conversion functions for basic types:
   string2MuPadName, 
   string2MuPadExpr,
   int2MuPadObject, mupadObject2Int,
   integer2MuPadObject, mupadObject2Integer,
   char2MuPadObject, mupadObject2Char,
   string2MuPadObject, mupadObject2String, 
   showMuPadObject,			   -- debugging only:
#ifdef __PARALLEL_HASKELL__
   mupadInitAllGum, mupadTermAllGum ) where -- the initializing  calls for the distributed interface across the GUM
#else
   mupadInit , mupadTerm) where		   -- ditto for the only STG
#endif

/* version string used for communication with MuPad, see start file */
#define VERSION "HaskellMuPad 0.2"


-- @menu
-- * Imports::			
-- * Interface Specification::	
-- * MuPadObject Type::		
-- * Classes and Instances::	
-- * MuPad calls::		
-- * High level conversion routines::  
-- * Communication with MuPad process::	 
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



-- @node Interface Specification, MuPadObject Type, Imports
-- @section Interface Specification

---------------------------------------------------------------------------
-- The interface consists of the following functions
--
-- * set up/down the mupad interface
--   
--   mupadInitPE  / mupadInit
--   mupadTermPE  / mupadTerm
--
-- * call a mupad function "fun"
--
--    r  = mupadEval  "fun" [ a ]
--   [r] = mupadEvalN "fun" [ a ]
--
--    a: Int, Char, String, MuPadObject
--    r: MuPadObject
--
-- if MuPad functions are called multiple times with same arguments,
-- it is more efficient to convert Int, Char, String to  MuPadObject before
--
-- * conversion functions
--
--   m =    int2MuPadObject i, i = mupadObject2Int    m
--   m =   char2MuPadObject c, c = mupadObject2Char   m
--   m = string2MuPadObject s, s = mupadObject2String m
--   m = string2MuPadName s
--   m = string2MuPadExpr s
--
--   m: MuPadObject, I: i: Int, c: Char, s: String
---------------------------------------------------------------------------

-- @node MuPadObject Type, Classes and Instances, Interface Specification
-- @section MuPadObject Type

---------------------------------------------------------------------------
-- MuPadObjects are "chunks" coming back from calls
-- to MuPad routines as a stream of bytes.
--
-- According to HM "Haskell-MuPad" protocol
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
--	    |  in    |	Ask MuPad for coding contents in Body		|
--	    |  out   |		      decoding				|
--	    |  fun n |	Ask MuPad for function evaluation, Bod. are pp	|
--	    |  lst n |		ditto, for multiple results		|
--	    |  0     |	Ok from MuPad, Body holds answer		|
--	    |  <0    |	NOK. Body holds reason				|
--	    |  >0    |	Ok: Body holds n results			|
--	    ------------------------------------------------------------
--
--
-- Body:
-- The basis for the MuPadObject constructor has been
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
data MuPadObject = MAR String 

-- Some MuPad usefull types aliases.
type MuPadPoly   = MuPadObject
type MuPadCoeffs = MuPadObject
type MuPadVar    = MuPadObject
type MuPadInt    = MuPadObject


-- @node Classes and Instances, MuPad calls, MuPadObject Type
-- @section Classes and Instances

-- class of things that can be passed to MuPad
-- @cindex MuPadCallable
class MuPadCallable a where
  -- convert this Haskell object into a MuPadObject  
  toMuPadObject :: a -> MuPadObject
  -- and the other direction, too	
  fromMuPadObject :: MuPadObject -> a
  -- write this MuPad object to a file	
  writeToMuPad :: Handle {-Handle-} -> a -> IO ()

-- a Haskell fixed precision integer
instance MuPadCallable Int where
  toMuPadObject = int2MuPadObject
  fromMuPadObject = mupadObject2Int
  writeToMuPad hdl i = hPutStrLn hdl (show i)

-- a Haskell arbitrary precision integer
instance MuPadCallable Integer where
  toMuPadObject = integer2MuPadObject
  fromMuPadObject = mupadObject2Integer
  writeToMuPad hdl i = hPutStrLn hdl (show i)

-- a Haskell character
instance MuPadCallable Char where
  toMuPadObject = char2MuPadObject
  fromMuPadObject = mupadObject2Char
  writeToMuPad hdl c = hPutStrLn hdl (show c)

-- a Haskell string
instance MuPadCallable String where
  toMuPadObject = string2MuPadObject
  fromMuPadObject = mupadObject2String
  writeToMuPad  = hPutStrLn 

-- a MuPad object
instance MuPadCallable MuPadObject where
  toMuPadObject = id
  fromMuPadObject = id
  writeToMuPad hdl (MAR str) = hPutStrLn hdl str

-- a MuPadObject can be shown 
instance Show MuPadObject where
  show = showMuPadObject

-- a MuPadObject can be compared
instance Eq MuPadObject where
  MAR str == MAR str' = str == str'

-- a MuPadObject can be reduced to normal form.
instance Control.Parallel.Strategies.NFData MuPadObject where
  rnf (MAR str) = (rnf str) 

#ifdef __PARALLEL_HASKELL__
instance NFData.NFData MuPadObject where
    rnf (MAR str) = (rnf str) 

instance Trans MuPadObject where
-- A CType (see FFI) must be transmissible
-- Need to transmit the number
-- of implicit (or extra-process) threads inside the mutex region. 
instance Control.Parallel.Strategies.NFData CInt where
instance Trans CInt where
instance NFData.NFData CInt where
--instance Eden.Trans CInt where
#endif

-- this MO means "undefined"
nullMO :: MuPadObject
nullMO = MAR ""

-- this MO means the "0" constant
-- may be safe traffic if you get it as constant.
nullPO :: MuPadPoly
nullPO = MAR "\"\"!"


---------------------------------------------------------------------------
-- @node MuPad calls, High level conversion routines, Classes and Instances
-- @section MuPad calls
---------------------------------------------------------------------------

-- ToDo:
--  . define a set of exceptions for possible errors on MuPad side


-- Implements a MuPad call;
-- name is a string; args anything MuPadCallable
-- Note how we enter the monad on a external context (FFI)
-- to grab the pipe handles.
-- @cindex mupadEval
mupadEval :: (MuPadCallable a) => String -> [a] -> MuPadObject
mupadEval name args = 
  unsafePerformIO $
  do
    inpipep <- creadInPipePointer
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    mupadEvalM inpipe outpipe name args

-- pure, monadic version of the MuPad call
-- Take care of mylock call in order to implement
-- a mutex region
mupadEvalM :: (MuPadCallable a) => Handle -> Handle -> String -> [a] -> IO MuPadObject
mupadEvalM inpipe outpipe name args =                 -- <mupad fct> <list of mupad args>
  do
#if defined(DEBUG)
    hPutStrLn stderr ("-> mupadEvalM" ++ show name ++ " args ->")
    hFlush stderr
#endif
    let 
      n = length args                  -- number of args to mupad fct call
      mos = map toMuPadObject args     -- convert all args to MuPadObjects
    mylock                    `demanding` Control.Parallel.Strategies.rnf mos 
    mupadStartEval outpipe n 
    mupadEvalCore outpipe name mos           -- write fct and arg names
    mupadEndEval			     -- write call-epilogue
    mupadEvalWait inpipe                     -- wait for result
    mstr <- mupadResult inpipe		     -- grab result
    myunlock `demanding` Control.Parallel.Strategies.rnf mstr
    return (MAR mstr)                        -- and pull into Haskell-land

-- Implements a MuPad call; returns a list of results
-- @cindex mupadEvalN
mupadEvalN :: (MuPadCallable a) => String -> [a] -> [MuPadObject]
mupadEvalN name args = 
  unsafePerformIO $ 
  do 
    inpipep <- creadInPipePointer 
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    mupadEvalNM inpipe outpipe name args

-- pure, monadic version of the MuPad call
-- Note the use of Control.Parallel.Strategies.rnf to require
-- reduced normal form (rnf), since the weak head normal forg (whnf)
-- may cause problems in an interactive-monadic context
-- @cindex mupadEvalNM
{-# INLINE mupadEvalNM #-}
mupadEvalNM :: (MuPadCallable a) => Handle -> Handle -> String -> [a] -> IO [MuPadObject]
mupadEvalNM inpipe outpipe name args =
  do 
    let 
      n = length args                  -- number of args to mupad fct call
      mos = map toMuPadObject args     -- convert all args to MuPadObjects
    mylock        `demanding` Control.Parallel.Strategies.rnf mos 
    mupadStartEvalN outpipe n
    mupadEvalCore outpipe name mos          -- write fct and arg names
    mupadEndEval			    -- write call-epilogue
    mupadEvalWait inpipe                    -- wait for result
    m <- mupadResultN inpipe		    -- grab result
    l <- mapM ( \ _ -> do
                  mstr <- mupadNextResult inpipe   -- get next result
		  let x = (MAR mstr)
                  return x `demanding` Control.Parallel.Strategies.rnf x) [1..m]  
    myunlock	  `demanding` Control.Parallel.Strategies.rnf l
    return l				--  list elem!


---------------------------------------------------------------------------
-- @node High level conversion routines, Communication with MuPad process, MuPad calls
-- @section High level conversion routines
---------------------------------------------------------------------------

-- a -> MuPadObject

-- convert a Haskell Int to a MuPadObject
-- @cindex int2MuPadObject
int2MuPadObject :: Int -> MuPadObject
int2MuPadObject = unsafePerformIO . int2MuPadObjectM

-- Pure, monadic version of previous
-- Note how we enter the monada on a external context (FFI)
-- to grab the pipe.
-- @cindex int2MuPadObjectM
int2MuPadObjectM :: Int -> IO MuPadObject
int2MuPadObjectM n = 
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mupadInt inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n
 


-- convert a Haskell arbitrary precision integer to a MuPadObject
-- @cindex integer2MuPadObjectM
integer2MuPadObject :: Integer -> MuPadObject
integer2MuPadObject = unsafePerformIO . integer2MuPadObjectM

-- Pure, monadic version of previous
-- ditto
-- @cindex integer2MuPadObjectM
integer2MuPadObjectM :: Integer -> IO MuPadObject
integer2MuPadObjectM n =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mupadInteger inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n

-- convert a Haskell Char to a MuPadObject
-- @cindex char2MuPadObject
char2MuPadObject :: Char -> MuPadObject
char2MuPadObject = unsafePerformIO . char2MuPadObjectM


-- Pure, monadic version of previous
-- Note how we enter the monad
-- to grab the pipe from external context (FFI)
-- @cindex char2MuPadObjectM
char2MuPadObjectM :: Char -> IO MuPadObject
char2MuPadObjectM c =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mupadChar inpipe outpipe c `demanding` Control.Parallel.Strategies.rnf c



-- convert a MuPadObject to a Haskell String
-- @cindex string2MuPadExpr
string2MuPadExpr :: String -> MuPadObject
string2MuPadExpr = unsafePerformIO . string2MuPadExprM

-- Pure, monadic version of previous
-- Ditto
-- @cindex string2MuPadExprM
string2MuPadExprM :: String -> IO MuPadObject
string2MuPadExprM str = 
  do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mupadExpr inpipe outpipe str `demanding` Control.Parallel.Strategies.rnf str


-- convert a Haskell String to a MuPad String 
-- @cindex string2MuPadObject
string2MuPadObject :: String -> MuPadObject
string2MuPadObject x = string2MuPadExpr ("\"" ++ x ++ "\"")

-- convert a Haskell String to a MuPad name ( not an expression! )
-- Usually a name in MuPad is used as a way
-- to implement "delayed " evaluation mode (lazy?). 
-- ( read MuPad User's Guide )
-- @cindex string2MuPadName
string2MuPadName x = string2MuPadExpr ("`" ++ x ++ "`")



--  MuPadObject -> a


-- convert a MuPadObject to a Haskell Int
-- Ditto.
-- @cindex mupadObject2Int
mupadObject2Int :: MuPadObject -> Int
mupadObject2Int mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInt inpipe outpipe mo


-- convert a MuPadObject to a Haskell arbitrary precision Integer
-- Ditto.
-- @cindex mupadObject2Integer
mupadObject2Integer :: MuPadObject -> Integer
mupadObject2Integer mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInteger inpipe outpipe mo


-- convert a MuPadObject to a Haskell Char
-- ditto
-- @cindex mupadObject2Char
mupadObject2Char :: MuPadObject -> Char
mupadObject2Char mo =   unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellChar inpipe outpipe mo


-- convert a MuPadObject to a Haskell String
-- ditto
-- @cindex mupadObject2String
mupadObject2String :: MuPadObject -> String
mupadObject2String mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellString inpipe outpipe mo


---------------------------------------------------------------------------
-- @node Communication with MuPad process, Low level conversion routines, High level conversion routines
-- @section Communication with MuPad process
--
-- Writing a MuPadObject directly to the pipe connecting with the MuPad proc
-- 
-- This section has been dropped from the 0.1 version.
-- writeStringToFile, writeByteArrayToFile are just:
--
-- hPutStrLn handle mstr
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- @node Low level conversion routines, Operations on Stream of Bytes, Communication with MuPad process
-- @section Low level conversion routines
--
-- This section has been dropped from the 0.1 version.

-- mupadExtractChar_ret , mupadExtractInt_ret
-- are just
-- 
-- read (string)
-- mupadEextractMO is just the MAR Constructor.
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- @node Operations on Stream of Bytes, Lock/Unlock operations , Low level conversion routines
-- @section Operations on Stream of Bytes
--
-- Basic operations on the representation of MuPadObjects as ByteArrays
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
-- the pipe to MuPad
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
-- mupadEval :: (MuPadCallable a) => String -> [a] -> MuPadObject
--
-- instead of
--
-- mupadEval :: (MuPadCallable a) => MVar -> String -> [a] -> MuPadObject
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
isSaneMO :: MuPadObject -> Bool
isSaneMO (MAR str) = and (map ( \ c -> isAscii c && (not (isControl c))) str)
#endif
-}

-- length of the ByteArray
-- @cindex lengthMO
lengthMO   :: MuPadObject -> Int
lengthMO (MAR str) = length str

showMuPadObject :: MuPadObject -> [Char]
showMuPadObject mo@(MAR str) = "DBG> Size: " ++  (show (lengthMO mo)) ++ "; Contents: " ++ str


-------------------------------------------------------------------
-- @node Settting up/down interface operations, Basic IO operations, Lock/Unlock operations 
-- @section Settting up/down interface operations
---------------------------------------------------------------------------


#ifdef __PARALLEL_HASKELL__
-- This is the famous patch to solve the replication of MuPad
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

-- mupadInitAllGum :: IO (Int) -- return how many PE's have been setup.
-- mupadInitAllGum = do   hPutStrLn stderr ("\t====== Setting up MuPAD ...." ++ show Eden.noPe ++ " ")
-- 		       peNos <- mapM (\pe -> instantiateAt pe (process mupadInitLocalGum) pe ) [1..Eden.noPe] 
-- 		       rnf peNos `seq` 
-- 			   return (length peNos)

mupadInitAllGum :: IO (Int) -- return how many PE's have been setup.
mupadInitAllGum = do
		     hPutStrLn stderr ("\t====== Setting up MuPad around ...."++ show Eden.noPe ++ " ")
                     let a = [process (mupadInitLocalGum) # (i) | i <- [1..Eden.noPe] ] `using` spine
		     return (length a) `demanding` Control.Parallel.Strategies.rnf a

-- (Should) return the id for the PE in GUM.
mupadInitLocalGum :: Int -> Int
mupadInitLocalGum = unsafePerformIO . mupadInitLocalGumM

mupadInitLocalGumM :: Int -> IO (Int) -- return the number of the PE just setup
mupadInitLocalGumM i = 
#else
mupadInit :: IO ()
mupadInit =
#endif
 do
#ifdef __PARALLEL_HASKELL__
   -- These signal handlers seem not to bet processed by Eden
   -- Reason: I guess PVM Signaling system overwrites Eden's.
#else
   -- install signal handler to terminate MuPad process properly 
   installHandler sigTERM (Catch (mupadTerm)) Nothing
   installHandler sigQUIT (Catch (mupadTerm)) Nothing
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
 		     executeFile MUPADBIN False [MUPADARGS] Nothing
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
		      mupadRoot <- System.Environment.getEnv "casROOTEden"
		      -- mupad read this file at the start .mupadpro40/userinit.mu ; no need for init.mupad 
-- 		      hPutStrLn outpipe ("read `" ++ mupadRoot ++ "/init.mupad`:")
		      -- start.mupad is increpted in the starting call; no need for start.mupad
-- 		      hPutStrLn outpipe  ("read `"++ mupadRoot ++ "/start.mupad`:")
 		      hPutStrLn outpipe  ("ready")
		      -- make termination sure
		      -- wait for process to be ready
		      a <- hGetLine inpipe
#if defined(DEBUG)
		      putStrLn ("\tLaunching pid...: \t" ++ (show pid))
		      putStrLn ("\tMUPAD SERVER :... " ++ a)
#endif
		      -- first line in protocol is name of debug file 
		      hPutStrLn outpipe (mupadRoot ++ "/error." ++ show(pid))
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
-- Same as mupadInitAllGum (read above)
-- You must call this function the last
-- on your main program
-- when no more processes are remaining
-- to asure 1:1 process:PE
-- Otherwise, concurrent process inside a PE
-- may become a chaos.
mupadTermAllGum :: IO(Int) -- return how many PE's have been set down.
mupadTermAllGum = do
 		      hPutStrLn stderr "\t====== Setting down MuPAD ...." 
 		      peNos <- mapM (\pe -> instantiateAt pe (process mupadTermLocalGum) pe ) [1..Eden.noPe] 
 		      rnf peNos `seq` 
 		   	 return (length peNos)

-- mupadTermAllGum :: IO(Int) -- return how many PE's have been set down.
-- mupadTermAllGum = do
-- 		      hPutStrLn stderr ("\t====== Setting down MuPad around GUM...." ++ show Eden.noPe ++ " ")
-- 		      let a = [process (mupadTermLocalGum) # (i) | i <- [1..Eden.noPe] ]  `using` spine
-- 		      return (length a) `demanding` Control.Parallel.Strategies.rnf a

--
-- A hack to evaluate the list eagarly
--
spine :: [a] -> ()
spine [] = ()
spine (x:xs) = spine xs

-- (Should) return the Id of the PE.
mupadTermLocalGum :: Int -> Int
mupadTermLocalGum = unsafePerformIO . mupadTermLocalGumM


-- Monadic version:
-- Killing the server process.		   
mupadTermLocalGumM :: Int -> IO(Int)
mupadTermLocalGumM i = 
#else
mupadTerm :: IO()
mupadTerm =
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
   putStrLn ("\tMUPAD SERVER :... terminate")
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
readMuPad :: Handle -> IO (String)
readMuPad hdl = do
	         line <- hGetLine hdl
		 return (line)

-------------------------------------------------------------------------------
--
-- Read the Header of a HM message.
--
-- Panic error: When the MuPad server sends other thing
-- than an Integer, the system will abort dirtly
-- (as Haskell Prelude is designed to)
-- Be warned if you are debugging !! 
-------------------------------------------------------------------------------
readMuPadCode :: Handle -> IO(Int)
readMuPadCode hdl = do
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
mupadStartEval :: Handle -> Int -> IO()
mupadStartEval outpipe  n =
  do
    hPutStrLn outpipe "fun"
    hPutStrLn outpipe (show n)
    return ()

-- wait for MuPad Peer answer to be available.
-- Since the subsequent IO-call -hGetLine-
-- operates on a  Handle -not a Fd-, 
-- the RTS prevents to block the rest of the threads.
-- So, we Keep it here for historical reasons.
mupadEvalWait :: Handle -> IO ()
mupadEvalWait inpipe =  return ()



-- This writes the fct name and the args (must have been converted to 
-- MuPadObjects already) to the given handle (the pipe).
-- @cindex mupadEvalCore
{-# INLINE mupadEvalCore #-}
mupadEvalCore :: Handle -> String -> [MuPadObject] -> IO ()
mupadEvalCore hdl name args =
  do 
    hPutStrLn hdl name
    mapM (\ arg -> (writeToMuPad hdl arg)) args
    return ()




------------------------------------------------------------------------------
-- mupadResult
-- Reads the whole HM Message ( Header plus Body )
-- and lifts the body part
------------------------------------------------------------------------- 
mupadResult :: Handle -> IO (String)
mupadResult  inpipe = 
 do
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   if (header/=0) 
      then
        do 
	  report "error in MuPad function" body
	  return  body 
      else return  body 

----------------------------------------------------------------------------
-- end evaluation
-- In 0.1 version it used to commit the (fflush)
-- but in 0.2 automatic flush is achieved automatically when
-- a line is entered.
-- Kept here for historical reasons.
 ------------------------------------------------------------------------- */
mupadEndEval :: IO ()
mupadEndEval = return ()  




 ----------------------------------------------------------------------------
 -- start evaluation with n arguments and sequence of results
 --
 ------------------------------------------------------------------------- 
mupadStartEvalN :: Handle -> Int -> IO()
mupadStartEvalN outpipe  n =
  do
    hPutStrLn outpipe "lst"
    hPutStrLn outpipe (show n)
    return()


------------------------------------------------------------------------------
-- mupadResultN
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
mupadResultN :: Handle -> IO(Int)
mupadResultN inpipe=
 do
   header <- readMuPadCode inpipe
   if (header<0)  -- Error in service, just log it.
     then
       do
	 body <- readMuPad inpipe   
	 report "error in MuPad function" body
         return (header)
     else   return (header)  
   
 ------------------------------------------------------------------------------
 -- mupadNextResult
 -- Assumed handle ready,
 -- reads subsequent body calls.
 ------------------------------------------------------------------------- */
mupadNextResult :: Handle -> IO (String)
mupadNextResult  inpipe = 
 do
   partialBody <- readMuPad inpipe
   return ( partialBody) 


-------------------------------------------------------------------
-- @node    Conversion Routines, , Evaluation routines,
-- @section Debugging routines
---------------------------------------------------------------------------
-- Protocol HM works in this way.
-- 1.- Sending header (in,out)
-- 2.- Sending body "[4,4,x^2]"
-- 3.- Receiving header (0,-1...)
-- 4.- Receiving body (MuPadObject)
--
-- WARNING: TODO: Exception on MuPad side treatment.

----------------------------------------------------------------------------
-- convert Haskell string to MuPad object
-- ------------------------------------------------------------------------- */
mupadExpr:: Handle -> Handle -> String -> IO (MuPadObject)
mupadExpr inpipe outpipe expr =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe expr
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
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
-- convert Haskell int to MuPad integer
-- ditto.
------------------------------------------------------------------------- */

mupadInt::  Handle -> Handle -> Int -> IO (MuPadObject)
mupadInt inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Int" body
       return  (MAR body)
    else
       return  (MAR body)



----------------------------------------------------------------------------
-- convert an arbitrary Haskell integer to MuPad integer
-- ditto
------------------------------------------------------------------------- */
mupadInteger::  Handle -> Handle -> Integer -> IO (MuPadObject)
mupadInteger inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Integer" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert an arbitrary Haskell char to MuPad char
-- ditto
------------------------------------------------------------------------- */
mupadChar::  Handle -> Handle -> Char -> IO (MuPadObject)
mupadChar inpipe outpipe char =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show char)
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header/=0)
    then
      do
       report "error in converting Haskell Char" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert MuPad Object to Haskell string
--
--------------------------------------------------------------------------- */
haskellString ::Handle -> Handle -> MuPadObject -> IO(String)
haskellString inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting MuPad String" body
       return  (body)
    else
       return  (body)


----------------------------------------------------------------------------
-- convert MuPad string to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInt ::Handle -> Handle -> MuPadObject -> IO(Int)
haskellInt inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting MuPad Int" body
       return  (read body)
    else
       return  (read body)

 ----------------------------------------------------------------------------
-- convert MuPad Integer to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInteger ::Handle -> Handle -> MuPadObject -> IO(Integer)
haskellInteger inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting MuPad Int" body
       return  (read body)
    else
       return  (read body)

----------------------------------------------------------------------------
-- convert MuPad Char to Haskell string
--
------------------------------------------------------------------------- */
haskellChar ::Handle -> Handle -> MuPadObject -> IO(Char)
haskellChar inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMuPadCode inpipe
   body <- readMuPad inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting MuPad Int" body
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
-- we are wasting information from MuPad messages.
-- info is usually a formatted string.
------------------------------------------------------------------------- */
report:: String -> String -> IO()
report key info = do
		   hPutStrLn stderr ("mupad interface error: "++key++"("++info++")")
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
