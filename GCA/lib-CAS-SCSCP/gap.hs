----------------------------------------------------------------------------
-- GHC interface to Gap (including Eden extensions)
--
-- Version 0.2
--
-- (c) 2006
--     Abdallah Al Zain <ceeatia@macs.hw.ac.uk>
--     
--
-- Based on Haskell-Interface
--
-- (c) 2000
--     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
--     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
--
----------------------------------------------------------------------------

module GapAPI
  (GapObject,                      -- Haskell representation of Gap things
   GapPoly,GapCoeffs,GapVar,GapInt, -- Renaming Types
   GapCallable(..),			    -- Class 
   nullMO,				    -- The NULL Gap-object
   nullPO,				    -- The 0 Gap-Object
   gapEval, gapEvalN,            -- evaluating a Gap function
                                     -- conversion functions for basic types:
   string2GapName, 
   string2GapExpr,
   int2GapObject, gapObject2Int,
   integer2GapObject, gapObject2Integer,
   char2GapObject, gapObject2Char,
   string2GapObject, gapObject2String, 
   showGapObject,			   -- debugging only:
#ifdef __PARALLEL_HASKELL__
   gapInitAllGum, gapTermAllGum ) where -- the initializing  calls for the distributed interface across the GUM
#else
   gapInit , gapTerm) where		   -- ditto for the only STG
#endif

/* version string used for communication with Gap, see start file */
#define VERSION "HaskellGap 0.2"


-- @menu
-- * Imports::			
-- * Interface Specification::	
-- * GapObject Type::		
-- * Classes and Instances::	
-- * Gap calls::		
-- * High level conversion routines::  
-- * Communication with Gap process::	 
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




-- @node Interface Specification, GapObject Type, Imports
-- @section Interface Specification

---------------------------------------------------------------------------
-- The interface consists of the following functions
--
-- * set up/down the gap interface
--   
--   gapInitPE  / gapInit
--   gapTermPE  / gapTerm
--
-- * call a gap function "fun"
--
--    r  = gapEval  "fun" [ a ]
--   [r] = gapEvalN "fun" [ a ]
--
--    a: Int, Char, String, GapObject
--    r: GapObject
--
-- if Gap functions are called multiple times with same arguments,
-- it is more efficient to convert Int, Char, String to  GapObject before
--
-- * conversion functions
--
--   m =    int2GapObject i, i = gapObject2Int    m
--   m =   char2GapObject c, c = gapObject2Char   m
--   m = string2GapObject s, s = gapObject2String m
--   m = string2GapName s
--   m = string2GapExpr s
--
--   m: GapObject, I: i: Int, c: Char, s: String
---------------------------------------------------------------------------

-- @node GapObject Type, Classes and Instances, Interface Specification
-- @section GapObject Type

---------------------------------------------------------------------------
-- GapObjects are "chunks" coming back from calls
-- to Gap routines as a stream of bytes.
--
-- According to HM "Haskell-Gap" protocol
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
--	    |  in    |	Ask Gap for coding contents in Body		|
--	    |  out   |		      decoding				|
--	    |  fun n |	Ask Gap for function evaluation, Bod. are pp	|
--	    |  lst n |		ditto, for multiple results		|
--	    |  0     |	Ok from Gap, Body holds answer		|
--	    |  <0    |	NOK. Body holds reason				|
--	    |  >0    |	Ok: Body holds n results			|
--	    ------------------------------------------------------------
--
--
-- Body:
-- The basis for the GapObject constructor has been
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
data GapObject = MAR String 

-- Some Gap usefull types aliases.
type GapPoly   = GapObject
type GapCoeffs = GapObject
type GapVar    = GapObject
type GapInt    = GapObject


-- @node Classes and Instances, Gap calls, GapObject Type
-- @section Classes and Instances

-- class of things that can be passed to Gap
-- @cindex GapCallable
class GapCallable a where
  -- convert this Haskell object into a GapObject  
  toGapObject :: a -> GapObject
  -- and the other direction, too	
  fromGapObject :: GapObject -> a
  -- write this Gap object to a file	
  writeToGap :: Handle {-Handle-} -> a -> IO ()

-- a Haskell fixed precision integer
instance GapCallable Int where
  toGapObject = int2GapObject
  fromGapObject = gapObject2Int
  writeToGap hdl i = hPutStrLn hdl (show i)

-- a Haskell arbitrary precision integer
instance GapCallable Integer where
  toGapObject = integer2GapObject
  fromGapObject = gapObject2Integer
  writeToGap hdl i = hPutStrLn hdl (show i)

-- a Haskell character
instance GapCallable Char where
  toGapObject = char2GapObject
  fromGapObject = gapObject2Char
  writeToGap hdl c = hPutStrLn hdl (show c)

-- a Haskell string
instance GapCallable String where
  toGapObject = string2GapObject
  fromGapObject = gapObject2String
  writeToGap  = hPutStrLn 

-- a Gap object
instance GapCallable GapObject where
  toGapObject = id
  fromGapObject = id
  writeToGap hdl (MAR str) = hPutStrLn hdl str

-- a GapObject can be shown 
instance Show GapObject where
  show = showGapObject

-- a GapObject can be compared
instance Eq GapObject where
  MAR str == MAR str' = str == str'

-- a GapObject can be reduced to normal form.
instance Control.Parallel.Strategies.NFData GapObject where
  rnf (MAR str) = (rnf str) 

#ifdef __PARALLEL_HASKELL__
instance NFData.NFData GapObject where
    rnf (MAR str) = (rnf str) 

instance Trans GapObject where
-- A CType (see FFI) must be transmissible
-- Need to transmit the number
-- of implicit (or extra-process) threads inside the mutex region. 
instance Control.Parallel.Strategies.NFData CInt where
instance Trans CInt where
instance NFData.NFData CInt where
--instance Eden.Trans CInt where
#endif

-- this MO means "undefined"
nullMO :: GapObject
nullMO = MAR ""

-- this MO means the "0" constant
-- may be safe traffic if you get it as constant.
nullPO :: GapPoly
nullPO = MAR "\"\"!"


---------------------------------------------------------------------------
-- @node Gap calls, High level conversion routines, Classes and Instances
-- @section Gap calls
---------------------------------------------------------------------------

-- ToDo:
--  . define a set of exceptions for possible errors on Gap side


-- Implements a Gap call;
-- name is a string; args anything GapCallable
-- Note how we enter the monad on a external context (FFI)
-- to grab the pipe handles.
-- @cindex gapEval
gapEval :: (GapCallable a) => String -> [a] -> GapObject
gapEval name args = 
  unsafePerformIO $
  do
    inpipep <- creadInPipePointer
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    gapEvalM inpipe outpipe name args

-- pure, monadic version of the Gap call
-- Take care of mylock call in order to implement
-- a mutex region
gapEvalM :: (GapCallable a) => Handle -> Handle -> String -> [a] -> IO GapObject
gapEvalM inpipe outpipe name args =                 -- <gap fct> <list of gap args>
  do
#if defined(DEBUG)
    hPutStrLn stderr ("-> gapEvalM" ++ show name ++ "")
    hFlush stderr
#endif
    let 
      n = length args                  -- number of args to gap fct call
      mos = map toGapObject args     -- convert all args to GapObjects
    mylock                    `demanding` Control.Parallel.Strategies.rnf mos 
    gapStartEval outpipe n 
    gapEvalCore outpipe name mos           -- write fct and arg names
    gapEndEval			     -- write call-epilogue
    gapEvalWait inpipe                     -- wait for result
    mstr <- gapResult inpipe		     -- grab result
    myunlock `demanding` Control.Parallel.Strategies.rnf mstr
    return (MAR mstr)                        -- and pull into Haskell-land

-- Implements a Gap call; returns a list of results
-- @cindex gapEvalN
gapEvalN :: (GapCallable a) => String -> [a] -> [GapObject]
gapEvalN name args = 
  unsafePerformIO $ 
  do 
    inpipep <- creadInPipePointer 
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    gapEvalNM inpipe outpipe name args

-- pure, monadic version of the Gap call
-- Note the use of rnf to require
-- reduced normal form (rnf), since the weak head normal forg (whnf)
-- may cause problems in an interactive-monadic context
-- @cindex gapEvalNM
{-# INLINE gapEvalNM #-}
gapEvalNM :: (GapCallable a) => Handle -> Handle -> String -> [a] -> IO [GapObject]
gapEvalNM inpipe outpipe name args =
  do 
    let 
      n = length args                  -- number of args to gap fct call
      mos = map toGapObject args     -- convert all args to GapObjects
    mylock        `demanding` Control.Parallel.Strategies.rnf mos 
    gapStartEvalN outpipe n
    gapEvalCore outpipe name mos          -- write fct and arg names
    gapEndEval			    -- write call-epilogue
    gapEvalWait inpipe                    -- wait for result
    m <- gapResultN inpipe		    -- grab result
    l <- mapM ( \ _ -> do
                  mstr <- gapNextResult inpipe   -- get next result
		  let x = (MAR mstr)
                  return x `demanding` Control.Parallel.Strategies.rnf x) [1..m]  
    myunlock	  `demanding` Control.Parallel.Strategies.rnf l
    return l				--  list elem!


---------------------------------------------------------------------------
-- @node High level conversion routines, Communication with Gap process, Gap calls
-- @section High level conversion routines
---------------------------------------------------------------------------

-- a -> GapObject

-- convert a Haskell Int to a GapObject
-- @cindex int2GapObject
int2GapObject :: Int -> GapObject
int2GapObject = unsafePerformIO . int2GapObjectM

-- Pure, monadic version of previous
-- Note how we enter the monada on a external context (FFI)
-- to grab the pipe.
-- @cindex int2GapObjectM
int2GapObjectM :: Int -> IO GapObject
int2GapObjectM n = 
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   gapInt inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n
 


-- convert a Haskell arbitrary precision integer to a GapObject
-- @cindex integer2GapObjectM
integer2GapObject :: Integer -> GapObject
integer2GapObject = unsafePerformIO . integer2GapObjectM

-- Pure, monadic version of previous
-- ditto
-- @cindex integer2GapObjectM
integer2GapObjectM :: Integer -> IO GapObject
integer2GapObjectM n =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   gapInteger inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n

-- convert a Haskell Char to a GapObject
-- @cindex char2GapObject
char2GapObject :: Char -> GapObject
char2GapObject = unsafePerformIO . char2GapObjectM


-- Pure, monadic version of previous
-- Note how we enter the monad
-- to grab the pipe from external context (FFI)
-- @cindex char2GapObjectM
char2GapObjectM :: Char -> IO GapObject
char2GapObjectM c =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   gapChar inpipe outpipe c `demanding` Control.Parallel.Strategies.rnf c



-- convert a GapObject to a Haskell String
-- @cindex string2GapExpr
string2GapExpr :: String -> GapObject
string2GapExpr = unsafePerformIO . string2GapExprM

-- Pure, monadic version of previous
-- Ditto
-- @cindex string2GapExprM
string2GapExprM :: String -> IO GapObject
string2GapExprM str = 
  do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   gapExpr inpipe outpipe str `demanding` Control.Parallel.Strategies.rnf str


-- convert a Haskell String to a Gap String 
-- @cindex string2GapObject
string2GapObject :: String -> GapObject
string2GapObject x = string2GapExpr ("\"" ++ x ++ "\"")

-- convert a Haskell String to a Gap name ( not an expression! )
-- Usually a name in Gap is used as a way
-- to implement "delayed " evaluation mode (lazy?). 
-- ( read Gap User's Guide )
-- @cindex string2GapName
string2GapName x = string2GapExpr ("`" ++ x ++ "`")



--  GapObject -> a


-- convert a GapObject to a Haskell Int
-- Ditto.
-- @cindex gapObject2Int
gapObject2Int :: GapObject -> Int
gapObject2Int mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInt inpipe outpipe mo


-- convert a GapObject to a Haskell arbitrary precision Integer
-- Ditto.
-- @cindex gapObject2Integer
gapObject2Integer :: GapObject -> Integer
gapObject2Integer mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInteger inpipe outpipe mo


-- convert a GapObject to a Haskell Char
-- ditto
-- @cindex gapObject2Char
gapObject2Char :: GapObject -> Char
gapObject2Char mo =   unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellChar inpipe outpipe mo


-- convert a GapObject to a Haskell String
-- ditto
-- @cindex gapObject2String
gapObject2String :: GapObject -> String
gapObject2String mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellString inpipe outpipe mo


---------------------------------------------------------------------------
-- @node Communication with Gap process, Low level conversion routines, High level conversion routines
-- @section Communication with Gap process
--
-- Writing a GapObject directly to the pipe connecting with the Gap proc
-- 
-- This section has been dropped from the 0.1 version.
-- writeStringToFile, writeByteArrayToFile are just:
--
-- hPutStrLn handle mstr
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- @node Low level conversion routines, Operations on Stream of Bytes, Communication with Gap process
-- @section Low level conversion routines
--
-- This section has been dropped from the 0.1 version.

-- gapExtractChar_ret , gapExtractInt_ret
-- are just
-- 
-- read (string)
-- gapEextractMO is just the MAR Constructor.
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- @node Operations on Stream of Bytes, Lock/Unlock operations , Low level conversion routines
-- @section Operations on Stream of Bytes
--
-- Basic operations on the representation of GapObjects as ByteArrays
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
-- the pipe to Gap
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
-- gapEval :: (GapCallable a) => String -> [a] -> GapObject
--
-- instead of
--
-- gapEval :: (GapCallable a) => MVar -> String -> [a] -> GapObject
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
isSaneMO :: GapObject -> Bool
isSaneMO (MAR str) = and (map ( \ c -> isAscii c && (not (isControl c))) str)
#endif
-}

-- length of the ByteArray
-- @cindex lengthMO
lengthMO   :: GapObject -> Int
lengthMO (MAR str) = length str

showGapObject :: GapObject -> [Char]
showGapObject mo@(MAR str) = "DBG> Size: " ++  (show (lengthMO mo)) ++ "; Contents: " ++ str


-------------------------------------------------------------------
-- @node Settting up/down interface operations, Basic IO operations, Lock/Unlock operations 
-- @section Settting up/down interface operations
---------------------------------------------------------------------------


#ifdef __PARALLEL_HASKELL__
-- This is the famous patch to solve the replication of Gap
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
-- gapInitAllGum :: IO (Int) -- return how many PE's have been setup.
-- gapInitAllGum = do   hPutStrLn stderr ("\t====== Setting up Gap around ...." ++ show Eden.noPe ++ " ")
--                        -- let a = [process (gapInitLocalGum) # (i) | i <- [1.. Eden.noPe] ] `using` spine
--   		     peNos <- mapM (\pe -> instantiateAt pe (process gapInitLocalGum) pe ) [1..Eden.noPe] 
--   		     return (length peNos) `demanding` Control.Parallel.Strategies.rnf peNos
--  			    Control.Parallel.Strategies.rnf peNos `seq` 
-- 		   	 return (length peNos)

gapInitAllGum :: IO (Int) -- return how many PE's have been setup.
gapInitAllGum = do
  		     hPutStrLn stderr ("\t====== Setting up Gap around ...." ++ show Eden.noPe ++ " ")
                     let a = [process (gapInitLocalGum) # (i) | i <- [1.. Eden.noPe] ] `using` spine
    		     return (length a) `demanding` Control.Parallel.Strategies.rnf a

-- (Should) return the id for the PE in GUM.
gapInitLocalGum :: Int -> Int
gapInitLocalGum = unsafePerformIO . gapInitLocalGumM

gapInitLocalGumM :: Int -> IO (Int) -- return the number of the PE just setup
gapInitLocalGumM i = 
#else
gapInit :: IO ()
gapInit =
#endif
 do
#ifdef __PARALLEL_HASKELL__
   -- These signal handlers seem not to bet processed by Eden
   -- Reason: I guess PVM Signaling system overwrites Eden's.
#else
   -- install signal handler to terminate Gap process properly 
   installHandler sigTERM (Catch (gapTerm)) Nothing
   installHandler sigQUIT (Catch (gapTerm)) Nothing
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
--    yield 
   case (mpid) of
     Nothing	-> do     -- redirect in subprocess stdin and stdout to pipes
	             dupTo outr stdInput
		     dupTo inw  stdOutput
		     executeFile GAPBIN False [GAPARGS] Nothing
		     error "cannot execute file"
#ifdef __PARALLEL_HASKELL__
		     return(i)
#else
		     return()
#endif
     Just pid	-> do  -- Just ProcessID
		      -- yield
		      threadDelay 100000
		      -- threadDelay 100000 -- Arbitrary time
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
		      gapRoot <- System.Environment.getEnv "casROOTEdenSCSCP"
 		      hPutStrLn outpipe ("Read (\"" ++ gapRoot ++ "/initOM.g\");")
 		      hPutStrLn outpipe  ("Read (\""++ gapRoot ++ "/startOM.g\");")
		      yield
		      -- make termination sure
		      -- wait for process to be ready  
		      a <- hGetLine inpipe
#if defined(DEBUG)
		      putStrLn ("\tLaunching pid...: \t" ++ (show pid))
		      putStrLn ("\tGAP SERVER :... " ++ a)
#endif
		      -- first line in protocol is name of debug file 
--		      hPutStrLn outpipe (gapRoot ++ "/error." ++ show(pid))
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
-- Same as gapInitAllGum (read above)
-- You must call this function the last
-- on your main program
-- when no more processes are remaining
-- to asure 1:1 process:PE
-- Otherwise, concurrent process inside a PE
-- may become a chaos.
gapTermAllGum :: IO(Int) -- return how many PE's have been set down.
gapTermAllGum = do
		      hPutStrLn stderr "\t====== Setting down Gap ...." 
		      --let a = [process (gapTermLocalGum) # (i) | i <- [1.. Eden.noPe] ]  `using` spine
		      peNos <- mapM (\pe -> instantiateAt pe (process gapTermLocalGum) pe ) [1..Eden.noPe] 
		      rnf peNos `seq` 
		   	 return (length peNos) -- `demanding` Control.Parallel.Strategies.rnf a

-- gapTermAllGum :: IO(Int) -- return how many PE's have been set down.
-- gapTermAllGum = do
-- 		      hPutStrLn stderr "\t====== Setting down Gap around GUM...." 
-- 		      let a = [process (gapTermLocalGum) # (i) | i <- [1.. Eden.noPe] ]  `using` spine
-- 		      return (length a) `demanding` Control.Parallel.Strategies.rnf a

--
-- A hack to evaluate the list eagarly
--
spine :: [a] -> ()
spine [] = ()
spine (x:xs) = spine xs

-- (Should) return the Id of the PE.
gapTermLocalGum :: Int -> Int
gapTermLocalGum = unsafePerformIO . gapTermLocalGumM


-- Monadic version:
-- Killing the server process.		   
gapTermLocalGumM :: Int -> IO(Int)
gapTermLocalGumM i = 
#else
gapTerm :: IO()
gapTerm =
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
   putStrLn ("\tGAP SERVER :... terminate")
#endif
   signalProcess sigTERM pid
   signalProcess sigKILL pid
--    signalProcess sigTERM (pid+1)
--    signalProcess sigKILL (pid+1)

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
readGap :: Handle -> IO (String)
readGap hdl = do
	         line <- hGetLine hdl
		 return (line)

-------------------------------------------------------------------------------
--
-- Read the Header of a HM message.
--
-- Panic error: When the Gap server sends other thing
-- than an Integer, the system will abort dirtly
-- (as Haskell Prelude is designed to)
-- Be warned if you are debugging !! 
-------------------------------------------------------------------------------
readGapCode :: Handle -> IO(Int)
readGapCode hdl = do
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
gapStartEval :: Handle -> Int -> IO()
gapStartEval outpipe  n =
  do
    hPutStrLn outpipe "fun"
    hPutStrLn outpipe (show n)
    return ()

-- wait for Gap Peer answer to be available.
-- Since the subsequent IO-call -hGetLine-
-- operates on a  Handle -not a Fd-, 
-- the RTS prevents to block the rest of the threads.
-- So, we Keep it here for historical reasons.
gapEvalWait :: Handle -> IO ()
gapEvalWait inpipe =  return ()



-- This writes the fct name and the args (must have been converted to 
-- GapObjects already) to the given handle (the pipe).
-- @cindex gapEvalCore
{-# INLINE gapEvalCore #-}
gapEvalCore :: Handle -> String -> [GapObject] -> IO ()
gapEvalCore hdl name args =
  do 
    hPutStrLn hdl name
    mapM (\ arg -> (writeToGap hdl arg)) args
    return ()




------------------------------------------------------------------------------
-- gapResult
-- Reads the whole HM Message ( Header plus Body )
-- and lifts the body part
------------------------------------------------------------------------- 
gapResult :: Handle -> IO (String)
gapResult  inpipe = 
 do
   header <- readGapCode inpipe
   body <- readGap inpipe
   if (header/=0) 
      then
        do 
	  report "error in Gap function" body
	  return  body 
      else return  body 

----------------------------------------------------------------------------
-- end evaluation
-- In 0.1 version it used to commit the (fflush)
-- but in 0.2 automatic flush is achieved automatically when
-- a line is entered.
-- Kept here for historical reasons.
 ------------------------------------------------------------------------- */
gapEndEval :: IO ()
gapEndEval = return ()  




 ----------------------------------------------------------------------------
 -- start evaluation with n arguments and sequence of results
 --
 ------------------------------------------------------------------------- 
gapStartEvalN :: Handle -> Int -> IO()
gapStartEvalN outpipe  n =
  do
    hPutStrLn outpipe "lst"
    hPutStrLn outpipe (show n)
    return()


------------------------------------------------------------------------------
-- gapResultN
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
gapResultN :: Handle -> IO(Int)
gapResultN inpipe=
 do
   header <- readGapCode inpipe
   if (header<0)  -- Error in service, just log it.
     then
       do
	 body <- readGap inpipe   
	 report "error in Gap function" body
         return (header)
     else   return (header)  
   
 ------------------------------------------------------------------------------
 -- gapNextResult
 -- Assumed handle ready,
 -- reads subsequent body calls.
 ------------------------------------------------------------------------- */
gapNextResult :: Handle -> IO (String)
gapNextResult  inpipe = 
 do
   partialBody <- readGap inpipe
   return ( partialBody) 


-------------------------------------------------------------------
-- @node    Conversion Routines, , Evaluation routines,
-- @section Debugging routines
---------------------------------------------------------------------------
-- Protocol HM works in this way.
-- 1.- Sending header (in,out)
-- 2.- Sending body "[4,4,x^2]"
-- 3.- Receiving header (0,-1...)
-- 4.- Receiving body (GapObject)
--
-- WARNING: TODO: Exception on Gap side treatment.

----------------------------------------------------------------------------
-- convert Haskell string to Gap object
-- ------------------------------------------------------------------------- */
gapExpr:: Handle -> Handle -> String -> IO (GapObject)
gapExpr inpipe outpipe expr =
 do
#if defined(DEBUG)
   hPutStrLn stderr ("-> gapExpr expr" ++ show expr ++ "")
   hFlush stderr
#endif
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe expr
   header <- readGapCode inpipe
   body <- readGap inpipe
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
-- convert Haskell int to Gap integer
-- ditto.
------------------------------------------------------------------------- */

gapInt::  Handle -> Handle -> Int -> IO (GapObject)
gapInt inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Int" body
       return  (MAR body)
    else
       return  (MAR body)



----------------------------------------------------------------------------
-- convert an arbitrary Haskell integer to Gap integer
-- ditto
------------------------------------------------------------------------- */
gapInteger::  Handle -> Handle -> Integer -> IO (GapObject)
gapInteger inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Integer" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert an arbitrary Haskell char to Gap char
-- ditto
------------------------------------------------------------------------- */
gapChar::  Handle -> Handle -> Char -> IO (GapObject)
gapChar inpipe outpipe char =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show char)
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header/=0)
    then
      do
       report "error in converting Haskell Char" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert Gap Object to Haskell string
--
--------------------------------------------------------------------------- */
haskellString ::Handle -> Handle -> GapObject -> IO(String)
haskellString inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Gap String" body
       return  (body)
    else
       return  (body)


----------------------------------------------------------------------------
-- convert Gap string to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInt ::Handle -> Handle -> GapObject -> IO(Int)
haskellInt inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Gap Int" body
       return  (read body)
    else
       return  (read body)

 ----------------------------------------------------------------------------
-- convert Gap Integer to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInteger ::Handle -> Handle -> GapObject -> IO(Integer)
haskellInteger inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Gap Int" body
       return  (read body)
    else
       return  (read body)

----------------------------------------------------------------------------
-- convert Gap Char to Haskell string
--
------------------------------------------------------------------------- */
haskellChar ::Handle -> Handle -> GapObject -> IO(Char)
haskellChar inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readGapCode inpipe
   body <- readGap inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Gap Int" body
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
-- we are wasting information from Gap messages.
-- info is usually a formatted string.
------------------------------------------------------------------------- */
report:: String -> String -> IO()
report key info = do
		   hPutStrLn stderr ("gap interface error: "++key++"("++info++")")
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
