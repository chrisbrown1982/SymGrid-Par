----------------------------------------------------------------------------
-- GHC interface to Maple (including Eden extensions)
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

module MapleAPI
  (MapleObject,                      -- Haskell representation of Maple things
   MaplePoly,MapleCoeffs,MapleVar,MapleInt, -- Renaming Types
   MapleCallable(..),			    -- Class 
   nullMO,				    -- The NULL Maple-object
   nullPO,				    -- The 0 Maple-Object
   mapleEval, mapleEvalN,            -- evaluating a Maple function
                                     -- conversion functions for basic types:
   string2MapleName, 
   string2MapleExpr,
   int2MapleObject, mapleObject2Int,
   integer2MapleObject, mapleObject2Integer,
   char2MapleObject, mapleObject2Char,
   string2MapleObject, mapleObject2String, 
   showMapleObject,			   -- debugging only:
#ifdef __PARALLEL_HASKELL__
   mapleInitAllGum, mapleTermAllGum ) where -- the initializing  calls for the distributed interface across the GUM
#else
   mapleInit , mapleTerm) where		   -- ditto for the only STG
#endif

/* version string used for communication with Maple, see start file */
#define VERSION "HaskellMaple 0.2"


-- @menu
-- * Imports::			
-- * Interface Specification::	
-- * MapleObject Type::		
-- * Classes and Instances::	
-- * Maple calls::		
-- * High level conversion routines::  
-- * Communication with Maple process::	 
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



-- @node Interface Specification, MapleObject Type, Imports
-- @section Interface Specification

---------------------------------------------------------------------------
-- The interface consists of the following functions
--
-- * set up/down the maple interface
--   
--   mapleInitPE  / mapleInit
--   mapleTermPE  / mapleTerm
--
-- * call a maple function "fun"
--
--    r  = mapleEval  "fun" [ a ]
--   [r] = mapleEvalN "fun" [ a ]
--
--    a: Int, Char, String, MapleObject
--    r: MapleObject
--
-- if Maple functions are called multiple times with same arguments,
-- it is more efficient to convert Int, Char, String to  MapleObject before
--
-- * conversion functions
--
--   m =    int2MapleObject i, i = mapleObject2Int    m
--   m =   char2MapleObject c, c = mapleObject2Char   m
--   m = string2MapleObject s, s = mapleObject2String m
--   m = string2MapleName s
--   m = string2MapleExpr s
--
--   m: MapleObject, I: i: Int, c: Char, s: String
---------------------------------------------------------------------------

-- @node MapleObject Type, Classes and Instances, Interface Specification
-- @section MapleObject Type

---------------------------------------------------------------------------
-- MapleObjects are "chunks" coming back from calls
-- to Maple routines as a stream of bytes.
--
-- According to HM "Haskell-Maple" protocol
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
--	    |  in    |	Ask Maple for coding contents in Body		|
--	    |  out   |		      decoding				|
--	    |  fun n |	Ask Maple for function evaluation, Bod. are pp	|
--	    |  lst n |		ditto, for multiple results		|
--	    |  0     |	Ok from Maple, Body holds answer		|
--	    |  <0    |	NOK. Body holds reason				|
--	    |  >0    |	Ok: Body holds n results			|
--	    ------------------------------------------------------------
--
--
-- Body:
-- The basis for the MapleObject constructor has been
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
data MapleObject = MAR String 

-- Some Maple usefull types aliases.
type MaplePoly   = MapleObject
type MapleCoeffs = MapleObject
type MapleVar    = MapleObject
type MapleInt    = MapleObject


-- @node Classes and Instances, Maple calls, MapleObject Type
-- @section Classes and Instances

-- class of things that can be passed to Maple
-- @cindex MapleCallable
class MapleCallable a where
  -- convert this Haskell object into a MapleObject  
  toMapleObject :: a -> MapleObject
  -- and the other direction, too	
  fromMapleObject :: MapleObject -> a
  -- write this Maple object to a file	
  writeToMaple :: Handle {-Handle-} -> a -> IO ()

-- a Haskell fixed precision integer
instance MapleCallable Int where
  toMapleObject = int2MapleObject
  fromMapleObject = mapleObject2Int
  writeToMaple hdl i = hPutStrLn hdl (show i)

-- a Haskell arbitrary precision integer
instance MapleCallable Integer where
  toMapleObject = integer2MapleObject
  fromMapleObject = mapleObject2Integer
  writeToMaple hdl i = hPutStrLn hdl (show i)

-- a Haskell character
instance MapleCallable Char where
  toMapleObject = char2MapleObject
  fromMapleObject = mapleObject2Char
  writeToMaple hdl c = hPutStrLn hdl (show c)

-- a Haskell string
instance MapleCallable String where
  toMapleObject = string2MapleObject
  fromMapleObject = mapleObject2String
  writeToMaple  = hPutStrLn 

-- a Maple object
instance MapleCallable MapleObject where
  toMapleObject = id
  fromMapleObject = id
  writeToMaple hdl (MAR str) = hPutStrLn hdl str

-- a MapleObject can be shown 
instance Show MapleObject where
  show = showMapleObject

-- a MapleObject can be compared
instance Eq MapleObject where
  MAR str == MAR str' = str == str'

-- a MapleObject can be reduced to normal form.
instance Control.Parallel.Strategies.NFData MapleObject where
  rnf (MAR str) = (rnf str) 

#ifdef __PARALLEL_HASKELL__
instance NFData.NFData MapleObject where
    rnf (MAR str) = (rnf str) 

instance Trans MapleObject where
-- A CType (see FFI) must be transmissible
-- Need to transmit the number
-- of implicit (or extra-process) threads inside the mutex region. 
instance Control.Parallel.Strategies.NFData CInt where
instance Trans CInt where
instance NFData.NFData CInt where
--instance Eden.Trans CInt where
#endif

-- this MO means "undefined"
nullMO :: MapleObject
nullMO = MAR ""

-- this MO means the "0" constant
-- may be safe traffic if you get it as constant.
nullPO :: MaplePoly
nullPO = MAR "\"\"!"


---------------------------------------------------------------------------
-- @node Maple calls, High level conversion routines, Classes and Instances
-- @section Maple calls
---------------------------------------------------------------------------

-- ToDo:
--  . define a set of exceptions for possible errors on Maple side


-- Implements a Maple call;
-- name is a string; args anything MapleCallable
-- Note how we enter the monad on a external context (FFI)
-- to grab the pipe handles.
-- @cindex mapleEval
mapleEval :: (MapleCallable a) => String -> [a] -> MapleObject
mapleEval name args = 
  unsafePerformIO $
  do
    inpipep <- creadInPipePointer
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    mapleEvalM inpipe outpipe name args

-- pure, monadic version of the Maple call
-- Take care of mylock call in order to implement
-- a mutex region
mapleEvalM :: (MapleCallable a) => Handle -> Handle -> String -> [a] -> IO MapleObject
mapleEvalM inpipe outpipe name args =                 -- <maple fct> <list of maple args>
  do
#if defined(DEBUG)
    hPutStrLn stderr "-> mapleEvalM"
    hFlush stderr
#endif
    let 
      n = length args                  -- number of args to maple fct call
      mos = map toMapleObject args     -- convert all args to MapleObjects
    mylock                    `demanding` Control.Parallel.Strategies.rnf mos 
    mapleStartEval outpipe n 
    mapleEvalCore outpipe name mos           -- write fct and arg names
    mapleEndEval			     -- write call-epilogue
    mapleEvalWait inpipe                     -- wait for result
    mstr <- mapleResult inpipe		     -- grab result
    myunlock `demanding` Control.Parallel.Strategies.rnf mstr
    return (MAR mstr)                        -- and pull into Haskell-land

-- Implements a Maple call; returns a list of results
-- @cindex mapleEvalN
mapleEvalN :: (MapleCallable a) => String -> [a] -> [MapleObject]
mapleEvalN name args = 
  unsafePerformIO $ 
  do 
    inpipep <- creadInPipePointer 
    inpipe <- deRefStablePtr inpipep
    outpipep <- creadOutPipePointer
    outpipe <- deRefStablePtr outpipep
    mapleEvalNM inpipe outpipe name args

-- pure, monadic version of the Maple call
-- Note the use of Control.Parallel.Strategies.rnf to require
-- reduced normal form (rnf), since the weak head normal forg (whnf)
-- may cause problems in an interactive-monadic context
-- @cindex mapleEvalNM
{-# INLINE mapleEvalNM #-}
mapleEvalNM :: (MapleCallable a) => Handle -> Handle -> String -> [a] -> IO [MapleObject]
mapleEvalNM inpipe outpipe name args =
  do 
    let 
      n = length args                  -- number of args to maple fct call
      mos = map toMapleObject args     -- convert all args to MapleObjects
    mylock        `demanding` Control.Parallel.Strategies.rnf mos 
    mapleStartEvalN outpipe n
    mapleEvalCore outpipe name mos          -- write fct and arg names
    mapleEndEval			    -- write call-epilogue
    mapleEvalWait inpipe                    -- wait for result
    m <- mapleResultN inpipe		    -- grab result
    l <- mapM ( \ _ -> do
                  mstr <- mapleNextResult inpipe   -- get next result
		  let x = (MAR mstr)
                  return x `demanding` Control.Parallel.Strategies.rnf x) [1..m]  
    myunlock	  `demanding` Control.Parallel.Strategies.rnf l
    return l				--  list elem!


---------------------------------------------------------------------------
-- @node High level conversion routines, Communication with Maple process, Maple calls
-- @section High level conversion routines
---------------------------------------------------------------------------

-- a -> MapleObject

-- convert a Haskell Int to a MapleObject
-- @cindex int2MapleObject
int2MapleObject :: Int -> MapleObject
int2MapleObject = unsafePerformIO . int2MapleObjectM

-- Pure, monadic version of previous
-- Note how we enter the monada on a external context (FFI)
-- to grab the pipe.
-- @cindex int2MapleObjectM
int2MapleObjectM :: Int -> IO MapleObject
int2MapleObjectM n = 
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mapleInt inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n
 


-- convert a Haskell arbitrary precision integer to a MapleObject
-- @cindex integer2MapleObjectM
integer2MapleObject :: Integer -> MapleObject
integer2MapleObject = unsafePerformIO . integer2MapleObjectM

-- Pure, monadic version of previous
-- ditto
-- @cindex integer2MapleObjectM
integer2MapleObjectM :: Integer -> IO MapleObject
integer2MapleObjectM n =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mapleInteger inpipe outpipe n `demanding` Control.Parallel.Strategies.rnf n

-- convert a Haskell Char to a MapleObject
-- @cindex char2MapleObject
char2MapleObject :: Char -> MapleObject
char2MapleObject = unsafePerformIO . char2MapleObjectM


-- Pure, monadic version of previous
-- Note how we enter the monad
-- to grab the pipe from external context (FFI)
-- @cindex char2MapleObjectM
char2MapleObjectM :: Char -> IO MapleObject
char2MapleObjectM c =
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mapleChar inpipe outpipe c `demanding` Control.Parallel.Strategies.rnf c



-- convert a MapleObject to a Haskell String
-- @cindex string2MapleExpr
string2MapleExpr :: String -> MapleObject
string2MapleExpr = unsafePerformIO . string2MapleExprM

-- Pure, monadic version of previous
-- Ditto
-- @cindex string2MapleExprM
string2MapleExprM :: String -> IO MapleObject
string2MapleExprM str = 
  do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   mapleExpr inpipe outpipe str `demanding` Control.Parallel.Strategies.rnf str


-- convert a Haskell String to a Maple String 
-- @cindex string2MapleObject
string2MapleObject :: String -> MapleObject
string2MapleObject x = string2MapleExpr ("\"" ++ x ++ "\"")

-- convert a Haskell String to a Maple name ( not an expression! )
-- Usually a name in Maple is used as a way
-- to implement "delayed " evaluation mode (lazy?). 
-- ( read Maple User's Guide )
-- @cindex string2MapleName
string2MapleName x = string2MapleExpr ("`" ++ x ++ "`")



--  MapleObject -> a


-- convert a MapleObject to a Haskell Int
-- Ditto.
-- @cindex mapleObject2Int
mapleObject2Int :: MapleObject -> Int
mapleObject2Int mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInt inpipe outpipe mo


-- convert a MapleObject to a Haskell arbitrary precision Integer
-- Ditto.
-- @cindex mapleObject2Integer
mapleObject2Integer :: MapleObject -> Integer
mapleObject2Integer mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellInteger inpipe outpipe mo


-- convert a MapleObject to a Haskell Char
-- ditto
-- @cindex mapleObject2Char
mapleObject2Char :: MapleObject -> Char
mapleObject2Char mo =   unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellChar inpipe outpipe mo


-- convert a MapleObject to a Haskell String
-- ditto
-- @cindex mapleObject2String
mapleObject2String :: MapleObject -> String
mapleObject2String mo = unsafePerformIO $
 do
   inpipep <- creadInPipePointer 
   inpipe <- deRefStablePtr inpipep
   outpipep <- creadOutPipePointer
   outpipe <- deRefStablePtr outpipep
   haskellString inpipe outpipe mo


---------------------------------------------------------------------------
-- @node Communication with Maple process, Low level conversion routines, High level conversion routines
-- @section Communication with Maple process
--
-- Writing a MapleObject directly to the pipe connecting with the Maple proc
-- 
-- This section has been dropped from the 0.1 version.
-- writeStringToFile, writeByteArrayToFile are just:
--
-- hPutStrLn handle mstr
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- @node Low level conversion routines, Operations on Stream of Bytes, Communication with Maple process
-- @section Low level conversion routines
--
-- This section has been dropped from the 0.1 version.

-- mapleExtractChar_ret , mapleExtractInt_ret
-- are just
-- 
-- read (string)
-- mapleEextractMO is just the MAR Constructor.
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- @node Operations on Stream of Bytes, Lock/Unlock operations , Low level conversion routines
-- @section Operations on Stream of Bytes
--
-- Basic operations on the representation of MapleObjects as ByteArrays
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
-- the pipe to Maple
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
-- mapleEval :: (MapleCallable a) => String -> [a] -> MapleObject
--
-- instead of
--
-- mapleEval :: (MapleCallable a) => MVar -> String -> [a] -> MapleObject
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
isSaneMO :: MapleObject -> Bool
isSaneMO (MAR str) = and (map ( \ c -> isAscii c && (not (isControl c))) str)
#endif
-}

-- length of the ByteArray
-- @cindex lengthMO
lengthMO   :: MapleObject -> Int
lengthMO (MAR str) = length str

showMapleObject :: MapleObject -> [Char]
showMapleObject mo@(MAR str) = "DBG> Size: " ++  (show (lengthMO mo)) ++ "; Contents: " ++ str


-------------------------------------------------------------------
-- @node Settting up/down interface operations, Basic IO operations, Lock/Unlock operations 
-- @section Settting up/down interface operations
---------------------------------------------------------------------------


#ifdef __PARALLEL_HASKELL__
-- This is the famous patch to solve the replication of Maple
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
-- mapleInitAllGum :: IO (Int) -- return how many PE's have been setup.
-- mapleInitAllGum = do   hPutStrLn stderr ("\t====== Setting up Maple ...." ++ show Eden.noPe ++ " ")
-- 		       peNos <- mapM (\pe -> instantiateAt pe (process mapleInitLocalGum) pe ) [1..Eden.noPe] 
-- 		       rnf peNos `seq` 
-- 			   return (length peNos)

mapleInitAllGum :: IO (Int) -- return how many PE's have been setup.
mapleInitAllGum = do
		     hPutStrLn stderr ("\t====== Setting up Maple around ..."++ show Eden.noPe ++ " ")
                     let a = [process (mapleInitLocalGum) # (i) | i <- [1..Eden.noPe] ] `using` spine
		     return (length a) `demanding` Control.Parallel.Strategies.rnf a

-- (Should) return the id for the PE in GUM.
mapleInitLocalGum :: Int -> Int
mapleInitLocalGum = unsafePerformIO . mapleInitLocalGumM

mapleInitLocalGumM :: Int -> IO (Int) -- return the number of the PE just setup
mapleInitLocalGumM i = 
#else
mapleInit :: IO ()
mapleInit =
#endif
 do
#ifdef __PARALLEL_HASKELL__
   -- These signal handlers seem not to bet processed by Eden
   -- Reason: I guess PVM Signaling system overwrites Eden's.
#else
   -- install signal handler to terminate Maple process properly 
   installHandler sigTERM (Catch (mapleTerm)) Nothing
   installHandler sigQUIT (Catch (mapleTerm)) Nothing
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
		     executeFile MAPLEBIN False [MAPLEARGS] Nothing
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
		      mapleRoot <- System.Environment.getEnv "casROOTEden"
		      hPutStrLn outpipe ("read `" ++ mapleRoot ++ "/init.maple`:")
		      hPutStrLn outpipe  ("read `"++ mapleRoot ++ "/start.maple`:")
		      -- make termination sure
		      -- wait for process to be ready
		      a <- hGetLine inpipe
#if defined(DEBUG)
		      putStrLn ("\tLaunching pid...: \t" ++ (show pid))
		      putStrLn ("\tMAPLE SERVER :... " ++ a)
#endif
		      -- first line in protocol is name of debug file 
		      hPutStrLn outpipe (mapleRoot ++ "/error." ++ show(pid))
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
-- Same as mapleInitAllGum (read above)
-- You must call this function the last
-- on your main program
-- when no more processes are remaining
-- to asure 1:1 process:PE
-- Otherwise, concurrent process inside a PE
-- may become a chaos.
mapleTermAllGum :: IO(Int) -- return how many PE's have been set down.
mapleTermAllGum = do
 		      hPutStrLn stderr "\t====== Setting down Maple ...." 
 		      peNos <- mapM (\pe -> instantiateAt pe (process mapleTermLocalGum) pe ) [1..Eden.noPe] 
 		      rnf peNos `seq` 
 		   	 return (length peNos)

-- mapleTermAllGum :: IO(Int) -- return how many PE's have been set down.
-- mapleTermAllGum = do
-- 		      hPutStrLn stderr ("\t====== Setting down Maple around GUM...." ++ show Eden.noPe ++ " ")
-- 		      let a = [process (mapleTermLocalGum) # (i) | i <- [1..Eden.noPe] ]  `using` spine
-- 		      return (length a) `demanding` Control.Parallel.Strategies.rnf a

--
-- A hack to evaluate the list eagarly
--
spine :: [a] -> ()
spine [] = ()
spine (x:xs) = spine xs

-- (Should) return the Id of the PE.
mapleTermLocalGum :: Int -> Int
mapleTermLocalGum = unsafePerformIO . mapleTermLocalGumM


-- Monadic version:
-- Killing the server process.		   
mapleTermLocalGumM :: Int -> IO(Int)
mapleTermLocalGumM i = 
#else
mapleTerm :: IO()
mapleTerm =
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
   putStrLn ("\tMAPLE SERVER :... terminate")
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
readMaple :: Handle -> IO (String)
readMaple hdl = do
	         line <- hGetLine hdl
		 return (line)

-------------------------------------------------------------------------------
--
-- Read the Header of a HM message.
--
-- Panic error: When the Maple server sends other thing
-- than an Integer, the system will abort dirtly
-- (as Haskell Prelude is designed to)
-- Be warned if you are debugging !! 
-------------------------------------------------------------------------------
readMapleCode :: Handle -> IO(Int)
readMapleCode hdl = do
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
mapleStartEval :: Handle -> Int -> IO()
mapleStartEval outpipe  n =
  do
    hPutStrLn outpipe "fun"
    hPutStrLn outpipe (show n)
    return ()

-- wait for Maple Peer answer to be available.
-- Since the subsequent IO-call -hGetLine-
-- operates on a  Handle -not a Fd-, 
-- the RTS prevents to block the rest of the threads.
-- So, we Keep it here for historical reasons.
mapleEvalWait :: Handle -> IO ()
mapleEvalWait inpipe =  return ()



-- This writes the fct name and the args (must have been converted to 
-- MapleObjects already) to the given handle (the pipe).
-- @cindex mapleEvalCore
{-# INLINE mapleEvalCore #-}
mapleEvalCore :: Handle -> String -> [MapleObject] -> IO ()
mapleEvalCore hdl name args =
  do 
    hPutStrLn hdl name
    mapM (\ arg -> (writeToMaple hdl arg)) args
    return ()




------------------------------------------------------------------------------
-- mapleResult
-- Reads the whole HM Message ( Header plus Body )
-- and lifts the body part
------------------------------------------------------------------------- 
mapleResult :: Handle -> IO (String)
mapleResult  inpipe = 
 do
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   if (header/=0) 
      then
        do 
	  report "error in Maple function" body
	  return  body 
      else return  body 

----------------------------------------------------------------------------
-- end evaluation
-- In 0.1 version it used to commit the (fflush)
-- but in 0.2 automatic flush is achieved automatically when
-- a line is entered.
-- Kept here for historical reasons.
 ------------------------------------------------------------------------- */
mapleEndEval :: IO ()
mapleEndEval = return ()  




 ----------------------------------------------------------------------------
 -- start evaluation with n arguments and sequence of results
 --
 ------------------------------------------------------------------------- 
mapleStartEvalN :: Handle -> Int -> IO()
mapleStartEvalN outpipe  n =
  do
    hPutStrLn outpipe "lst"
    hPutStrLn outpipe (show n)
    return()


------------------------------------------------------------------------------
-- mapleResultN
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
mapleResultN :: Handle -> IO(Int)
mapleResultN inpipe=
 do
   header <- readMapleCode inpipe
   if (header<0)  -- Error in service, just log it.
     then
       do
	 body <- readMaple inpipe   
	 report "error in Maple function" body
         return (header)
     else   return (header)  
   
 ------------------------------------------------------------------------------
 -- mapleNextResult
 -- Assumed handle ready,
 -- reads subsequent body calls.
 ------------------------------------------------------------------------- */
mapleNextResult :: Handle -> IO (String)
mapleNextResult  inpipe = 
 do
   partialBody <- readMaple inpipe
   return ( partialBody) 


-------------------------------------------------------------------
-- @node    Conversion Routines, , Evaluation routines,
-- @section Debugging routines
---------------------------------------------------------------------------
-- Protocol HM works in this way.
-- 1.- Sending header (in,out)
-- 2.- Sending body "[4,4,x^2]"
-- 3.- Receiving header (0,-1...)
-- 4.- Receiving body (MapleObject)
--
-- WARNING: TODO: Exception on Maple side treatment.

----------------------------------------------------------------------------
-- convert Haskell string to Maple object
-- ------------------------------------------------------------------------- */
mapleExpr:: Handle -> Handle -> String -> IO (MapleObject)
mapleExpr inpipe outpipe expr =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe expr
   header <- readMapleCode inpipe
   body <- readMaple inpipe
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
-- convert Haskell int to Maple integer
-- ditto.
------------------------------------------------------------------------- */

mapleInt::  Handle -> Handle -> Int -> IO (MapleObject)
mapleInt inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Int" body
       return  (MAR body)
    else
       return  (MAR body)



----------------------------------------------------------------------------
-- convert an arbitrary Haskell integer to Maple integer
-- ditto
------------------------------------------------------------------------- */
mapleInteger::  Handle -> Handle -> Integer -> IO (MapleObject)
mapleInteger inpipe outpipe num =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show num)
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Haskell Integer" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert an arbitrary Haskell char to Maple char
-- ditto
------------------------------------------------------------------------- */
mapleChar::  Handle -> Handle -> Char -> IO (MapleObject)
mapleChar inpipe outpipe char =
 do
   mylock
   hPutStrLn outpipe "in" 
   hPutStrLn outpipe (show char)
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header/=0)
    then
      do
       report "error in converting Haskell Char" body
       return  (MAR body)
    else
       return  (MAR body)


----------------------------------------------------------------------------
-- convert Maple Object to Haskell string
--
--------------------------------------------------------------------------- */
haskellString ::Handle -> Handle -> MapleObject -> IO(String)
haskellString inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Maple String" body
       return  (body)
    else
       return  (body)


----------------------------------------------------------------------------
-- convert Maple string to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInt ::Handle -> Handle -> MapleObject -> IO(Int)
haskellInt inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- ditto
   if (header/=0)
    then
      do
       report "error in converting Maple Int" body
       return  (read body)
    else
       return  (read body)

 ----------------------------------------------------------------------------
-- convert Maple Integer to Haskell string
--
 ------------------------------------------------------------------------- */
haskellInteger ::Handle -> Handle -> MapleObject -> IO(Integer)
haskellInteger inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Maple Int" body
       return  (read body)
    else
       return  (read body)

----------------------------------------------------------------------------
-- convert Maple Char to Haskell string
--
------------------------------------------------------------------------- */
haskellChar ::Handle -> Handle -> MapleObject -> IO(Char)
haskellChar inpipe outpipe (MAR mstr) =
 do
   mylock
   hPutStrLn outpipe "out" 
   hPutStrLn outpipe mstr
   header <- readMapleCode inpipe
   body <- readMaple inpipe
   myunlock `demanding` Control.Parallel.Strategies.rnf body -- NEW
   if (header /= 0)
    then
      do
       report "error in converting Maple Int" body
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
-- we are wasting information from Maple messages.
-- info is usually a formatted string.
------------------------------------------------------------------------- */
report:: String -> String -> IO()
report key info = do
		   hPutStrLn stderr ("maple interface error: "++key++"("++info++")")
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
