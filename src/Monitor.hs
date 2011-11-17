{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------------------
-- (c) 2010
--     Hans-Wolfgang Loidl <hwloidl@cs.st-andrews.ac.uk>
--  code clean-up and port to ghc-6.12.2
--  
-- (c) 2008
--     Jost Berthold <berthold@mathematik.uni-marburg.de>
--  introduced more accurate types and included header file
--  
--
-- (c) 2006
--     Abdallah Al Zain <ceeatia@macs.hw.ac.uk>
--     
-- this module is intended to bypass the heap, stack of an ordinary STG machine
-- provding an interface to an external repository, (hence a side-effect).
-----------------------------------------------------------------------------

module Monitor where

import Foreign.StablePtr
import Foreign.Ptr -- for NULL pointer
 -- types only
import System.IO -- IO,Handle
import Control.Concurrent

#ifndef __PARALLEL_HASKELL__
-- version for testing with ghci, no C stuff involved
import System.IO.Unsafe
import Control.Monad

-- data is stored in CAF. We cannot do this in a parallel version,
-- because we cannot rely on per-PE execution.
{-# NOINLINE storageVar #-}
storageVar :: MVar (StablePtr (MVar a))
storageVar = unsafePerformIO (newEmptyMVar)

cwriteMVarPointer :: StablePtr (MVar a) -> IO()
cwriteMVarPointer ptr = do
  empty <- isEmptyMVar storageVar
  when (not empty) (takeMVar storageVar >> return ()) -- empty it
  putMVar storageVar ptr

cdeleteMVarPointer :: IO ()
cdeleteMVarPointer = do 
  empty <- isEmptyMVar storageVar
  if empty then return () 
           else do takeMVar storageVar
                   return ()

creadMVarPointer :: IO (StablePtr (MVar a))
creadMVarPointer = do 
  empty <- isEmptyMVar storageVar
  if empty then return (castPtrToStablePtr nullPtr)
           else readMVar storageVar

-- ID supply, hand-rolled.
{-# NOINLINE idVar #-}
idVar :: MVar Integer -- overflow impossible, but IDs may get very long!
idVar = unsafePerformIO (newMVar 0)

newID :: IO String
newID = do i <- takeMVar idVar
           putMVar idVar (i+1)
           return ("HsSCSCP" ++ show i)

#else

foreign import ccall  unsafe 
    "monitor.h cwriteMVarPointer" cwriteMVarPointer 
    :: StablePtr (MVar a) -> IO()
foreign import ccall unsafe 
    "monitor.h cdeleteMVarPointer" cdeleteMVarPointer 
    :: IO ()
foreign import ccall unsafe
    "monitor.h creadMVarPointer" creadMVarPointer 
    :: IO (StablePtr (MVar a))

foreign import ccall  unsafe 
    "monitor.h cwriteIDVarPointer" cwriteIDVarPointer 
    :: StablePtr (MVar Integer) -> IO()
foreign import ccall unsafe 
    "monitor.h cdeleteIDVarPointer" cdeleteIDVarPointer 
    :: IO ()
foreign import ccall unsafe
    "monitor.h creadIDVarPointer" creadIDVarPointer 
    :: IO (StablePtr (MVar Integer))

-- ID supply for PARALLEL_HASKELL
idInit :: IO (StablePtr (MVar Integer))
idInit = do v <- newMVar 0
            p <- newStablePtr v
            cwriteIDVarPointer p
            return p

-- will surely never be used...
idCancel :: IO ()
idCancel = cdeleteIDVarPointer

newID :: IO String
newID = do p <- creadIDVarPointer
           -- check if null, init if so
           let checkP = castStablePtrToPtr p
#warning   -- check for race condition
           p' <- if (checkP == nullPtr) then idInit 
                                        else return p
           idVar <- deRefStablePtr p
           i <- modifyMVar idVar ( \n  -> return (n+1,n)) 
                               -- take -> (put,return)
           return ("HsSCSCP" ++ show i)

#endif
