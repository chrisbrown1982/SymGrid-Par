----------------------------------------------------------------------------
-- (c) 2006
--     Abdallah Al Zain <ceeatia@macs.hw.ac.uk>
--     
--
-- this module is intended to bypass the heap, stack
-- of an ordinary STG machine
-- provding an interface to a external repository, 
-- (hence a side-effect).

module Monitor where

import Foreign.StablePtr
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types

#ifdef __PARALLEL_HASKELL__
import Eden -- Though not used, we need to include it explictly in every Eden program
#endif


-- There is no need to invooke cinit, since values on monitor 
-- is setup on load time.


-- Concerning resources
foreign import ccall  cwritePointers :: StablePtr a {-InPipe-} -> StablePtr a {-OutPipe-} -> StablePtr b {-Pipd-} -> StablePtr c {-Handle-} -> IO()
foreign import ccall  creadInPipePointer :: IO (StablePtr a)
foreign import ccall  creadOutPipePointer :: IO (StablePtr a)
foreign import ccall  creadPidPointer :: IO (StablePtr a)
foreign import ccall  "fork"  c_fork :: IO (CPid)


#ifdef __PARALLEL_HASKELL__
foreign import ccall  creadQueuePointer :: IO (StablePtr a)
-- On locking threads on critical region.
foreign import ccall cinit :: IO()  -- Sets to xzero
foreign import ccall ctestAndInc :: IO(CInt)  -- This simulates atomic checking and inc.
foreign import ccall ctestAndDec :: IO(CInt)  -- Decrements.
#else
foreign import ccall  creadMvarPointer :: IO (StablePtr a)
#endif



