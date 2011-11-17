{-# OPTIONS_GHC -XMagicHash #-}
{- --------------------------------------- 
The low-level parallel functional language:
      EDen Implementation language

This module defines sensible abstractions
over the primitives implemented in the RTE.
------------------------------------------ -}
module Edi 
-- interface:
   (fork,           -- :: IO () -> IO (), from conc.hs, without ThreadID
    spawnProcessAt, -- :: Int -> IO () -> IO ()
    spawnArgsProcessAt, -- ::NFData a =>  Int -> (a -> IO ()) -> a -> IO ()
    ChanName',      -- EdI channel type 
    createC,        -- :: IO (ChanName' a,a) , prim.Op.
    createCs,       -- :: Int -> IO ([ChanName' a],[a])
    sendWith,       -- :: (Strategy a) -> ChanName' a -> a -> IO ()
    sendNF,         -- :: NFData a => ChanName' a -> a -> IO ()
    sendStreamWith, -- :: (Strategy a) -> ChanName' [a] -> [a] -> IO ()
    sendNFStream,   -- :: NFData a => ChanName' [a] -> [a] -> IO ()
    noPe, selfPe,   -- :: IO Int
    module Strategies
   )
   where

import Control.Parallel.Eden.ParPrim -- primitives
import Control.Parallel.Strategies as Strategies hiding (Strategy)
import Control.DeepSeq
import Control.Concurrent
import GHC.Base(Int#,Int(..), (+#) )

type Strategy a = a -> ()

-- data ChanName' a = Chan Int# Int# Int#
--               deriving Show
instance NFData a => NFData (ChanName' a)
    where rnf (Chan pe proc i) = rnf (I# (pe +# proc +# i))

-- Process Creation:
--------------------
spawnProcessAt :: Int -> IO () -> IO () -- forces IO() type!
spawnProcessAt pe action = sendData (Instantiate pe) action

-- additional: force evaluation of arguments (uncurried version)
spawnArgsProcessAt :: NFData a => Int -> (a -> IO()) -> a -> IO ()
spawnArgsProcessAt pe argsAction args 
               = (rnf args `seq` 
		  sendData (Instantiate pe) (argsAction args))

-- Communication:
-----------------

-- creation of n channels in one call, "safe" evaluation
createCs :: NFData a => Int -> IO ([ChanName' a],[a])
createCs n | n >= 0 = do list <- sequence (replicate n createC)
			 let (cs, vs) = unzip list
                         rnf cs `seq` -- channels fully evaluated
                         -- spine vs `seq` -- value list spine (optional)
                            return (cs,vs)
           | otherwise = error "createCs: n < 0"
             

-- Evaluation / Communication:
------------------------------
sendWith :: (Strategy a) -> ChanName' a -> a -> IO ()
sendWith strat c d = connectToPort c >> 
                     (strat d `seq` sendData Data d)

-- sendChan mit Auswertung, ohne Connect-Nachricht
sendNF :: NFData a => ChanName' a -> a -> IO ()
sendNF = sendWith rnf

sendStreamWith :: (Strategy a) -> ChanName' [a] -> [a] -> IO ()
sendStreamWith strat c xs = connectToPort c >> 
                            send xs
    where send l@[]   = sendData Data l
          send (x:xs) = (strat x `seq` sendData Stream x) >> 
                        send xs

sendNFStream :: NFData a => ChanName' [a] -> [a] -> IO ()
sendNFStream = sendStreamWith rnf

--------------------------------------------------------------

-- JUNKYARD:

-- monadic evaluation control
rnfM :: NFData a => a -> IO ()
rnfM x = case rnf x of { () -> yield }
--rnfM  = return . rnf -- doznwork: returns _unevaluated_ (rnf x)

-- sendChan ohne Auswertung und Connect-Nachricht
sendVia :: ChanName' a -> a -> IO ()
sendVia c d = connectToPort c >> 
	      sendData Data d
		      
-- sendChan mit Auswertung und Connect-Nachricht
connectSendNFvia :: NFData a => ChanName' a -> a -> IO ()
connectSendNFvia c d = connectToPort c >> 
		       sendData Connect d >>
		       rnfM d >>
		       sendData Data d

-- sendStream: Connect-Nachricht und Stream-NF Senden
sendStreamNFvia :: NFData a => ChanName' [a] -> [a] -> IO ()
sendStreamNFvia c d = connectToPort c >> 
		      sendData Connect d >>
		      sendStream' d
    where sendStream'  l@[] = sendData Data l
	  sendStream' (x:xs)= rnfM x >>
			      sendData Stream x >> 
			      sendStream' xs

