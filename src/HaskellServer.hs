-- Haskell server, performing some Haskell-side computer algebra computations

module Main where

#if 1

#warning Haskell Server currently not supported

main = error "Haskell Server currently not supported"

#else

import Network
import System
import System.IO
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as C

import SCSCP_API
import HS_SCSCP
import HS2SCSCP

-- import HACK_PreShared
import BaseServices

import Poly       -- polynomial library from GB package
import Karatsuba  -- Haskell implementation of Karatsuba multiplication

dumpComm :: PortNumber -> IO()
dumpComm port = do 
	  putStrLn ("starting up dumpComm, opening port " 
                    ++ show port)
          sock <- listenOn (PortNumber port)
          (h,name,num) <- accept sock
          hSetBuffering h NoBuffering
          putStrLn "someone connected... :) "
	  putStrLn "I say hi, and dump anything I get then."
          hPutStrLn h (writePI (myInit port))
          hPutStrLn h (writePI (Quit Nothing))
          putStrLn ("peer sent me this  (total): \n")
	  dump h
	  sClose sock
          putStrLn "exited"


dump :: Handle -> IO ()
dump h = C.catch (do c <- hGetChar h 
                     putStr (show c)
		     when (c=='\n') (putChar c) 
		     dump h) 
                  (\e -> return ())


main :: IO()
main = do args <- getArgs
          let portNum = if null args 
                         then 12321
                         else fromInteger (read (head args))
	  dServer portNum

dServer :: PortNumber -> IO ()
dServer portNum = do
 putStrLn ("starting up, opening port " ++ show portNum)
 sock <- listenOn (PortNumber portNum)
 C.catch (do (h,name,num) <- accept sock
	     hSetBuffering h NoBuffering
	     putStrLn "someone just connected... :) I say hi."
	     hPutStrLn h (writePI (myInit portNum))
	     answer <- hGetLine h
	     putStrLn ("client: " ++ answer )
             let str = (writePI (Version "1.2"))
	     hPutStrLn h str
	     putStrLn ("server: "++str)
	     scscpInteract h
	     hPutStrLn h (writePI (Quit (Just "Hey, I am a dummy server")))
	     sClose sock
	     putStrLn "exited")
      (\e -> do putStrLn ("something went wrong...\n" ++ show e)
                sClose sock 
                return ())
 dServer portNum

myInit :: PortNumber -> SCSCP_PI
myInit portNum = Init { piInitName="dummyServer"
                      , piInitV   ="0.0alpha"
                      , piInitID  ="dummyServer:" ++ show portNum
                      , piInitSCSCPs  =words ("1.2 1.2beta 1.2.345")}

scscpInteract :: Handle -> IO()
scscpInteract h = do 
         putStrLn "interact: expecting some input"
         -- expect a PCall message, enclosed in PIs
         (between,pi) <- splitAtPI h
         when (not (null between)) 
           (putStrLn ("ignored input before PI: " ++ between))
         case pi of 
           Quit _ -> do putStrLn "received quit message"
                        return ()
           Start  -> do (input,pi) <- splitAtPI h
                        case pi of 
                          Cancel -> do putStrLn "canceled, start over"
                                       scscpInteract h
                          End -> do 
                            let msg = readSCSCPMsg input
                            putStrLn "received the following:"
                            putStrLn (show msg)
			    answer <- processThis msg
                            -- threadDelay 1000 
                            hPutStrLn h (writePI Start)
                            hPutStrLn h (writeSCSCPMsg answer)
                            hPutStrLn h (writePI End)
                          Quit _ -> do putStrLn "received quit message"
                                       return ()
                          other -> putStrLn ("unexpected PI after Start: " 
                                             ++ show other)
           other -> putStrLn ("unexpected PI (ignoring it): " 
                              ++ show other)
         scscpInteract h -- start over


processThis :: SCSCPMsg -> IO SCSCPMsg 
processThis ( PCall id name dat opts)
    | name `elem` supportedNames 
	= do putStrLn ("supported call message for "++(show name))
             C.catch (let Just f = lookup name supported
		          r = f dat
		      in case r of
	               Left msg  -> do putStrLn $ "this error while processing: "++msg
                                       return $ 
	                                  PTerminated id (CAMsg msg) 
                                                      Nothing Nothing
	               Right res -> do putStr "successful, result: "
                                       putStrLn (show res)
                                       return $
	                                 PResult res id Nothing Nothing
		  ) (\e -> do putStrLn ("exception:\n" ++ show e)
                              return $ 
  	                         PTerminated id (CAMsg (show e)) 
                                             Nothing Nothing
		    )
processThis anyMessage
    = do putStrLn "unsupported call message"
         return $ PTerminated (callID anyMessage) 
                              (CAMsg "dummy server") 
			      Nothing Nothing

-------------------------------------------------------------
-- server functionality defined here
-------------------------------------------------------------

supported_local = [ (scscp_HS_Phi                , phiOM            )
             	  , (scscp_HS_Fib		 , fibOM            )
             	  , (scscp_HS_Factorial	         , factOM           )
             	  , (scscp_HS_FactorialAcc	 , factAccOM        )
             	  , (scscp_HS_Plus		 , plusOM           )
             	  , (scscp_HS_ProductPoly	 , productPolyOM    )
             	  , (scscp_HS_SumPoly	         , sumPolyOM        )
             	  , (scscp_HS_DifferencePoly	 , differencePolyOM )
             	  , (scscp_HS_QuotientPoly	 , quotientPolyOM   )
             	  , (scscp_HS_KaratsubaPoly 	 , karatsubaPolyOM  )
                  ]

supported :: [(CAName, [OMObj] -> Either String OMObj)]
supported = supported_caStandard ++ supported_local

supportedNames =  map fst supported_local
supportedFs    =  map snd supported_local

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

caStandardOps = [ if op==(Right GetAllowedHeads)
                    then handleGetAllowedHeads
                    else \args -> Left (show op ++ ": not implemented")
		| op <- caStandardNames ]

supported_caStandard = zip caStandardNames caStandardOps

handleGetAllowedHeads :: [OMObj] -> Either String OMObj
handleGetAllowedHeads _ = Right (toOM "euler, plus, productPoly, productPoly, sumPoly, differencePoly, quotientPoly, karatsuba")

-----------------------------------------------------------------------------

phiOM :: [OMObj] -> Either String OMObj
phiOM []  = Left "no argument"
phiOM [x] = Right (toOM (dumbPhi (fromOM x)))
phiOM xs  = Left "too many arguments"

dumbPhi :: Integer -> Integer
dumbPhi n | n < 2     = 0
          | otherwise = fromIntegral $ 
                        length [ p | p <- [1..n-1], gcd n p == 1 ]

-----------------------------------------------------------------------------

fibOM :: [OMObj] -> Either String OMObj
fibOM []  = Left "no argument"
fibOM [x] = Right (toOM (fib (fromOM x)))
fibOM xs  = Left "too many arguments"

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-----------------------------------------------------------------------------

factOM :: [OMObj] -> Either String OMObj
factOM []  = Left "no argument"
factOM [x] = Right (toOM (fact (fromOM x)))
factOM xs  = Left "too many arguments"

fact :: Integer -> Integer
fact 0 = 1
fact n = n*(fact (n-1))

factAccOM :: [OMObj] -> Either String OMObj
factAccOM []  = Left "no argument"
factAccOM [x] = Right (toOM (factAcc (fromOM x) (0::Integer)))
factAccOM xs  = Left "too many arguments"

factAcc :: Integer -> Integer -> Integer
factAcc 0 acc = acc
factAcc n acc = factAcc (n-1) (n*acc)

--------------

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

productPolyOM :: [OMObj] -> Either String OMObj
-- productPolyOM _ = error "productPolyOM not implemented"
productPolyOM [p1',p2'] = let str1 :: String
                              str1 = fromOM p1'
                              str2 :: String
                              str2 = fromOM p2'
                              p1 :: Poly
                              p1 = read str1
                              p2 :: Poly
                              p2 = read str2
                          in  Right (toOM (show (product_Poly p1 p2)))

sumPolyOM :: [OMObj] -> Either String OMObj
sumPolyOM [p1',p2'] = let str1 :: String
                          str1 = fromOM p1'
                          str2 :: String
                          str2 = fromOM p2'
                          p1 :: Poly
                          p1 = read str1
                          p2 :: Poly
                          p2 = read str2
                      in  Right (toOM (show (sum_Poly p1 p2)))

differencePolyOM :: [OMObj] -> Either String OMObj
differencePolyOM [p1',p2'] = let str1 :: String
                              	 str1 = fromOM p1'
                              	 str2 :: String
                              	 str2 = fromOM p2'
                              	 p1 :: Poly
                              	 p1 = read str1
                              	 p2 :: Poly
                              	 p2 = read str2
                             in  Right (toOM (show (difference_Poly p1 p2)))

quotientPolyOM :: [OMObj] -> Either String OMObj
quotientPolyOM [p1',p2'] = let str1 :: String
                               str1 = fromOM p1'
                               str2 :: String
                               str2 = fromOM p2'
                               p1 :: Poly
                               p1 = read str1
                               p2 :: Poly
                               p2 = read str2
                           in  Right (toOM (show (quotient_Poly p1 p2)))

karatsubaPolyOM :: [OMObj] -> Either String OMObj
karatsubaPolyOM [p1', p2'] = let str1 :: String
                                 str1 = fromOM p1'
                                 str2 :: String
                                 str2 = fromOM p2'
                                 p1 :: [Poly]
                                 p1 = read str1
                                 p2 :: [Poly]
                                 p2 = read str2
                             in  Right (toOM (show (karatsuba p1 p2)))

#endif
