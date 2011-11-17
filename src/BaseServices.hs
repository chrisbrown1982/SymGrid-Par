{-
Time-stamp: <Mon Jun 28 2010 23:16:44 Stardate: Stardate: [-28]3294.43 hwloidl>

This module defines basic services, provided via SCSCP interfaces, that should
be known to the coordination server from initialisation.

Naming scheme (it's just a scheme, for convenience, the CD should identify the service):
WS_ ... GAP server
HS_ ... Haskell server
CS_ ... Coordination server

See testClient.hs as an example of a client, testing the coordination server.
See CoordinationServer.hs, acting as a client to eg. GAP server.
-}

module BaseServices where

import SCSCP_API

-----------------------------------------------------------------------------
-- GAP services
scscp_WS_Plus = Left ("scscp_transient_1", "WS_Plus")
scscp_WS_Factorial = Left("scscp_transient_1", "WS_Factorial")
scscp_WS_Phi = Left ("scscp_transient_1", "WS_Phi") -- matches GAP server init
scscp_WS_Fibonacci = Left ("scscp_transient_1", "WS_Fibonacci") -- matches GAP server init
--scscp_WS_Bonzo = Left ("scscp_transient_1", "WS_Phi") -- matches GAP server init
scscp_WS_sumEulerClassic = Left("scscp_transient_1", "WS_sumEulerClassic")
scscp_WS_SumEulerRange = Left("scscp_transient_1", "WS_SumEulerRange")
scscp_WS_sumEulerList = Left("scscp_transient_1", "WS_sumEulerList")
scscp_WS_IdGroup = Left("scscp_transient_1", "WS_IdGroup")
scscp_WS_AutomorphismGroup = Left("scscp_transient_1", "WS_AutomorphismGroup")
-- ...
scscp_WS_RandomPolynomial = Left("scscp_transient_1", "WS_RandomPolynomial")
scscp_WS_RandomPolynomialAsString = Left("scscp_transient_1", "WS_RandomPolynomialAsString")
--scscp_WS_Karatsuba = Left("scscp_transient_1", "WS_Karatsuba")
scscp_WS_Polynomial2String = Left("scscp_transient_1", "WS_Polynomial2String")
scscp_WS_KaratsubaStr = Left("scscp_transient_1", "WS_KaratsubaStr")
scscp_WS_KaratsubaStr_x = Left("scscp_transient_1", "WS_KaratsubaStr_x")
scscp_WS_Karatsuba = Left ("scscp_transient_1","WS_Karatsuba")
--scscp_WS_KaratsubaGAPstr = Left ("scscp_transient_1","WS_Karatsuba") -- this uses strings as input, i.e. GAP specific
--scscp_WS_KaratsubaGAP = Left ("scscp_transient_1","HWL_Karatsuba")
--scscp_WS_Polynomial2String = Left ("scscp_transient_1", "HWL_Polynomial2String")
scscp_WS_Resultant = Left ("scscp_transient_1","WS_Resultant")
scscp_WS_Res = Left ("scscp_transient_1","WS_Res")
scscp_WS_GroebnerBasis = Left ("scscp_transient_1","WS_GroebnerBasis")

-----------------------------------------------------------------------------
-- Haskell services
scscp_HS_Phi  = Left ("scscp_transient_2","euler")
scscp_HS_Fib  = Left ("scscp_transient_2","fib")
scscp_HS_Factorial  = Left ("scscp_transient_2","fact")
scscp_HS_FactorialAcc  = Left ("scscp_transient_2","factAcc")
scscp_HS_Plus = Left ("scscp_transient_2","plus")
-- scscpSumEulerClassic = Left("scscp_transient_1", "WS_sumEulerClassic")
scscp_HS_ProductPoly = Left ("scscp_transient_2","productPoly")
scscp_HS_SumPoly     = Left ("scscp_transient_2","sumPoly")
scscp_HS_DifferencePoly = Left ("scscp_transient_2","differencePoly")
scscp_HS_QuotientPoly = Left ("scscp_transient_2","quotientPoly")
scscp_HS_KaratsubaPoly = Left ("scscp_transient_2","karatsubaPoly")

-----------------------------------------------------------------------------
--- CMB added orbit
scscp_WS_Fib1 = Left ("scscp_transient_1", "WS_Fib1")
scscp_WS_Fib2 = Left ("scscp_transient_1", "WS_Fib2")
scscp_WS_Fib3 = Left ("scscp_transient_1", "WS_Fib3")

scscp_WS_Mat1 = Left ("scscp_transient_1", "WS_Mat1")
scscp_WS_Mat2 = Left ("scscp_transient_1", "WS_Mat2")
scscp_WS_Mat3 = Left ("scscp_transient_1", "WS_Mat3")

scscp_WS_Fin1 = Left ("scscp_transient_1", "WS_Fin1")
scscp_WS_Fin2 = Left ("scscp_transient_1", "WS_Fin2")

scscp_CS_Orbit = Left ("scscp_transient_1", "CS_Orbit")
scscp_CS_HMI = Left ("scscp_transient_1", "CS_HMI")


-- Coordination server
scscp_CS_Phi = Left ("scscp_transient_1", "CS_Phi")
scscp_CS_Fib = Left ("scscp_transient_1", "CS_Fib")
scscp_CS_Factorial = Left("scscp_transient_1", "CS_Factorial")
scscp_CS_SumEuler2 = Left("scscp_transient_1", "CS_sumEuler2") -- HACK: uses transient_1 to match with gap CLIENT
scscp_CS_SumEuler = Left("scscp_transient_1", "CS_SumEuler") -- HACK: uses transient_1 to match with gap CLIENT
scscp_CS_SumEulerPar = Left("scscp_transient_1", "CS_sumEulerPar") -- HACK: uses transient_1 to match with gap CLIENT
scscp_CS_SumEulerShuffle = Left ("scscp_transient_1","CS_sumEulerShuffleSCSCP")  -- HACK: uses transient_1 to match with gap CLIENT
scscp_CS_RandomPolynomial = Left ("scscp_transient_1","CS_RandomPolynomial") -- pass through
scscp_CS_RandomPolynomialAsString = Left ("scscp_transient_1","CS_RandomPolynomialAsString") -- pass through
scscp_CS_Polynomial2String = Left("scscp_transient_1", "CS_Polynomial2String") -- pass through
scscp_CS_KaratsubaStr = Left("scscp_transient_1", "CS_KaratsubaStr") -- pass through
scscp_CS_KaratsubaStr_x = Left("scscp_transient_1", "CS_KaratsubaStr_x") -- pass through
scscp_CS_Karatsuba = Left ("scscp_transient_1","CS_Karatsuba") -- pass through
scscp_CS_KaratsubaPar = Left ("scscp_transient_1","CS_karatsubaParSCSCP")
scscp_CS_KaratsubaPar0 = Left ("scscp_transient_1","CS_karatsubaPar0SCSCP")
-- these are only passed through
scscp_CS_SumEulerClassic  = Left ("scscp_transient_1","CS_sumEulerClassicSCSCP")
scscp_CS_Resultant = Left ("scscp_transient_1","CS_Resultant")
scscp_CS_Res = Left ("scscp_transient_1","CS_Res")
scscp_CS_Resultant2 = Left ("scscp_transient_1","CS_Resultant2")
scscp_CS_GroebnerBasis = Left ("scscp_transient_1","CS_GroebnerBasis")
-- skeletons
scscp_CS_ParProcesses1 = Left  ("scscp_transient_1","CS_parProcesses1") -- run 1 givne services on given input lists
scscp_CS_ParProcesses2 = Left  ("scscp_transient_1","CS_parProcesses2") -- run 2 given services on given input lists
scscp_CS_ParList = Left  ("scscp_transient_1","CS_parList")             -- parallel map
scscp_CS_ParMap = Left  ("scscp_transient_1","CS_parMap")       -- parallel map
scscp_CS_ParMap' = Left  ("scscp_transient_1","CS_parMap'")       -- parallel map
scscp_CS_ParMapFold = Left  ("scscp_transient_1","CS_parMapFold")       -- parallel fold-of-map
scscp_CS_ParMapFold1 = Left  ("scscp_transient_1","CS_parMapFold1")       -- parallel fold-of-map (non-empty list)
scscp_CS_ParZipWith = Left  ("scscp_transient_1","CS_parZipWith")       -- parallel zipWith
scscp_CS_Map = Left  ("scscp_transient_1","CS_map")       -- sequential map
scscp_CS_Map' = Left  ("scscp_transient_1","CS_map'")       -- sequential map
scscp_CS_MapFold = Left  ("scscp_transient_1","CS_mapFold")       -- sequential fold-of-map
scscp_CS_MapFold1 = Left  ("scscp_transient_1","CS_mapFold1")       -- sequential fold-of-map (non-empty list)
scscp_CS_ZipWith = Left  ("scscp_transient_1","CS_zipWith")       -- sequential zipWith

{-
-- some convenience short-cuts, good for interactive testing
s'prodPolyGB = scscpProductPoly
s'sumPolyGB = scscpSumPoly
s'diffPolyGB = scscpDifferencePoly
s'quotPolyGB = scscpQuotientPoly
s'karaGB = scscpKaratsuba

s'randPoly = scscpRandomPoly
s'poly2string = scscpPolynomial2String
s'kara = scscpKaratsubaGAP
s'kara' = scscpKaratsubaGAPstr
s'karaPar = scscpKaratsubaPar

s'sE  = scscpSumEulerShuffle
s'sE2 = scscp_sumEuler2
s'sE' = scscpSumEulerClassic
-}
