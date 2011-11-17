-- Time-stamp: <Thu Apr 01 2010 09:59:14 Stardate: Stardate: [-28]2851.66 hwloidl>
--
-- Basic SCSCP operations such as readSCSCPMsg and writeSCSCPMsg.
-- Builds on HaXmL for realising OpenMath encoding.
-----------------------------------------------------------------------------


module HS2SCSCP where

import SCSCP_DTD -- parser support
import HS_SCSCP  -- types 

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.Types
import Text.XML.HaXml.Verbatim
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Pretty
import Text.PrettyPrint

import Data.Maybe
import Data.Char

-- debugging only
import Debug.Trace

-- getContent :: Document -> Content
getTopElement (Document _ _ e _) = CElem e

cleanUpStr xs = reverse (cleanUpStrAcc [] xs)

cleanUpStrAcc acc ('\t':xs) = cleanUpStrAcc acc xs
cleanUpStrAcc acc ('\n':xs) = cleanUpStrAcc acc xs
cleanUpStrAcc acc (c:xs)    = cleanUpStrAcc (c:acc) xs
cleanUpStrAcc acc []        = acc

-- interface: read a message
readSCSCPMsg :: String -> SCSCPMsg
readSCSCPMsg str'= let str = cleanUpStr str'
                       parsed  = xmlParse "<server output>" str
                       (interim,_) = fromElem [getTopElement parsed]
                       (OMOBJ _ (OMATTR atps oma)) = fromJust interim
                       -- processing attributes:
                       -- pairing on atps
                       pairs :: [(String,String)]
                       pairs = case atps of
                                 Nothing         -> []
                                 Just (OMATP xs) -> mkPairs xs
                       mkPairs [] = []
                       mkPairs (OneOf2 (oms,OMSTR str):rest)
                           = ( oMSName oms,str):mkPairs rest
                       mkPairs (TwoOf2 (oms, OMI str):rest)
                           = (oMSName oms,str):mkPairs rest
                       -- always there (we hope so...)
                       callId = fromJust (lookup "call_id" pairs) -- HWL: WAS: "call_ID"
                       -- CATime and CAMem (aliased Int) not always there
                       maybeInt key = case lookup key pairs of
                                        Nothing -> Nothing
                                        Just s  -> Just ((read s)::Int)
                       -- attributes for result and error:
                       t = maybeInt "info_runtime"
                       mem = maybeInt "info_memory"
                       -- attributes for call:
                       pct = maybeInt nMaxTime
                       pcmaxmem= maybeInt nMaxMem
                       pcminmem= maybeInt nMinMem
                       pcdebug = maybeInt nDebug
                       pcResult= Nothing -- for now
                       -- processing call/result/error data
                       (tipe,result) = case oma of 
                               OMAOMS_OMR     (x,OMR ref)
                                   -> (oMSName x,toOM (CARef ref))
                               OMAOMS_OME     (x,OME oms (OMSTR err)) 
                                   -> (oMSName x, -- pass errors to HS land
                                       error (oMSName oms ++ ": " ++ err))
                               OMAOMS_EATALL  (x,EAT (Elem n attrs c)) 
                                   -> (oMSName x, -- 
                                       OM n attrs (concatMap verbatim c))
                       caError = case oma of 
                                   OMAOMS_OME (_,OME oms (OMSTR err))
                                     -> case oMSName oms of 
                                          -- scscp1 sez:
                                          "error_memory" 
                                              -> CAMemExhausted
                                          -- "error_CAS" in SCSCP-1.2.pdf
                                          "error_system_specific"
                                              -> CAMsg err
                                          -- runtime exhausted? Spec.unclear
                                          "error_runtime"
                                              -> CATimeExhausted
                       -- callName = Left ("Why decode PCall?",
                       --                  "I want to be a client anyway.")
                       -- The tricky thing here is, the OMA is parsed
                       -- by EATALL. We do not have the nice HaXml
                       -- structures and field names from above.
                       (callName,callRest) =
                         case oma of 
                           OMAOMS_EATALL (x, EAT (Elem n attrs (oms:rest)))
                               -> (case fromElem [oms] of 
                                    (Just ok,_) -> let cd = oMSCd ok
                                                   in if cd == "scscp2" 
                                                      -- all special op.s 
                                                      -- are defined there 
                                                      then Right 
                                                           (decodeOp 
                                                            (oMSName ok))
                                                      else Left (cd,oMSName ok)
                                    other -> Left ("sys error",
                                                   "cannot extract call name: found\n"++(show oma))
                                  , -- FIXME: debug code only, all strings
                                    map contentToOM rest )
                           other -> (Left ("sys error","invalid PCall format")
                                    , [])
                   in -- trace ("!! raw string read is: \n"++str) $
                      case tipe of 
                        "procedure_completed" 
                            -> PResult result callId t mem
                        "procedure_terminated" 
                            -> PTerminated callId caError t mem
                        "procedure_call"
                            -> PCall callId callName callRest
                                   (PCOpts pcResult pct pcminmem 
                                           pcmaxmem pcdebug)
                               -- error ("Why should I decode PCall?" ++ 
                               --       " I am a client, not a server!")
                        other -> error "unexpected SCSCP message type"

scscpPrefix = "<OMOBJ><OMATTR><OMATP>"
-- then the options. we assume call_ID to be always present.
scscpInfix  = "</OMATP><OMA>"
-- then the OMS indicating the type, then the included data, which can
-- be OMA, OME, OMR, or any other result type
scscpSuffix = "</OMA></OMATTR></OMOBJ>"

-----------------------------------------------------------------------------
-- Main routine

-- construct OM encoding of a message
writeSCSCPMsg :: SCSCPMsg -> String -- XML
writeSCSCPMsg a@(PCall id name args opts) 
    = scscpPrefix
      ++ writeOpts id opts -- see HS_SCSCP
      ++ scscpInfix
      ++ "<OMS cd=\"scscp1\" name=\"procedure_call\" />"
      ++ "<OMA><OMS cd=" ++ show cd 
               ++ " name=" ++ show op ++ " />"
      ++ (if null ref 
            then concatMap writeOMObj args
            else ref)
      ++ "</OMA>" 
      ++ scscpSuffix
    where (cd,op,ref) = case name of 
                          Left (cd,n) -> (cd,n,"")
--                           Right UnbindObj 
--                                       -> ("scscp2","unbind",
--                                           writeOMObj (toOM r))
--                           Right RetrieveObj 
--                                       -> ("scscp2","retrieve",
--                                           writeOMObj (toOM r))
                          Right special -> ("scscp2", encodeOp special, bonzo special) -- HWL: CHECK: whether scscp1 or scscp2
                                                 -- decode: see HS_SCSCP
          bonzo GetSignature | length args < 2 = error $ "bonzo: length of arguments to GetSignature too short; should be 2 but is" ++(show (length args))
                             | otherwise       = "<OMS cd=\""++(getContentFromOMObj (args!!0))++"\" name=\"" ++ (getContentFromOMObj (args!!1)) ++ "\"/>" -- HWL: HACK: encode arguments to GetSignature in an OMS (BROKEN)
          bonzo _            = ""

          getContentFromOMObj (OM tag attList []) = error "getContentFromOMObj: empty content in OMObj"
	  getContentFromOMObj (OM tag attList nonemptyContent) = nonemptyContent

writeSCSCPMsg (PTerminated id err t mem) 
    = scscpPrefix
      ++ writeInfos id t mem
      ++ scscpInfix
      ++ "<OMS cd=\"scscp1\" name=\"procedure_terminated\" />"
      ++ "<OME><OMS cd=\"scscp1\" name="
      ++ show (errTypeName err)
      ++ " /><OMSTR>" ++ errText err ++ "</OMSTR>"
      ++ "</OME>" 
      ++ scscpSuffix
writeSCSCPMsg (PResult res id t mem) 
    = scscpPrefix 
      ++ writeInfos id t mem
      ++ scscpInfix
      ++ "<OMS cd=\"scscp1\" name=\"procedure_completed\" />"
      ++ writeOMObj res
      ++ scscpSuffix

-- my own pretty-printer, simple indentation only
indentXml :: String -> String
-- for now hand-rolled: separate each tag in a new line
-- entry, when starting like xml should:
indentXml ('<':rest) = '<':indent 1 rest
    where indent :: Int -> String -> String
          -- end
          indent lvl "" = ""
          -- tag immediately closed:
          indent lvl ('/':'>':rest) = "/>" 
                                      ++ indent (lvl-1) rest
          -- closing tag:
          indent lvl ('<':'/':rest) = '\n':replicate (lvl-1) ' '
                                      ++ "</" ++ 
                                      endTag (lvl-1) rest
          -- opening tag:
          indent lvl ('<':rest) = '\n':replicate lvl ' '
                                  ++ '<':indent (lvl+1) rest
          -- no tag delimiter
          indent lvl(s:rest) = s:indent lvl rest
          endTag l ('>':rest) = '>':indent l rest
          endTag l (s:rest)   = s:endTag l rest
          endTag l [] = "" -- should not happen, though
indentXml other = other -- in fact, not xml
-- later: try using HaXml. Seems to introduce some overhead, though.
-- indentXml = render . document 

