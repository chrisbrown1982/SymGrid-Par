% some latex stuff for typesetting literate code...

% command to wrap around code which should not show
\newcommand{\texignore}[1]{}

% uniform size for all code flavours
\newcommand{\codesize}{\scriptsize}

% code highlighting commands (in text and in own block)
\newcommand{\cd}[1]{{{\codesize \tt{#1}}}}

% code environments, completely verbatim (fancyvrb):
\renewcommand{\FancyVerbFormatLine}[1]{~~~#1}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\codesize, frame=lines,
                                           label=\textit{compiled code}}
\DefineVerbatimEnvironment{icode}{Verbatim}{fontsize=\codesize, frame=lines,
                                           label=\textit{ignored code}}
\DefineVerbatimEnvironment{xcode}{Verbatim}{fontsize=\codesize, frame=lines}

\texignore{% i.e., do not print this...
\begin{code}
{-# OPTIONS_GHC -cpp -XOverlappingInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE IncoherentInstances #-} 
module HS_SCSCP where
\end{code}
}

\author{Jost Berthold, Hans-Wolfgang Loidl, Chris Brown University of St.Andrews (\{hwloidl,jb, chrisb\}@cs.st-andrews.ac.uk)}

\hwlcomment{This documentation is incomplete and needs update before the release!!}

This document was first created to describe first (top-down) 
implementation ideas for
SCSCP in Haskell (client only). Meanwhile, changes have been made 
throughout, and the module now provides types and conversions from
SCSCP-XML to Haskell and vice-versa.
We continue to collect the basic ideas and notes here.

SCSCP\cite{SCSCP-1.2} is a protocol
for interaction between computer-algebra systems, following the
OpenMath standard and developed in the SCIEnce EU project. 
\marginpar{\jbcomment{updated to SCSCP-1.3}}
Basis of
our design is the version 1.2 of the protocol specification.
The content dictionaries (CD) for SCSCP are scscp1 and scscp2.
% and 
% they slightly differ from the SCSCP-1.2 description. When different,
% we follow the scscp CD definition of keywords, and the 
% SCSCP-1.2 spec. for message formats.

For any terminology, pls. refer to the aforementioned specification
and to the content dictionaries.

\section*{Implementing SCSCP in Haskell}

\subsection*{Features summary}

  The goal of the implementation is to send computation requests from
  a Haskell program to a CA system, and to receive the answers for
  further processing.

  Furthermore, the module provides features for starting and stopping
  its respective communication partners, namely SCSCP servers linked
  to CA systems.

  Implementing such an SCSCP client lays the ground for a more
  long-term goal: implementing a coordination server to use several CA
  systems in parallel. Such a server should be able to answer client
  requests, and otherwise coordinate computations which we provide as
  skeletons (and define an according \cd{symgrid-par} content
  dictionary).

\subsection*{Limitations}
\begin{itemize}

\item We want to implement an interface for the SCSCP usage via
  sockets, thus not using a web service.
\item Most likely, not all possible functionality of a client will be
  implemented.  For the implemented features, see the subsequent
  description.
\item Where possible, potentially large computation data should not be
  accessed inside the calling Haskell program. Haskell serves as a
  coordination layer.  Thus, SCSCP messages will only be decomposed to
  a degree which allows to coordinate the respective computation, and
  included computation data otherwise passed on as opaque objects.
\item we do not implement the binary OpenMath format in this version,
  but restrict ourselves to the XML variant.
\end{itemize}

\newpage

This is where the code starts.

Some imports (technical...):
\begin{code}

import Text.XML.HaXml.Types

-- and for parsing in OMObj:
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Verbatim

import Data.Char
import Data.Maybe
import Data.List

-- import the standard types for CA. 
-- currently these are manually defined, should we move
-- towards the Numeric Prelude?
import SGPTypes

import Debug.Trace

import Control.Monad -- for maybe monad

import Control.Parallel.Strategies -- for NFData OMObj
import Control.DeepSeq
#ifdef __PARALLEL_HASKELL__
import Control.Parallel.Eden(Trans)
#endif
\end{code}

\subsection*{SCSCP data objects}

First of all, SCSCP data (in OpenMath XML) needs to be sent and
received, thus stored inside Haskell, and sometimes also accessed from
Haskell code. While we want to avoid it, still we need to extract
basic types such as \cd{Bool}, \cd{Int}, \cd{Integer}, and
\cd{String}.

We store OpenMath objects as their enclosing tag, including potential
attributes, and the contents as verbatim. In this manner, we can
compose the XML from it easily, even without deeper XML knowledge.
For simple types (in type class OMData), we generate the OpenMath
representation ourselves, and can read it out just as easily.

\begin{code}
-- data objects, opaque if at all possible
type Tag = String
data OMObj = OM 
               Tag         -- as name suggests
               [Attribute] -- attribute list
               String      -- embedded XML
    deriving (Eq,Show)

instance NFData OMObj where
    rnf (OM tag attrs content) = rnf (tag,attrs,content)
-- instance NFData Attribute... but Attribute is an alias
-- These guys need to be defined for it.
instance NFData AttValue where 
    rnf (AttValue vals) = rnf vals 
instance NFData Reference where
    rnf (RefEntity ref) = rnf ref
    rnf (RefChar n) = rnf n

#ifdef __PARALLEL_HASKELL__
instance Trans OMObj
#endif

-- to parse in an OMObj, we use the HaXml parser
-- failure is passed to the caller by the contents
parseOM :: String -> OMObj
parseOM input = case xmlParse' "parseOM-internal" input of 
		  Left errmsg -> let err = error ("cannot parse this OMObj: " 
						  ++ take 100 input 
                                                  ++ "...:\n" ++ errmsg)
		                 in OM err err err
		  Right (Document _ _ (Elem tag atts cs) _) 
		              -> OM tag atts (concatMap verbatim cs)

-- without parsing, directly XML elements (HaXml type) to OMObj
elemToOM :: Element -> OMObj
elemToOM (Elem tag atts cs) = OM tag atts (concatMap verbatim cs)
contentToOM :: Content -> OMObj
contentToOM (CElem (Elem tag atts cs)) 
    = OM tag atts (concatMap verbatim cs)
contentToOM (CString _ t) = toOM t        -- encode it as a string
contentToOM (CMisc x) = error "contentToOM: unexpected content" 

-- converting back OMObj as XML. We do it directly, don't know what
-- HaXML would format for us
writeOMObj :: OMObj -> String
writeOMObj (OM tag attList []) 
    | tag == "OMSTR" = "<OMSTR></OMSTR>" -- workaround needed in GAP SCSCP
    | otherwise = '<':tag ++ ' ':writeAttributes attList ++ "/>"
writeOMObj (OM tag attList nonemptyContent)
    = '<':tag ++ ' ':writeAttributes attList 
      ++ ' ':'>':nonemptyContent ++ '<':'/':tag ++ ">"

-- ... but we can convert it back to a HaXml type as well, 
-- ignoring that the content might contain tags:
objToHaXml :: OMObj -> Content
objToHaXml (OM tag attList content) 
    = CElem (Elem tag attList [CString True content])

writeAttributes :: [Attribute] -> String
writeAttributes = unwords . map showAttribute

showAttribute :: Attribute -> String
showAttribute (name, val) 
    = name ++ "=" ++ show val

-- Attribute is an alias, thus not instance of Show... 
-- These guys need to be defined for it.
instance Show AttValue where 
    show (AttValue vals) = "\"" ++ concatValStrings vals ++ "\""

concatValStrings :: [Either String Reference] -> String
concatValStrings [] = ""
concatValStrings ((Left s):rest)    = s ++ concatValStrings rest
concatValStrings ((Right ref):rest) = show ref ++ concatValStrings rest

instance Show Reference where 
    show (RefEntity ref) = '&':ref ++ ";"
--    show (RefChar n)) = "\"&#" ++ show n ++ ";\""
    show (RefChar n) = "&#x" ++ hex n ++ ";"

-- 
hex :: Int -> String
hex n | n < 0 = hex (n `mod` 256) -- HACK
      | n < 16 = c:""
      | otherwise = hex rest ++ digits!!last : ""
    where rest  = n `div` 16
          last  = n `mod` 16
          c     = digits!!n
          digits= "0123456789ABCDEF"
\end{code}

We want to pass around OM objects inside Haskell, but 
not inspect them (see above). Certain simple Haskell 
types need to be convertible to OMObj (which we do 
straight-forward by hand,), we define a type class 
and instances for the conversion here.

\begin{code}

-- A type class for Haskell data types which can be embedded in an
-- SCSCP message.
class OMData a where 
    toOM   :: a -> OMObj
    fromOM :: OMObj -> a

instance OMData OMObj where 
    toOM   = id
    fromOM = id

-- 	<OMS cd="logic1" name="true"/>
instance OMData Bool where
    toOM b = OM "OMS" atts "" 
        where atts :: [Attribute]
              atts =  [("cd"  ,AttValue [Left "logic1"]),
                       ("name",AttValue [Left val]     ) ]
              val = if b then "true" else "false"
    fromOM (OM "OMS" attribs "" ) 
        | (cd `elem` attribs) 
          && isJust attribute = decode val
      where attribute = lookup "name" attribs 
            AttValue ((Left val):_) = fromJust attribute
            cd = ("cd",(AttValue [Left "logic1"]))
            decode "true"  = True
            decode "false" = False
    fromOM other = error ("cannot read Boolean from " ++ show other)

instance OMData Int where
    toOM i    = OM "OMI" [] (show i)
    fromOM (OM "OMI" [] num) = read num
    fromOM other = error ("cannot read Int from " ++ show other)
instance OMData Integer where
    toOM i    = OM "OMI" [] (show i)
    fromOM (OM "OMI" [] num) = read num
    fromOM other = error ("cannot read Integer from " ++ show other)

instance OMData String where
    toOM s    = OM "OMSTR" [] (encodeXML s)
    fromOM (OM "OMSTR" [] txt) = txt
    fromOM other = error ("cannot treat " ++ show other ++ " as String")

{-instance OMData MatrixRow where
    toOM (MatrixRow ariths) = res
              where res = OM "OMA" [] (concat (map writeOMObj ([OM "OMS" [("cd", AttValue [Left "linalg2"]),
                                                                          ("name", AttValue [Left "matrixrow"])] "", toOM ariths])))

    fromOM (OM "OMA" [] listXML) =
         case parsed of
           Left msg -> passError msg
           Right (Document _ _ (Elem oma [] ((CElem lSym):elems)) _)
            -> scrut lSym elems
       where -- use xmlParse' to deconstruct
               scrut lSym elems
                 | isMatrixRowSym lSym = (MatrixRow (mkF elems))


                 | otherwise = passError "Not a MatrixRow element"
                  


               parsed = xmlParse' "list-parse" 
                            ("<OMA>" ++ trim listXML ++ "</OMA>")
                  -- String trimming (remove white). definition below.
               mkF e = (map (fromOM . parseOM) . 
                           filter (not . all isSpace) . -- rule out \n
                           map verbatim) e
               passError msg = error ("cannot parse as a matrixRow:"
                                         ++ take 100 listXML 
                                         ++ "..: " ++ msg)
    fromOM other = error ("cannot read matrixRow from " ++ show other) -}


-- added by Chris Brown 9 Sept 2010
-- convert to numeric prelude?
instance OMData Arith where
    toOM (Matrix rows) = OM "OMA" [] (concat (matrixOMS : map (writeOMObj . toOM) rows))

    toOM (MatrixRow ariths) = OM "OMA" [] (concat (matrixRowOMS : map (writeOMObj . toOM) ariths)) 

    toOM (Num x) = OM "OMI" [] (show x)
    toOM (Mul x y) = OM "OMA" [] (concat (map writeOMObj ([OM "OMS" [("cd", AttValue [Left "arith1"]),
                                                                     ("name", AttValue [Left "times"])] "", toOM x, toOM y])))

    toOM (Power x y) = OM "OMA" [] (concat (map writeOMObj ([OM "OMS" [("cd", AttValue [Left "arith1"]),
                                                                       ("name", AttValue [Left "power"])] "", toOM x, toOM y])))

    fromOM (OM "OMI" [] num) = Num (read num)
    fromOM (OM "OMA" [] listXML) =
         case parsed of
           Left msg -> passError msg
           Right (Document _ _ (Elem oma [] ((CElem lSym):elems)) _)
            -> scrut lSym elems
       where -- use xmlParse' to deconstruct
               scrut lSym elems
                 | isArithMulSym lSym = (Mul (head (mkF [head elems])) (head (mkF [head (tail elems)])))
                 | isArithPowerSym lSym =(Power (head (mkF [head elems])) (head (mkF [head (tail elems)])))
                 | isMatrixSym lSym = (Matrix (mkF elems))
                 | isMatrixRowSym lSym = (MatrixRow (mkF elems))
                 | isListSym lSym = (MatrixRow (mkF elems))
                 | otherwise = passError "Not an Arithmetic element"
                  


               parsed = xmlParse' "list-parse" 
                            ("<OMA>" ++ trim listXML ++ "</OMA>")
                  -- String trimming (remove white). definition below.
               mkF e = (map (fromOM . parseOM) . 
                           filter (not . all isSpace) . -- rule out \n
                           map verbatim) e
               passError msg = error ("cannot parse as an arith:"
                                         ++ take 100 listXML 
                                         ++ "..: " ++ msg)
    fromOM other = error ("cannot read Int from " ++ show other)



instance OMData FiniteField where
    toOM (PrimEl x) = res 
         where res = OM "OMA" [] ((concat (map writeOMObj [OM "OMS" att1 "", toOM x])))
               att1 :: [Attribute]
               att1 = [("cd", AttValue [Left "finfield1"]),
                       ("name", AttValue [Left "primitive_element"] )]
    fromOM (OM "OMA" [] listXML) =
            case parsed of 
              Left msg -> passError msg
              Right (Document _ _ (Elem oma [] ((CElem lSym):elems)) _)
                       -> if isFiniteSym lSym
                               then (PrimEl (head (mkFinite elems)))
                          else passError "Not a finite field"
            where -- use xmlParse' to deconstruct
                  parsed = xmlParse' "list-parse" 
                            ("<OMA>" ++ trim listXML ++ "</OMA>")
                  -- String trimming (remove white). definition below.
                  mkFinite  = map (fromOM . parseOM) . 
                           filter (not . all isSpace) . -- rule out \n
                           map verbatim 
                  passError msg = error ("cannot parse as a list:"
                                         ++ take 100 listXML 
                                         ++ "..: " ++ msg)
encodeXML :: String -> String
encodeXML "" = ""
encodeXML ('&':s) = "&amp;" ++ encodeXML s
encodeXML ('<':s) = "&lt;" ++ encodeXML s
encodeXML ('>':s) = "&gt;" ++ encodeXML s
encodeXML (c:s) | isPrint c = c:encodeXML s
                | otherwise = "&#" ++ show (ord c) ++ ';':encodeXML s


-- todo: a list instance would perhaps be very handy. 
-- Problems:
--  1. we need to decompose/chop the string at its element delimiters
--  2. (more severe) we could get back a weird list which leads to a
--     type error (list with holes, different types).
--  3. we want a separate instance for String. So we add the flag
--     -XOverlappingInstances -fallow-incoherent-instances

-- added Chris Brown 11 August 2010
-- added provision to allows marshalling of matrices from GAP
-- a matrix is a [[a]] component

instance OMData a => OMData [a] where 
    toOM xs = res
        where res = OM "OMA" [] 
                     (concat (listOMS: map (writeOMObj . toOM) xs))
    -- no, we don't!
    -- fromOM _ = error "list conversion from OM not supported"
    -- or should we?
    fromOM (OM "OMA" [] listXML) 
             = case parsed of 
                 Left msg    -> passError msg
                 Right (Document _ _ (Elem oma [] ((CElem lSym):elems)) _)
                             -> if isMatrixSym lSym 
                                 then mkList elems
                                 else 
                                  if isListSym lSym 
                                   then mkList elems 
                                   else passError "not a list1"
                 Right other -> passError "not a list2"
            where -- use xmlParse' to deconstruct
                  parsed = xmlParse' "list-parse" 
                            ("<OMA>" ++ trim listXML ++ "</OMA>")
                  -- String trimming (remove white). definition below.
                  mkList = map (fromOM . parseOM) . 
                           filter (not . all isSpace) . -- rule out \n
                           map verbatim 
                  passError msg = error ("cannot parse as a list:"
                                         ++ take 100 listXML 
                                         ++ "..: " ++ msg)
    fromOM other = error ("list-parse: " ++  take 100 (show other)
                          ++ "...\n is not a list.")

listSymbol = [("cd",(AttValue [Left "list1"])),
              ("name",(AttValue [Left "list"]))]

finiteSymbol = [("cd", (AttValue [Left "finfield1"])), ("name", (AttValue [Left "primitive_element"]))]
matrixSymbol = [("cd", (AttValue [Left "linalg2"])), ("name", (AttValue [Left "matrix"]))]
matrixRowSymbol = [("cd", (AttValue [Left "linalg2"])), ("name", (AttValue [Left "matrixrow"]))]
arithPowerSym = [("cd", (AttValue [Left "arith1"])), ("name", (AttValue [Left "power"]))]
mulPowerSym   = [("cd", (AttValue [Left "arith1"])), ("name", (AttValue [Left "times"]))]

listOMS = verbatim (Elem "OMS" listSymbol [])
matrixOMS = verbatim (Elem "OMS" matrixSymbol [])
matrixRowOMS = verbatim (Elem "OMS" matrixRowSymbol [])
isListSym (Elem "OMS" s []) | s == listSymbol || s == matrixRowSymbol  = True
                            | otherwise        = error ("isListSym:" ++ (show s))
isListSym other = False

isArithPowerSym (Elem "OMS" s []) = s == arithPowerSym
isArithPowerSym _ = False

isArithMulSym (Elem "OMS" s []) = s == mulPowerSym
isArithMulSym _ = False

isMatrixSym (Elem "OMS" s []) = s == matrixSymbol
isMatrixSym other    = False

isMatrixRowSym (Elem "OMS" s []) = s == matrixRowSymbol
isMatrixRowSym other = False

isFiniteSym (Elem "OMS" s []) = s == finiteSymbol
isFiniteSym other    = False


\end{code}

\subsection*{SCSCP message data type}

The following messages are described by the SCSCP spec., and mapped to
a Haskell type (containing useful other types for encoding).

\begin{code}
-- aliases
type CallID = String
type CATime = Int -- ?too coarse?
type CAMem  = Int

-- type for CA references (stored inside the server)
newtype CARef  = CARef String -- to be embedded in an OMR object
    deriving (Eq,Read,Show)

instance NFData CARef where
    rnf (CARef s) = rnf s
-- no instance Trans, should stay local!


instance OMData CARef where 
    toOM (CARef s)    = OM "OMR" [("xref",AttValue [Left s])] "" 
    fromOM (OM "OMR" attribs "" ) = CARef xref 
        where attribute = lookup "xref" attribs 
              AttValue ((Left xref):_) = fromJust attribute 
    fromOM other = error ("cannot treat " ++ show other ++ " as OM-ref.") 

-- type for commands to execute. Either a standard operation (defined
-- in scscp2 CD), or a server-provided operation encoded as
-- (cd-name,op-name)
type CAName = Either (String,String) CAStandardName

data SCSCPMsg = -- to CA System
                PCall { pcCallID :: CallID
                      , pcName :: CAName
                      , pcData :: [OMObj] 
                      , pcOpts :: ProcOptions }
              | PInterrupt -- empty content (!! can cause race condition !!)
              -- from CA System
              -- procedure complete: called Proc.Result here
              | PResult { prResult :: OMObj
                        , prCallID :: CallID
                        , prTime   :: Maybe CATime
                        , prMem    :: Maybe CAMem }
              | PTerminated { ptCallID :: CallID
                            , ptReturned :: CAError
                            , ptTime :: Maybe CATime
                            , ptMem  :: Maybe CAMem }
               deriving (Show)

-- we will often need to extract the callID immediately:
callID :: SCSCPMsg -> CallID
callID (PCall      cid  _  _ _) = cid
callID (PResult     _  cid _ _) = cid
callID (PTerminated cid _  _ _) = cid

-- encoded procedure options for PCall
data ProcOptions = PCOpts { pcResult :: Maybe PResultOption
                          , pcMaxTime :: Maybe CATime
                          , pcMinMem  :: Maybe CAMem
                          , pcMaxMem  :: Maybe CAMem 
                          , pcDebug :: Maybe Int }
               deriving (Read,Show)

-- scscp1 defines the following names: option_debuglevel,
-- option_max_memory, option_min_memory, option_runtime
nMaxTime= "option_runtime"
nMaxMem = "option_max_memory"
nMinMem = "option_min_memory"
nDebug  = "option_debuglevel"

data PResultOption = Result | ResultRef | NoResult
               deriving (Read,Show)

-- defined following scscp1: option_return_cookie,
-- option_return_nothing, option_return_object,
decodeResultOption :: String -> PResultOption
decodeResultOption "option_return_nothing" = NoResult
decodeResultOption "option_return_cookie"  = ResultRef
decodeResultOption "option_return_object"  = Result
-- decodeResultOption other = Result -- defaulting
decodeResultOption other = error ("not a result option:" ++ other)

encodeResultOption :: PResultOption -> String -- XML
encodeResultOption NoResult  = "option_return_nothing"
encodeResultOption ResultRef = "option_return_cookie" 
encodeResultOption Result    = "option_return_object"

defaultProcOptions = PCOpts (Just Result) Nothing Nothing Nothing Nothing
-- HWL: WAS:
-- defaultProcOptions = PCOpts Nothing Nothing 
--                      Nothing Nothing Nothing

writeOpts :: CallID -> ProcOptions -> String 
writeOpts callId (PCOpts res maxtime minmem maxmem debug)
    = prefix ++ "\"call_id\" /><OMSTR>"  -- HWL: WAS: "call_ID"
             ++ encodeXML callId ++ "</OMSTR>"
      ++ maybeResOpt ++ 
      concat (zipWith maybeWriteInt 
              [nMaxTime,nMinMem,nMaxMem,nDebug]
              [maxtime,minmem,maxmem,debug])
  where maybeResOpt = case res of 
                        Nothing -> ""
                        Just r  -> prefix 
                                   ++ show (encodeResultOption r)
                                   ++ " /><OMSTR></OMSTR>"
        maybeWriteInt name value = case value of 
                                   Nothing -> ""
                                   Just i  -> prefix ++ show name 
                                              ++ " />"
                                              ++ writeOMObj 
                                                  (toOM (i::Int))
        prefix = "<OMS cd=\"scscp1\" name="

-- encoded errors inside PTerminated. The error type is determined
-- from scscp1 definitions, and the error message assembled
-- accordingly. We have taken these ones from the SCSCP-1.2 spec.
data CAError = CAMsg String -- all-purpose
             | CAMemExhausted
             -- rest: future use, currently not in scscp CDs
             | CAInvalidRef -- CARef 
             | CAInterrupted
             | CANoSuchProc CAName
             | CATimeExhausted
             | SystemError String -- for our own error(s)
               deriving (Read,Show)
-- scscp1 CD and SCSCP-1.2 spec. are a little inconsistent, however:
-- the CD only contains 3 names, which do not clearly map to these
-- different error types.
errTypeName :: CAError -> String
errTypeName CAMemExhausted = "error_memory"
errTypeName CATimeExhausted= "error_runtime"
errTypeName (CAMsg _)      = "error_system_specific"
-- other error types go here in the future... 
errTypeName _ = "error_system_specific"

errText :: CAError -> String
-- clearly, pass on errors from CA system:
errText (CAMsg msg)      = msg
-- I added sensible text for these, unsure. The messages should follow
-- a canonical format if there is one:
errText (CAInvalidRef ) -- (CARef r)) 
                         = "invalid reference " -- ++ r
errText (CANoSuchProc (Left (cd,name))) 
                         = "procedure " ++ name 
                           ++ " does not exist in " ++ cd
-- this would need no text, if scscp1 defined an own error type
errText CAInterrupted    = "Interrupted"
errText CAMemExhausted   = ""
-- SCSCP-1.2 spec gives an example which implies:
--errText CAMemExhausted   = "Exceeded the permitted memory"
errText CATimeExhausted  = ""
errText (SystemError s)  = "sys error:" ++ s

-- writing out add. information for PTerminated and PResult messages.
-- Would be needed for a server... we add it for completeness.
writeInfos :: CallID -> Maybe CATime -> Maybe CAMem -> String
writeInfos id t mem 
    = "<OMS cd=\"scscp1\" name=\"call_id\" />"  -- HWL: was call_ID
      ++ "<OMSTR>" ++ id ++ "</OMSTR>"
      ++ maybeTime ++ maybeMem
    where maybeTime = case t of 
                        Nothing -> ""
                        Just t  -> "<OMS cd=\"scscp1\" " 
                                   ++ "name=\"info_runtime\" />"
                                   ++ writeOMObj (toOM t)
          maybeMem = case mem of 
                        Nothing -> ""
                        Just m  -> "<OMS cd=\"scscp1\" " 
                                   ++ "name=\"info_memory\" />"
                                   ++ writeOMObj (toOM m)


\end{code}

SCSCP assumes initial exchange of technical information messages (see
below), after which a sequence of dialogs between the client and SCSCP
server is performed, where the client sends a sequence of \cd{PCall}
messages, and the server responds each of them, in their original
order, with a corresponding \cd{PResult} or \cd{PTerminated} message.
The specification describes \cd{CallID} as a convenience and debug
feature only.

Clients can also send an \cd{PInterrupt} message, containing no
data. Its semantics on receiver (server) side is to stop the
\emph{current} procedure immediately, further ones might be in the
message queue of the server (and are not affected), there is no way of
addressing one of the several \cd{PCalls} using \cd{PInterrupt}, and
the server immediately reacts.

\subsection{Assumed standard procedures}

SCSCP assumes a set of standard procedures (e.g. to request supported
operations from a server), which we encode as follows and may be used
as \cd{pcName} in a \cd{PCall}.

\begin{code}
data CAStandardName = GetAllowedHeads -- returns "symbol_sets" 
                    | GetSignature    -- HWL: could encode cd and name as args here
                                      -- returns "signature"
                                      -- (OMSymbol,minarg,maxarg,(list of) symbol_sets )
                                      --      JB: pTerminated if not supported? Guess so.
                    | GetTransientCD  -- returns server-specific Content Dictionary
                                      --      (should prefix SCSCP_transient_)
                    | GetServiceDescr -- returns "service_description", 
		                      -- containing 3 Strings: 
                                      --      CA system name, version, and descriptive text
                    | StoreObj           -- computes an object, stores it and returns CARef
                    | RetrieveObj -- CARef  -- takes ref, returns the OMObj
                    | UnbindObj -- CARef    -- deletes a CARef'ed object from the server
               deriving (Eq,Read,Show)

-- this follows the scscp2 CD as published online
encodeOp :: CAStandardName -> String
encodeOp GetAllowedHeads = "get_allowed_heads" 
encodeOp GetSignature    = "get_signature"
encodeOp GetTransientCD  = "get_transient_cd"
encodeOp GetServiceDescr = "get_service_description"
encodeOp StoreObj        = "store"
encodeOp RetrieveObj     = "retrieve" -- CARef to be added!
encodeOp UnbindObj       = "unbind" -- deletes a CARef'ed, to be added!

decodeOp :: String -> CAStandardName
decodeOp "get_allowed_heads" = GetAllowedHeads
decodeOp "get_signature" = GetSignature 
decodeOp "get_transient_cd" = GetTransientCD
decodeOp "get_service_description" = GetServiceDescr
decodeOp "store" = StoreObj
decodeOp "retrieve" = RetrieveObj 
                      -- (RetrieveObj undefined) -- CARef to be added!
decodeOp "unbind"   = UnbindObj 
                      -- (UnbindObj undefined)   -- CARef to be added!
decodeOp other = error ("not a standard name: " ++ other)
\end{code}

These routines are called when handling the corresponding CAStandardName:

\begin{code}
constructServiceDescr name version descr = 
 OM "OMA" atts (Data.List.concat (Data.List.intersperse "\n" (map writeOMObj [(toOM name), (toOM version), (toOM descr)])))
        where atts :: [Attribute]
              atts =  [("cd"  ,AttValue [Left "scscp2"]),
                       ("name",AttValue [Left "service_description"]     ) ]

\end{code}

\textbf{TODO:} constructor functions for all  CAStandardName

\subsection*{The XML format specification}

Part 4 of the spec. contains an informal specification of the
messages, which we reproduce here for documentation.
\begin{itemize}
\item Procedure Call (\cd{PCall})
\begin{xcode}
<OMOBJ>
 <OMATTR>
 <!-- beginning of attribution pairs -->
  <OMATP>
   <OMS cd="scscp1" name="call_id" /> 
   <OMSTR>call_identifier</OMSTR> 
   <OMS cd="scscp1" name="option_runtime" /> 
   <OMI>runtime_limit_in_milliseconds</OMI> 
   <OMS cd="scscp1" name="option_min_memory" /> 
   <OMI>minimal_memory_required_in_bytes</OMI> 
   <OMS cd="scscp1" name="option_max_memory" /> 
   <OMI>memory_limit_in_bytes</OMI> 
   <OMS cd="scscp1" name="option_debuglevel" /> 
   <OMI>debuglevel_value</OMI> 
   <OMS cd="scscp1" name="option_return_object" />
   <!-- another possibility is "option_return_cookie" -->
   <OMSTR></OMSTR>
  </OMATP>
  <!-- Attribution pairs finished, now the procedure call -->
  <OMA>
   <OMS cd="scscp1" name="procedure_call" />
   <OMA>
    <!-- "SCSCP_transient_" is an obligatory prefix
         in the name of a transient CD -->
    <OMS cd="SCSCP_transient_identifier"
        name="NameOfTheProcedureRegisteredAsWebService" />
    <!-- Argument 1 -->
    <!-- ... -->
    <!-- Argument M -->
   </OMA> 
  </OMA>
 </OMATTR>
</OMOBJ>
\end{xcode}
 \jbcomment{So why are these call attributes not XML attributes? 
 Must be because OpenMath had to be left unmodified. IMHO, an extension by 
 a new tag would have been the way to go... including all PC attributes as 
 real XML attributes, and containing the argument list only}

\item Interrupt Signal (\cd{PInterrupt}): no content at all.

\item Procedure Completed (\cd{PResult})
\begin{xcode}
<OMOBJ>
 <OMATTR>
  <!-- Attribution pairs, dependently on the debugging level
       may include procedure name, OM object for the original
       procedure call, etc. -->
  <OMATP>
   <OMS cd="scscp1" name="call_id" />
   <OMSTR>call_identifier</OMSTR>
   <OMS cd="scscp1" name="info_runtime" />
   <OMI>runtime_in_milliseconds</OMI>
   <OMS cd="scscp1" name="info_memory" />
   <OMI>used_memory_in_bytes</OMI>
  </OMATP>
  <!-- Attribution pairs finished, now the result -->
  <OMA>
   <OMS cd="scscp1" name="procedure_completed" />
   <!-- The result itself, may be OM symbol for cookie -->
   <!-- OM_object_corresponding_to_the_result -->
  </OMA>
 </OMATTR>
</OMOBJ>
\end{xcode}

And for referenced data (stored in CA), the following (called a cookie):
\begin{xcode}
<OMOBJ>
 <OMATTR>
  <OMATP>
   <OMS cd="scscp1" name="call_id" />
   <OMSTR>call_identifier</OMSTR>
  </OMATP>
  <OMA>
   <OMS cd="scscp1" name="procedure_completed"/>
   <OMR xref="CAS_variable_identifier" />
  </OMA>
 </OMATTR>
</OMOBJ>
\end{xcode}

\item Procedure Terminated (\cd{PTerminated})
\begin{xcode}
<OMOBJ>
 <OMATTR>
  <!-- beginning of attribution pairs -->
  <OMATP>
   <OMS cd="scscp1" name="call_id" />
   <OMSTR>call_identifier</OMSTR>
   <OMS cd="scscp1" name="info_runtime" />
   <OMI>runtime_in_milliseconds</OMI>
   <OMS cd="scscp1" name="info_memory" />
   <OMI>used_memory_in_bytes</OMI>
  </OMATP>
  <!-- end of attribution pairs -->
  <!-- now the application part of the OM object -->
  <OMA>
   <OMS cd="scscp1" name="procedure_terminated" />
   <OME>
    <OMS cd="scscp1" name="name_of_standard_error"/>
    <!-- Error description depends on error type -->
    <OMSTR>Error_message</OMSTR>
   </OME>
  </OMA>
 </OMATTR>
</OMOBJ>
\end{xcode}
\end{itemize}

\subsection*{Message exchange between client and server}

This is mostly done using XML processing instructions. 
The initialisation is a sequence of messages as follows, where 
things like attribute order and format is strictly fixed.

Examples: 
\begin{xcode}
Server -> Client:
<?scscp service_name="MuPADserver" service_version="1.1"
service_id="host:26133" scscp_versions="1.0 3.4.1 1.2special" ?>

Client -> Server:
<?scscp version="1.0" ?>

Server -> Client:
<?scscp version="1.0" ?>
\end{xcode}
\begin{xcode}
Server -> Client:
<?scscp service_name="MuPADserver" service_version="1.1"
service_id="host:26133" scscp_versions="1.0 3.4.1 1.2special" ?>

Client -> Server:
<?scscp version="2.0" ?>

Server -> Client:
<?scscp quit reason="not supported version 2.0" ?>

OR JUST: <?scscp quit ?>
\end{xcode}

The actual data messages are enclosed in processing instruction blocks:
\begin{xcode}
<?scscp start ?>
... message (OpenMath object), formats see above...
<?scscp end ?>
\end{xcode}
The exception is the interrupt signal, which is just SIGUSR2 to the server.

Messages can be canceled using \verb!<?scscp cancel ?>! to close the block. 
The server should not process the message.

Sessions are terminated using \verb!<?scscp quit ?>!, optionally
giving a reason as above.

\begin{code}
-- PIs as a data type:
data SCSCP_PI = Init { piInitName :: String
                     , piInitV    :: String
                     , piInitID   :: String
                     , piInitSCSCPs :: [String]}
              | Version String
              | Quit (Maybe String)
              | Start | End | Cancel
              | Other String -- catch-all... (for kant extensions)
       deriving (Eq,Show)

piPrefix = "<?scscp"

writePI :: SCSCP_PI -> String
writePI Start  = piPrefix ++ " start ?>"
writePI End    = piPrefix ++ " end ?>"
writePI Cancel = piPrefix ++ " cancel ?>"
writePI (Quit Nothing)    = piPrefix ++ " quit ?>"
writePI (Quit (Just txt)) = piPrefix ++ " quit reason=" ++ show txt ++ " ?>"
writePI (Version ver)     = piPrefix ++ " version=" ++ show ver ++ " ?>"
writePI (Init n v iD vs)  = piPrefix 
                            ++ " service_name=" ++ show n
                            ++ " service_version=" ++ show v
                            ++ " service_id=" ++ show iD
                            ++ " scscp_versions=" ++ show (unwords vs) 
                            ++ " ?>"
writePI (Other msg) = piPrefix ++ ' ':msg ++ " ?>"

parsePI :: String -> SCSCP_PI
parsePI ('<':'?':'s':'c':'s':'c':'p':' ':more) 
    = parse' (init ( init (dropWhile isSpace more)))
parsePI other = error ("cannot parse this as a PI: " ++ other)
{- <?scscp start ?>
   <?scscp cancel ?> 
   <?scscp end   ?>
   <?scscp quit reason="blabla" ?>
   <?scscp version="X" ?>
   <?scscp service_name="A" service_version="B" 
           service_id="C" scscp_versions="D" ?>
-- libkant 4.0 uses these as well: 
   <?scscp error text="blabla" ?>
   <?scscp info text="blabla" ?>
-}
parse' ('s':'t':'a':'r':'t':_)     = Start
parse' ('c':'a':'n':'c':'e':'l':_) = Cancel
parse' ('e':'n':'d':_)             = End
parse' ('q':'u':'i':'t':rest)
    | all isSpace rest = Quit Nothing
    | otherwise        = Quit text
      where text = case dropWhile isSpace rest of 
              ('r':'e':'a':'s':'o':'n':'=':'\"':t) 
                  -> Just (takeWhile (not . isQ) t)
              other -> Nothing -- invalid PI, cannot parse.
parse' ('v':'e':'r':'s':'i':'o':'n':'=':'\"':ver)
    = Version (takeWhile (not . isQ) ver)
parse' ('s':'e':'r':'v':'i':'c':'e':'_':'n':'a':'m':'e':'=':'\"':rest)
    -- init message, we assume the attribute order given in the spec.
    = let (name,r2) = splitAndTrimWhen isQ rest
           -- drop "service_version=\""
          r3        = tail' (dropWhile (not . isQ) r2)
          (v,r4)    = splitAndTrimWhen isQ r3
           -- drop "service_id=\""
          r5        = tail' (dropWhile (not . isQ) r4)
          (iD,r6)   = splitAndTrimWhen isQ r5
           -- drop "scscp_versions=\""
          r7        = tail' (dropWhile (not . isQ) r6)
          vList = words (takeWhile (not . isQ) r7)
          -- yes, I know this is the state monad.
      in Init name v iD vList
-- parse' ('e':'r':'r':'o':'r':' ':'t':'e':'x':'t':'=':'\"':rest)
--    = Other "error text =\"" ++ rest
parse' other = Other other -- return anything as-is otherwise

\end{code}

\section*{Some string search and trim operations (helpers)}
\begin{code}
-- cut a string at the first char. for which a predicate holds,
-- dropping the last character and trimming the rest of whitespaces.
splitAndTrimWhen :: (Char -> Bool) -> String -> (String,String)
splitAndTrimWhen pred "" = ("","")
splitAndTrimWhen pred (c:cs) | pred c = ("",dropWhile isSpace cs)
                             | otherwise 
                                 = let (a,b) = splitAndTrimWhen pred cs
                                   in (c:a,b)
tail' :: [a] -> [a] -- never fails
tail' [] = []
tail' xs = tail xs
isQ :: Char -> Bool
isQ '\"' = True
isQ '\'' = True
isQ _ = False


-- boyer-moore implementation. might be useful later.
boyerMoore :: String -> String -> Maybe Int -- position of first match
boyerMoore pattern searched = bm searched 0
    where bm str i | null rest    = if match == pattern 
                                     then Just i else Nothing
                   | and matching = Just i -- found it
                   | otherwise    = bm skipped (i+skip firstBad)
                   where (match,rest) = splitAt l str
                         matching = zipWith (==) pattern match 
                         firstBad = fst (last notMatched)
                         notMatched = filter (not . snd) 
                                         (zip match matching)
                         skipped = drop (skip firstBad) str
          -- simplified, we only use the "bad-pattern" rule
          skip c = length (takeWhile (/= c) rPat)
          rPat = reverse pattern
          l = length pattern

trim :: String -> String
trim = dropWhile isSpace 

\end{code}

\newpage

\section*{Module overview of the Haskell SCSCP client}

\begin{center}
\includegraphics[width=0.9\textwidth]{modules}
\end{center}

The following modules are combined to the SCSCP API:
\begin{itemize}
  \item[\cd{SCSCP\_DTD}] conversion from XML to an intermediate data
    type.\\ 
%
    This file is generated from \cd{SCSCP.dtd} via DTDtoHaskell
    (HaXml), and then retouched to avoid decomposing objects deeper
    than necessary.
  \item[\cd{Hs2SCSCP}] Conversion from Haskell structures to SCSCP XML
    and vice-versa. Uses the \cd{SCSCP\_DTD} as intermediate data.
  \item[\cd{HS\_SCSCP}] This file. Provides all SCSCP-relevant data
    types, no functionality.
  \item[\cd{SCSCP\_API}] server access functions and start/stop
    actions, functions for SCSCP evaluation.
\end{itemize}

\section*{System architecture intended for SymGrid-Par}

\hwlcomment{A more detailed discussion of the SymGrid-Par 
 design is given in my overview talk at the SCIEnce workshop in October.}

\begin{center}
\includegraphics[width=0.7\textwidth]{architecture1}

\bigskip
\includegraphics[width=0.7\textwidth]{architecture2}
\end{center}

\subsection*{Coordination Server}

\hwlcomment{Documentation to be filled in}

% The coordination server uses techniques which we have tested in
% the SCSCP client, and completely rely on the \cd{Hs2SCSCP.hs} for
% SCSCP.

% -----------------------------------------------------------------------------
% TODO: check whether any of this is still relevant

% \newpage

% {\huge Junkyard: outdated material }

% {\bf Notes on SCSCP specification and examples }

% \begin{itemize}

% \item error message data unclear:

%       The description of error messages (procedure terminated)
%       differs from the given examples, in that the examples
%       do not give runtime and memory.
%       The text says that these will be included in an
%       error message.

% \item error messages under-specified in scscp1 CD and description: 
      
%       The textual description of error messages categorises a few
%       error types, which is not reflected by the scscp1 CD and neither
%       the examples given later. 

%       The following is missing from scscp1:
%       ``terminated by interrupt'', ``invalid cookie'', 
%       ``ran out of resource(time)''.

%       The exact meaning of ``procedure not supported'' and 
%       of ``cannot compute OM Object'' remains unclear.

      

% \item invalid XML in example B.3:

%       Example B.3 shows an error message from the CA system 
%       (actually, GAP) which contains literally 
%       \verb!<function>(<arguments>)!. As one can expect, this 
%       causes an XML parse error (parsing tags named ``function''
%       and ``argument'' instead of plain text).
%       The CA system should either wrap any unchecked text in 
%       the OMSTR into \verb!<[[CDATA  the-unchecked-text ]]>! or (better) 
%       encode $<$ and $>$ as necessary for XML.
%       The website describing the scscp1 CD does the latter, 
%       correctly.

%       Just a flaw in the example, or a problem of the gap server?


% \item inconsistency between scscp1 CD and Spec. 1.2 examples:

%   The example for proc.terminated with a CA system error given in the
%   SCSCP-1.2.pdf uses \verb!name="error_CAS"!, whereas the scscp1
%   CD specifies a \verb!error_system_specific!.

% \item (Question?) isn't the call\_id mandatory, and thereby
%   OMATTR/OMATP a required content for any SCSCP message?

%   SCSCP-1.2.pdf, p.4 bottom, reads as if it was mandatory. 

%   Examples for \verb!error_*! in scscp1 CD do not give any OMATP,
%   nor do they show the expected ``procedure\_terminated'' frame
%   inside which one will expect the error types to appear.

% \end{itemize}
