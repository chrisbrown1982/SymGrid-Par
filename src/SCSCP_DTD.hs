{-
-- Time-stamp: <Thu Mar 25 2010 22:33:01 Stardate: Stardate: [-28]2819.69 hwloidl>

SCSCP Messages to Haskell conversion module.

This code has been generated from a custom DTD for SCSCP messages (not
taking into account the generality of OpenMath, but tailored to SCSCP
messages parsed 

After code generation, the code has been retouched at the marked
places to enable the conversion we want (not parsing into the
arguments/results, but storing them as an opaque OpenMath object (OMO
String)

We also provide additional functions :: OMOBJ -> SCSCPMsg which
convert the parsed data from the intermediate type to the one we want
to use, and vice-versa.

All SCSCP Haskell types are imported from HS_SCSCP (todo: rename!).

Author: Jost Berthold, University of St.Andrews, UK
(c) 12/2008

-}

module SCSCP_DTD where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)

-- edited: verbatim instance for dummy type
import Text.XML.HaXml.Verbatim

{-Type decls-}

data OMOBJ = OMOBJ OMOBJ_Attrs OMATTR
           deriving (Eq,Show)
data OMOBJ_Attrs = OMOBJ_Attrs
    { oMOBJXmlns'OM :: (Maybe String)
    , oMOBJXmlns'xsi :: (Maybe String)
    , oMOBJXsi'schemaLocation :: (Maybe String)
    , oMOBJVersion :: (Maybe String)
    } deriving (Eq,Show)
data OMATTR = OMATTR (Maybe OMATP) OMA
            deriving (Eq,Show)
data OMS = OMS
    { oMSCd :: String
    , oMSName :: String
    } deriving (Eq,Show)
newtype OMATP = OMATP [(OneOf2 (OMS,OMSTR) (OMS,OMI))] 		deriving (Eq,Show)
data OMA = OMAOMS_OMR (OMS,OMR)
         | OMAOMS_OME (OMS,OME)
         | OMAOMS_EATALL (OMS,EATALL)
         deriving (Eq,Show)
data OME = OME OMS OMSTR
         deriving (Eq,Show)
newtype OMSTR = OMSTR String 		deriving (Eq,Show)
newtype OMI = OMI String 		deriving (Eq,Show)
data OMR = OMR
    { oMRXref :: String
    } deriving (Eq,Show)

-- EATALL stands for arbitrary OpenMath parts passed as arg.s or
-- result. These will be read in as OMObj, see below.

-- Type declaration modified, just decoration for any XML Element.
-- Converter must eat and produce any kind of element (but not text).
-- ??? do we need/should we expect any attributes here???
-- newtype EATALL = EATALL [ANY] 		deriving (Eq,Show)
newtype EATALL = EAT Element
            deriving (Eq,Show)

instance Verbatim EATALL where
    verbatim (EAT e) = verbatim e

-- for debugging only...
instance Show Element where 
    show (Elem name [] []) = "<"++name++"/>"
    show (Elem name ((n1,_):_) []) = "<"++name++' ':n1++"=.../>"
    show (Elem name attrs content) = "<"++name++" ...>...</"++name++">"
instance Eq Element where 
    Elem n1 as1 c1 == Elem n2 as2 c2 = n1 == n2 && 
                                       as1 == as2

--eatAllElem :: Element -> EATALL
eatAllElem ((CElem e):rest) = (Just (EAT e),rest)
eatAllElem ((CMisc _):rest) = eatAllElem rest
eatAllElem ((CString _ s):rest) | all isSpace s = eatAllElem rest
eatAllElem other = (Nothing,other)

--produceAllElem :: EATALL -> Element
produceAllElem (EAT e) = CElem e

{-Instance decls-}

instance XmlContent OMOBJ where
    fromElem (CElem (Elem "OMOBJ" as c0):rest) =
        (\(a,ca)->
           (Just (OMOBJ (fromAttrs as) a), rest))
        (definite fromElem "<OMATTR>" "OMOBJ" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMOBJ as a) =
        [CElem (Elem "OMOBJ" (toAttrs as) (toElem a))]
instance XmlAttributes OMOBJ_Attrs where
    fromAttrs as =
        OMOBJ_Attrs
          { oMOBJXmlns'OM = possibleA fromAttrToStr "xmlns:OM" as
          , oMOBJXmlns'xsi = possibleA fromAttrToStr "xmlns:xsi" as
          , oMOBJXsi'schemaLocation = possibleA fromAttrToStr "xsi:schemaLocation" as
          , oMOBJVersion = possibleA fromAttrToStr "version" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "xmlns:OM" (oMOBJXmlns'OM v)
        , maybeToAttr toAttrFrStr "xmlns:xsi" (oMOBJXmlns'xsi v)
        , maybeToAttr toAttrFrStr "xsi:schemaLocation" (oMOBJXsi'schemaLocation v)
        , maybeToAttr toAttrFrStr "version" (oMOBJVersion v)
        ]
instance XmlContent OMATTR where
    fromElem (CElem (Elem "OMATTR" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (OMATTR a b), rest))
           (definite fromElem "<OMA>" "OMATTR" ca))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMATTR a b) =
        [CElem (Elem "OMATTR" [] (maybe [] toElem a ++ toElem b))]
instance XmlContent OMS where
    fromElem (CElem (Elem "OMS" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMS" (toAttrs as) [])]
instance XmlAttributes OMS where
    fromAttrs as =
        OMS
          { oMSCd = definiteA fromAttrToStr "OMS" "cd" as
          , oMSName = definiteA fromAttrToStr "OMS" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "cd" (oMSCd v)
        , toAttrFrStr "name" (oMSName v)
        ]
instance XmlContent OMATP where
    fromElem (CElem (Elem "OMATP" [] c0):rest) =
        (\(a,ca)->
           (Just (OMATP a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMATP a) =
        [CElem (Elem "OMATP" [] (concatMap toElem a))]
instance XmlContent OMA where
    fromElem (CElem (Elem "OMA" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (OMAOMS_OMR a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (OMAOMS_OME a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,_) -> (Just (OMAOMS_EATALL a), rest)
                        (_,_) ->
                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMAOMS_OMR a) = [CElem (Elem "OMA" [] (toElem a) )]
    toElem (OMAOMS_OME a) = [CElem (Elem "OMA" [] (toElem a) )]
    toElem (OMAOMS_EATALL a) = [CElem (Elem "OMA" [] (toElem a) )]
instance XmlContent OME where
    fromElem (CElem (Elem "OME" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (OME a b), rest))
           (definite fromElem "<OMSTR>" "OME" ca))
        (definite fromElem "<OMS>" "OME" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OME a b) =
        [CElem (Elem "OME" [] (toElem a ++ toElem b))]
instance XmlContent OMSTR where
    fromElem (CElem (Elem "OMSTR" [] c0):rest) =
        (\(a,ca)->
           (Just (OMSTR a), rest))
        (definite fromText "text" "OMSTR" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMSTR a) =
        [CElem (Elem "OMSTR" [] (toText a))]
instance XmlContent OMI where
    fromElem (CElem (Elem "OMI" [] c0):rest) =
        (\(a,ca)->
           (Just (OMI a), rest))
        (definite fromText "text" "OMI" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (OMI a) =
        [CElem (Elem "OMI" [] (toText a))]
instance XmlContent OMR where
    fromElem (CElem (Elem "OMR" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "OMR" (toAttrs as) [])]
instance XmlAttributes OMR where
    fromAttrs as =
        OMR
          { oMRXref = definiteA fromAttrToStr "OMR" "xref" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "xref" (oMRXref v)
        ]
instance XmlContent EATALL where
    fromElem = eatAllElem
    toElem = (:[]) . produceAllElem
{- edited: use eatAllElem and produceAllElem
    fromElem (CElem (Elem "EATALL" [] c0):rest) =
        (\(a,ca)->
           (Just (EATALL a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (EATALL a) =
        [CElem (Elem "EATALL" [] (concatMap toElem a))]
-}

{-Done-}
