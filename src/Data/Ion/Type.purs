module Data.Ion.Type
  ( IonType
  , IonKind

  , Blob
  , Bool
  , Clob
  , Decimal
  , Float
  , Int
  , List
  , Null
  , Sexp
  , String
  , Struct
  , Symbol
  , Timestamp

  , name, binaryTypeId
  , blob
  , bool
  , clob
  , decimal
  , float
  , int
  , list
  , null
  , sexp
  , string
  , struct
  , symbol
  , timestamp
  )
       where

import Prelude
import Prim hiding (String, Boolean, Int, Symbol)

import Data.Function.Uncurried (Fn2, runFn2)
import Data.UInt (UInt)
import Data.UInt as UInt
import Prim as Prim
import Type.Proxy (Proxy)

newtype IonType = IonType
  { name :: Prim.String
  , binaryTypeId :: UInt
  }

name :: IonType -> Prim.String
name (IonType t) = t.name

binaryTypeId :: IonType -> UInt
binaryTypeId (IonType t) = t.binaryTypeId

null :: IonType
null = IonType { name: "null", binaryTypeId: UInt.fromInt 0 }

bool :: IonType
bool = IonType { name: "bool", binaryTypeId: UInt.fromInt 1 }

int :: IonType
int = IonType { name: "int", binaryTypeId: UInt.fromInt 2 }

float :: IonType
float = IonType { name: "float", binaryTypeId: UInt.fromInt 4 }

decimal :: IonType
decimal = IonType { name: "decimal", binaryTypeId: UInt.fromInt 5 }

timestamp :: IonType
timestamp = IonType { name: "timestamp", binaryTypeId: UInt.fromInt 6 }

symbol :: IonType
symbol = IonType { name: "symbol", binaryTypeId: UInt.fromInt 7 }

string :: IonType
string = IonType { name: "string", binaryTypeId: UInt.fromInt 8 }

clob :: IonType
clob = IonType { name: "clob", binaryTypeId: UInt.fromInt 9 }

blob :: IonType
blob = IonType { name: "blob", binaryTypeId: UInt.fromInt 10 }

list :: IonType
list = IonType { name: "list", binaryTypeId: UInt.fromInt 11 }

sexp :: IonType
sexp = IonType { name: "sexp", binaryTypeId: UInt.fromInt 12 }

struct :: IonType
struct = IonType { name: "struct", binaryTypeId: UInt.fromInt 13 }

instance Show IonType where
  show itype = "Ion " <> (name itype)

foreign import _ionTypeEqual :: Fn2 IonType IonType Prim.Boolean
instance Eq IonType where
  eq = runFn2 _ionTypeEqual

instance Ord IonType where
  compare a b = compare (binaryTypeId a) (binaryTypeId b)
data IonKind

class IsIonType (tipe :: IonKind) where
  reflectIonType :: Proxy tipe -> IonType

foreign import data Blob :: IonKind
instance proxyIonBlobIsIonType :: IsIonType Blob where
  reflectIonType _ = blob

foreign import data Bool :: IonKind
instance proxyIonBoolIsIonType :: IsIonType Bool where
  reflectIonType _ = bool

foreign import data Clob :: IonKind
instance proxyIonClobIsIonType :: IsIonType Clob where
  reflectIonType _ = clob

foreign import data Decimal :: IonKind
instance proxyIonDecimalIsIonType :: IsIonType Decimal where
  reflectIonType _ = decimal

foreign import data Float :: IonKind
instance proxyIonFloatIsIonType :: IsIonType Float where
  reflectIonType _ = float

foreign import data Int :: IonKind
instance proxyIonIntIsIonType :: IsIonType Int where
  reflectIonType _ = int

foreign import data List :: IonKind
instance proxyIonListIsIonType :: IsIonType List where
  reflectIonType _ = list

foreign import data Null :: IonKind
instance proxyIonNullIsIonType :: IsIonType Null where
  reflectIonType _ = null

foreign import data Sexp :: IonKind
instance proxyIonSexpIsIonType :: IsIonType Sexp where
  reflectIonType _ = sexp

foreign import data String :: IonKind
instance proxyIonStringIsIonType :: IsIonType String where
  reflectIonType _ = string

foreign import data Struct :: IonKind
instance proxyIonStructIsIonType :: IsIonType Struct where
  reflectIonType _ = struct

foreign import data Symbol :: IonKind
instance proxyIonSymbolIsIonType :: IsIonType Symbol where
  reflectIonType _ = symbol

foreign import data Timestamp :: IonKind
instance proxyIonTimestampIsIonType :: IsIonType Timestamp where
  reflectIonType _ = timestamp
