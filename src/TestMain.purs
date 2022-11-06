module TestMain (buf2hex, u2char, int, uint,
                module TextParser,
                module Parsing) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Enum (fromEnum, toEnum)
import Data.Ion.Parser.Text as TextParser
import Data.Maybe (Maybe)
import Data.UInt (UInt, toInt)
import Parsing as Parsing
import Unsafe.Coerce (unsafeCoerce)

foreign import buf2hex :: ArrayBuffer -> String

u2char :: UInt -> Maybe Char
u2char = toInt >>> toEnum

int :: UInt -> Int
int = unsafeCoerce

uint :: Int -> UInt
uint = unsafeCoerce
