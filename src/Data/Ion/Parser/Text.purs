module Data.Ion.Parser.Text where

import Parsing
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Rec.Class (class MonadRec, whileJust)
import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.ArrayBuffer.Builder as BinBuilder
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.Enum as Enum
import Data.Int.Bits as IntBits
import Data.List (List(..), (:))
import Data.List as List
import Data.String.CodeUnits as StringCU
import Data.Traversable (fold, sequence_, traverse, traverse_)
import Data.UInt (UInt)
import Data.UInt as UInt
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Parsing.Combinators (asErrorMessage, many, option, optionMaybe, optional, skipMany, try, withErrorMessage)
import Parsing.Combinators as PComb
import Parsing.Combinators.Array as PA
import Parsing.String as PT
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- This type should not escape this package; JS uints are 32 bit
type Byte = UInt

type IonTextParser m a = ParserT String m a

foreign import buf2hex :: ArrayBuffer -> String

--execQuartet :: forall m. String -> Either ParseError String
--execQuartet str = base64Quartet
--  # map (unsafePerformEffect <<< BinBuilder.execPut)
--  # map buf2hex
--  # runParser str

--- Binary: Blobs & Clobs

type BufPart = BinBuilder.PutM Effect Unit

clob :: forall m. IonTextParser m ArrayBuffer
clob = lobStart *> skipMany ws *> (shortQuoted <|> longQuoted) <* skipMany ws <* lobEnd
  where
    shortQuoted :: forall m'. IonTextParser m' ArrayBuffer
    shortQuoted = shortQuote *> shortText <* shortQuote

    shortText :: forall m'. IonTextParser m' ArrayBuffer
    shortText =
      whileJust (optionMaybe ((map charToBufPart shortChar <|> (try $ commonEscape charToBufPart))
                       <|> map BinBuilder.putUint8 hexEscape))
        # map BinBuilder.execPut
        # map unsafePerformEffect

    shortChar :: forall m'. IonTextParser m' Char
    shortChar =
      chars $ "\x0020-\x0021" -- no double quote
        <> "\x0023-\x005B" -- no backslash
        <> "\x005D-\x007F"

    longQuoted :: forall m'. IonTextParser m' ArrayBuffer
    longQuoted =
      whileJust1 (skipMany ws *> longQuote *> (whileJust (optionMaybe longText)) <* longQuote)
        # map BinBuilder.execPut
        # map unsafePerformEffect

    longText :: forall m'. IonTextParser m' BufPart
    longText = impl <|> (sq <> (impl <|> (sq <> impl)))
      where
        sq :: forall m''. IonTextParser m'' BufPart
        sq = (PT.string "\\'") $> BinBuilder.putUint8 (uint $ toCharCode '\'')
        impl :: forall m''. IonTextParser m'' BufPart
        impl = longChar <|> (try $ commonEscape charToBufPart) <|> map BinBuilder.putUint8 hexEscape

    longChar :: forall m'. IonTextParser m' BufPart
    longChar = map (BinBuilder.putUint8 <<< uint <<< toCharCode) $
      chars $ "\x0020-\x0026" -- no single quote
        <> "\x0028-\x005B" -- no backslash
        <> "\x005D-\x007F"

    longQuote :: forall m'. IonTextParser m' String
    longQuote = PT.string "'''"

    uint :: Int -> UInt
    uint = unsafeCoerce

    charToBufPart :: Char -> BufPart
    charToBufPart = BinBuilder.putUint8 <<< uint <<< toCharCode

    whileJust1 :: forall m' a. Monoid a => IonTextParser m' a -> IonTextParser m' a
    whileJust1 p = p <> whileJust (optionMaybe p)

blob :: forall m. IonTextParser m ArrayBuffer
blob = lobStart *> skipMany ws *> base64 <* skipMany ws <* lobEnd

lobStart :: forall m. IonTextParser m String
lobStart = PT.string "{{"

lobEnd :: forall m. IonTextParser m String
lobEnd = PT.string "}}"

-- TODO: Streaming
base64 :: forall m. IonTextParser m ArrayBuffer
base64 = (unsafePerformEffect <<< BinBuilder.execPutM <<< sequence_) <$>
          ((PA.many $ base64Quartet <* skipMany ws) <>
           (Array.singleton <$> (option mempty ((try base64Pad2) <|> base64Pad1))))
  where
    base64Quartet :: forall m'. IonTextParser m' (BinBuilder.Put Unit)
    base64Quartet = do
      -- 4 6bit (base64) uints -> 3 8bit (byte) uints
      u1 <- base64Char
      skipMany ws
      u2 <- base64Char
      skipMany ws
      u3 <- base64Char
      skipMany ws
      u4 <- base64Char
      pure $ do
        BinBuilder.putUint8 $ (u1 `UInt.shl` (uint 2)) + (first2Bits u2)
        BinBuilder.putUint8 $ ((last4Bits u2) `UInt.shl` (uint 4)) + (first4Bits u3)
        BinBuilder.putUint8 $ ((last2Bits u3) `UInt.shl` (uint 6)) + u4

    base64Pad1 :: forall m'. IonTextParser m' (BinBuilder.Put Unit)
    base64Pad1 = do
      u1 <- base64Char
      skipMany ws
      u2 <- base64Char
      skipMany ws
      u3 <- base64Char
      skipMany ws
      _ <- PT.char '='
      pure $ do
        BinBuilder.putUint8 $ (u1 `UInt.shl` (uint 2)) + (first2Bits u2)
        BinBuilder.putUint8 $ ((last4Bits u2) `UInt.shl` (uint 4)) + (first4Bits u3)

    base64Pad2 :: forall m'. IonTextParser m' (BinBuilder.Put Unit)
    base64Pad2 = do
      u1 <- base64Char
      skipMany ws
      u2 <- base64Char
      skipMany ws
      _ <- PT.char '='
      skipMany ws
      _ <- PT.char '='
      pure $ do
        BinBuilder.putUint8 $ (u1 `UInt.shl` (uint 2)) + (first2Bits u2)

    base64Char :: forall m'. IonTextParser m' UInt
    base64Char = parseDigit 'A' 'Z' 0
                 <|> parseDigit 'a' 'z' 26
                 <|> parseDigit '0' '9' 52
                 <|> (PT.char '+' $> (uint 62))
                 <|> (PT.char '/' $> (uint 63))
                 # expected "Base 64 character (A-Za-z0-9+/)"

    first2Bits :: UInt -> UInt
    first2Bits int6bits =
      int6bits `UInt.zshr` (uint 4)
    first4Bits :: UInt -> UInt
    first4Bits int6bits =
      int6bits `UInt.zshr` (uint 2)
    last4Bits :: UInt -> UInt
    last4Bits int6bits =
       (uint 15 {-001111 bit mask-}) `UInt.and` int6bits
    last2Bits :: UInt -> UInt
    last2Bits int6bits =
      (uint 3 {-000011 bit mask-}) `UInt.and` int6bits
    uint :: Int -> UInt
    uint = unsafeCoerce


--- Common ion bits

commonEscape :: forall m a. Monoid a => (Char -> a) -> IonTextParser m a
commonEscape conv =
  PT.char '\\' *>
    (map conv
      (chars "\"'/?\\"
       <|> (PT.char '0' $> '\x0000')
       <|> (PT.char 'a' $> '\x0007')
       <|> (PT.char 'b' $> '\x0008')
       <|> (PT.char 't' $> '\x0009')
       <|> (PT.char 'n' $> '\x000A')
       <|> (PT.char 'v' $> '\x000B')
       <|> (PT.char 'f' $> '\x000C')
       <|> (PT.char 'r' $> '\x000D')
       )
      <|> (newline $> mempty))

-- | Returned UInt guaranteed to be byte-sized
hexEscape :: forall m. IonTextParser m UInt
hexEscape = PT.char '\\' *> (PT.char 'u' *> ((\a b -> (UInt.shl (uint 4) a) `UInt.and` b) <$> hexDigit <*> hexDigit))
  where
    uint :: Int -> UInt
    uint = unsafeCoerce

decDigit :: forall m. IonTextParser m UInt
decDigit = parseDigit '0' '9' 0

binaryDigit :: forall m. IonTextParser m UInt
binaryDigit = parseDigit '0' '1' 0

hexDigit :: forall m. IonTextParser m UInt
hexDigit = digit <|> parseDigit 'a' 'f' 10 <|> parseDigit 'A' 'F' 10
           # expected "hex digit"

digit :: forall m. IonTextParser m UInt
digit = parseDigit '0' '9' 0
        # expected "digit"

shortQuote :: forall m. IonTextParser m Char
shortQuote = PT.char '"'


newline :: forall m. IonTextParser m String
newline = strs
          [ "\x000D\x000A" -- carriage return + line feed
          , "\x000D" -- carriage return
          , "\x000A" -- line feed
          ]
          # expected "newline"

ws :: forall m. IonTextParser m Char
ws = wsNotNL <|> chars ("\x000A" -- line feed
                        <> "\x000D" -- carriage return
                       )

wsNotNL :: forall m. IonTextParser m Char
wsNotNL = chars ("\x0009" -- tab
          <> "\x000B" -- vertical tab
          <> "\x000C" -- form feed
          <> "\x0020") -- space
          # expected "non-newline whitespace"

underscore :: forall m. IonTextParser m String
underscore = PT.string "_"
             # expected "_"

--- Helper functions

chars :: forall m. String -> IonTextParser m Char
chars spec = go spec'
  where
    spec' = List.fromFoldable $ StringCU.toCharArray spec
    go (a : '-' : b : rest) =
      PT.satisfy (\c -> between a c b) <|> go rest
    go (a : rest) = PT.char a <|> go rest
    go Nil = empty
  --Array.foldr (PT.char >>> (<|>)) empty

strs :: forall m. Array String -> IonTextParser m String
strs = Array.foldr (PT.string >>> (<|>)) empty

between :: Char -> Char -> Char -> Boolean
between a b c = ((toCharCode a) <= (toCharCode b)) && ((toCharCode b) <= (toCharCode c))

expected :: forall m a. String -> IonTextParser m a -> IonTextParser m a
expected tipe = asErrorMessage ("Expected " <> tipe)

parseDigit :: forall m. Char -> Char -> Int -> IonTextParser m UInt
parseDigit start end val = map (unsafeCoerce <<< convert) $ chars (StringCU.singleton start <> "-" <> StringCU.singleton end)
  where
    convert :: Char -> Int
    convert char = (Enum.fromEnum char) - (Enum.fromEnum start) + val
