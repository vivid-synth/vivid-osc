-- | An older implementation of the spec. I provide it here, and several
--   tests, to show that the new code behaves the same. Will be removed soon.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.OSC.Old (
     encodeOSC
   , encodeOSCDatum
   , encodeOSCBundle
   , encodeTimestamp

   , encodedOSC_addLength

   , decodeOSCData
   , decodeOSC
   , decodeOSCDatumWithPadding
   ) where

import Vivid.OSC (OSCDatum(..), Timestamp(..), OSC(..), OSCBundle(..), toTypeChar)
import Vivid.OSC.Old.Util

import Data.Binary (encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Monoid
import Data.Word

encodeOSC :: OSC -> ByteString
encodeOSC (OSC url args) = BSL.toStrict $ BSL.concat $ [
    encodeOSCDatum (OSC_S url)
   ,encodeOSCDatum (OSC_S ("," <> BS.concat (map toTypeChar args)))
   ] <> map encodeOSCDatum args

encodeOSCDatum :: OSCDatum -> BSL.ByteString
encodeOSCDatum = \case
   OSC_I i -> encode i
   OSC_S s -> BSL.fromStrict $
      s <> BS.replicate (align (BS.length s + 1) + 1) 0
   OSC_F f -> encode $ floatToWord f
   OSC_D d -> encode $ doubleToWord d
   OSC_T timestamp ->
      BSL.fromStrict $ encodeTimestamp timestamp
   OSC_B b -> mconcat [
       -- 4 bytes which describe the size of the blob:
        encode (fromIntegral (BS.length b) :: Int32)
       -- the blob itself:
      , BSL.fromStrict b
       -- padding:
      , BSL.fromStrict (BS8.pack (replicate  (align (BS.length b)) '\NUL'))
      ]

encodeOSCBundle :: OSCBundle -> ByteString
encodeOSCBundle (OSCBundle time messages) = mconcat [
     "#bundle\NUL"
   , encodeTimestamp time
   , (mconcat . map (encodedOSC_addLength . either id encodeOSC)) messages
   ]

encodeTimestamp :: Timestamp -> ByteString
encodeTimestamp (Timestamp time) =
   BSL.toStrict $ encode $ (round (time * 2^(32::Int)) :: Word64)


encodedOSC_addLength :: ByteString -> ByteString
encodedOSC_addLength bs =
   BSL.toStrict (encode (toEnum (BS.length bs) :: Word32)) <> bs


numBytesWithoutPadding :: Char -> ByteString -> Either String Int
numBytesWithoutPadding char b = case char of
   'i' -> Right 4
   'f' -> Right 4
   't' -> Right 4
   'd' -> Right 8
   's' -> case BS.elemIndex 0 $ b of
      Just x -> Right $ fromIntegral x
      Nothing -> Left $ "weirdness on " <> show b
   'b' -> Right . fromIntegral $
      (decode $ BSL.fromStrict $ BS.take 4 b :: Int32)
   c ->
      Left $ "vivid: unknown OSC character " <> show c <> ": " <> show b

numBytesWithPadding :: Char -> ByteString -> Either String Int
numBytesWithPadding char b = case char of
   'i' -> Right 4
   'f' -> Right 4
   't' -> Right 4
   'd' -> Right 8
   's' ->
      case numBytesWithoutPadding 's' b of
         Right nb ->
            let n = nb + 1
            in Right $ n + align n
         Left e -> Left e
   'b' ->
      case numBytesWithoutPadding 'b' b of
         Right nb -> Right $ nb + align nb + 4
         Left e -> Left e
   c ->
      Left $ "vivid: unknown OSC character " <> show c <> ": " <> show b

decodeOSCData :: [Char] -> ByteString -> Either String [OSCDatum]
decodeOSCData typeChars blob = case (typeChars, blob) of
   ([], "") -> Right []
   ([], leftover) -> Left $ "leftover bytes: " <> show leftover
   (_:_, "") -> Left $ "leftover typechars:" <> show typeChars
   (t:ypes, _) ->
      case (datum, rest) of
         (Right a, Right b) -> Right (a:b)
         (Right _, Left b) -> Left b
         (Left a, Right _) -> Left a
         (Left a, Left b) -> Left $ a ++ ", " ++ b
    where
      datum :: Either String OSCDatum
      datum = case thisBlob of
         Right b -> decodeOSCDatumWithPadding t b
         Left e -> Left e
      thisBlob :: Either String ByteString
      thisBlob = case numBytesWithPadding t blob of
         Right nb -> Right $ BS.take nb blob
         Left e -> Left e
      rest :: Either String [OSCDatum]
      rest = case numBytesWithPadding t blob of
         Right nb -> decodeOSCData ypes (BS.drop nb blob)
         Left e -> Left e

decodeOSC :: ByteString -> Either String OSC
decodeOSC b = case sizeAndStorage b of
   Left e -> Left e
   Right (sizeOfURL, storageOfURL) ->

      let (urlWithPad, allButURL) = BS.splitAt storageOfURL b
          url = BS.take sizeOfURL urlWithPad
      in case sizeAndStorage allButURL of
          Left e -> Left e
          -- typeDesc is like ",issif"
          Right (sizeOfTypeDesc, storageOfTypeDesc) ->
             case BS8.unpack $ BS.take sizeOfTypeDesc allButURL of
                (',':typeDesc) ->
                   let rest = BS.drop (storageOfURL + storageOfTypeDesc) $ b
                   in case decodeOSCData typeDesc rest of
                       Right decoded -> Right $ OSC url decoded
                       Left e -> Left e
                other -> Left $ "not understood: " ++ show other
 where
   sizeAndStorage :: ByteString -> Either String (Int, Int)
   sizeAndStorage bs =
      case (numBytesWithoutPadding 's' bs, numBytesWithPadding 's' bs) of
         (Right size, Right storage) -> Right (size, storage)
         (Left e0, Left e1) -> Left $ e0 ++ ", " ++ e1
         (Left e, _) -> Left e
         (_, Left e) -> Left e

decodeOSCDatumWithPadding :: Char -> ByteString -> Either String OSCDatum
decodeOSCDatumWithPadding char b = case char of
   'i' ->
      Right $ OSC_I $ decode $ BSL.fromStrict b
   'f' ->
      Right $ OSC_F $ wordToFloat $ decode $ BSL.fromStrict b
   's' ->
      case numBytesWithoutPadding 's' b of
         Right nb -> Right $ OSC_S $ BS.take nb b
         Left e -> Left e
   'b' ->
      case numBytesWithoutPadding 'b' b of
         Right nb -> Right $ OSC_B $ BS.take nb $ BS.drop 4 b
         Left e -> Left e
   'd' ->
      Right $ OSC_D $ wordToDouble $ decode $ BSL.fromStrict b
   't' ->
      Right $ OSC_T $ Timestamp $ (/(2^(32::Int))) $ realToFrac $
         (decode $ BSL.fromStrict b :: Word64)
   c ->
      Left $ "unknown character " <> show c <> ": " <> show b
