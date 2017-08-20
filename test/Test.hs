{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize hiding (runGet)
-- import Data.Serialize.Get hiding (runGet)
-- import qualified Data.Serialize.Get as Get
-- import Data.Serialize.Put
import Data.Time
import Data.Word

import Test.Microspec

import Vivid.OSC
import Vivid.OSC.Old.Util
import qualified Vivid.OSC.Old as Old

main :: IO ()
main = microspec $ do
   describe ".utils" $ do
      describe "float->word->float" $ \f ->
         wordToFloat (floatToWord f) === f
      describe "word->float->word" $ \w ->
         floatToWord (wordToFloat w) === w
      describe "double->word->double" $ \d ->
         wordToDouble (doubleToWord d) === d
      describe "word->Double->word" $ \w ->
         doubleToWord (wordToDouble w) === w
      describe "align == align'" $ \i ->
         alignTo4' (i :: Int) === align i
   describe "OSC types" $ do
      describe "OSC Datum" $ do
         it "newPutOSCdatum == oldPutOSCdatum" $ do
            \d -> BSL.toStrict (Old.encodeOSCDatum d) == encodeOSCDatum d
         {-
          - Problem is we don't have type tag; not high priority because
          - we test it in 'getOSC/putOSC' etc:
         it "getOSCDatum . putOSCDatum" $ \oldD ->
            let Right newD = runGet getOSCDatum (runPut $ putOSCDatum o)
            in datumEq newD oldD
            -}

         it "timestamps: old encode . decode" $
            \(Positive t) ->
                  let ts = Timestamp t
                      encoded = BSL.toStrict $ Old.encodeOSCDatum $ OSC_T ts
                      Right (OSC_T t') = Old.decodeOSCDatumWithPadding 't' encoded
                  in ts `timestampEq` t'

         describe "examples from the OSC 1.0 spec" $ do
            it "example 1" $ do
               Old.encodeOSCDatum (OSC_S "OSC")
                  === BSL.pack (map (toEnum . fromEnum) ['O','S','C', '\NUL'])
            it "example 1, new" $ do
               encodeOSCDatum (OSC_S "OSC")
                  === BS.pack (map (toEnum . fromEnum) ['O','S','C', '\NUL'])
            it "example 2" $ do
               Old.encodeOSCDatum (OSC_S "data")
                  === BSL.pack (map (toEnum . fromEnum) ("data"++replicate 4 '\NUL'))
            it "example 2, new" $ do
               encodeOSCDatum (OSC_S "data")
                   === BS.pack (map (toEnum . fromEnum) ("data"++replicate 4 '\NUL'))


      describe "the OSC type" $ do
         it "olddecode . oldencode" $ \(OSC a bs) ->
            -- My old decoding of timestamps was fucked up!:
            let oldO = OSC a $ filter (\case { OSC_T _ -> False ; _ -> True }) bs
            in Old.decodeOSC (Old.encodeOSC oldO) === Right oldO
         it "new encode == old encode" $ \o ->
            encodeOSC o === Old.encodeOSC o
         it "decode . encode" $ \o ->
            let Right new = decodeOSC (encodeOSC o)
            in oscEq new o
         describe "examples from the OSC 1.0 spec" $ do
            it "example 1" $
               encodeOSC (OSC "/oscillator/4/frequency" [OSC_F 440.0])
                  === BS.pack [
                     0x2f, 0x6f, 0x73, 0x63
                   , 0x69, 0x6c, 0x6c, 0x61
                   , 0x74, 0x6f, 0x72, 0x2f
                   , 0x34, 0x2f, 0x66, 0x72
                   , 0x65, 0x71, 0x75, 0x65
                   , 0x6e, 0x63, 0x79, 0x00
                   , 0x2c, 0x66, 0x00, 0x00
                   , 0x43, 0xdc, 0x00, 0x00
                   ]
            it "example 2" $
               let cmd = OSC "/foo" [OSC_I 1000, OSC_I (-1), OSC_S "hello", OSC_F 1.234, OSC_F 5.678]
                   out = BS.pack [
                        0x2f, 0x66, 0x6f, 0x6f
                      , 0x00, 0x00, 0x00, 0x00
                      , 0x2c, 0x69, 0x69, 0x73
                      , 0x66, 0x66, 0x00, 0x00
                      , 0x00, 0x00, 0x03, 0xe8
                      , 0xff, 0xff, 0xff, 0xff
                      , 0x68, 0x65, 0x6c, 0x6c
                      , 0x6f, 0x00, 0x00, 0x00
                      , 0x3f, 0x9d, 0xf3, 0xb6
                      , 0x40, 0xb5, 0xb2, 0x2d
                      ]
               in encodeOSC cmd === out
      describe "OSCBundle" $ do
         it "newDecode . newEncode" $ \t xs ->
            let oldB = OSCBundle t (map Right xs)
                Right newB = decodeOSCBundle (encodeOSCBundle oldB)
            in bundleEq newB oldB
         it "oldEncode === newEncode" $ \b ->
            Old.encodeOSCBundle b === encodeOSCBundle b
   describe "bijections" $ do
      it "getOSC . putOSC" $ \origOSC ->
         let Right newOSC = decodeOSC (encodeOSC origOSC)
         in oscEq newOSC origOSC
      it "putOSCString vs old" $ \(NonNullBS s) ->
         runPut (putOSCString s) === BSL.toStrict (Old.encodeOSCDatum (OSC_S s))
      describe "getString . putString" $ \(NonNullBS s) ->
         runGet getOSCString (runPut (putOSCString s)) === Right s
      it "decodetimestamp . encodetimestamp" $ \t ->
         fromRight (runGet getOSCTimestamp (runPut (putOSCTimestamp t))) `timestampEq` t
      it "oldencode timestamp == new encode" $ \t ->
         runPut (putOSCTimestamp t) === Old.encodeTimestamp t
      it "encodeOSC isRight (with the valid input we generate)" pending
      describe "binary blobs" $ do
         it "decode . encode" $ \(BS.pack -> b) ->
            runGet getOSCBlob (runPut (putOSCBlob b)) === Right b
      describe "decodeOSCdatumswithpadding == getOSCDatum" pending
   describe "unit tests - a few examples for each function" $ do
      describe "putOSCDatum" $ do
         describe "OSC_S" $ do
            it "pads an extra 4 when it's already a multiple of 4" $
               encodeOSCDatum (OSC_S "four") === "four\NUL\NUL\NUL\NUL"
      describe "do this for every function" pending
      describe "more manual test cases" pending
-- binary up to the mazimum size
-- very large and very small floats
-- NaNs and (+/-)Infinity (floats and doubles)
-- Huge numbers of variants
-- Some example times to parse
-- Some example numbers to parse


   describe "timestamp" $ do
      it "timestamp->utc" $ pending -- \t ->
         -- utcToTimestamp (timestampToUTC t) === t
      it "utc->timestamp" $ pending -- \u ->
         -- timestampToUTC (utcToTimestamp u) === u

fromRight :: Show e => Either e x -> x
fromRight = \case
   Right x -> x
   Left e -> error $ show e

-- TODO: generic-random:
instance Arbitrary OSCDatum where
   arbitrary = oneof [
       OSC_I <$> arbitrary

       -- Note no 0: OSC strings cannot contain the null character:
     , OSC_S <$> nonNullBS
     , OSC_F <$> arbitrary
     , OSC_D <$> arbitrary
     , (OSC_B . BS.pack) <$> arbitrary
     , OSC_T <$> arbitrary
     ]

nonNullBS :: Gen ByteString
nonNullBS = BS.pack <$> listOf (choose (1, maxBound :: Word8))

newtype NonNullBS = NonNullBS ByteString
 deriving (Show)

instance Arbitrary NonNullBS where
   arbitrary = NonNullBS <$> nonNullBS

instance Arbitrary Timestamp where
    -- Note cannot be negative:
   arbitrary = (Timestamp . getNonNegative) <$> arbitrary

instance Arbitrary OSC where
   arbitrary = OSC
      <$> nonNullBS -- Again no '\NUL'
      <*> arbitrary

instance Arbitrary OSCBundle where
   arbitrary = OSCBundle
      <$> arbitrary
      <*> listOf (oneof [
           -- TODO:
           -- (Left . BS.pack) <$> arbitrary -- todo: maybe needs to be padded?
           Right <$> arbitrary -- type inference is nice!
         ])

instance Arbitrary UTCTime where
   arbitrary = undefined

-- TODO: do we need this? is there a way to encode timestamps non-lossily?:
-- I would also love to get rid of this whole thing:
timestampEq :: Timestamp -> Timestamp -> Bool
timestampEq (Timestamp time0) (Timestamp time1) =
   abs (time0 - time1) < 0.0000001

datumEq :: OSCDatum -> OSCDatum -> Bool
datumEq a b = case (a, b) of
   (OSC_T t0, OSC_T t1) -> timestampEq t0 t1
   _ -> a == b

oscEq :: OSC -> OSC -> Bool
oscEq (OSC p0 args0) (OSC p1 args1) =
   (p0 == p1) && argsMatch args0 args1
 where
   argsMatch [] [] = True
   argsMatch [] (_:_) = False
   argsMatch (_:_) [] = False
   argsMatch (d0:rest0) (d1:rest1) =
      datumEq d0 d1 && argsMatch rest0 rest1

bundleEq :: OSCBundle -> OSCBundle -> Bool
bundleEq (OSCBundle t0 msgs0) (OSCBundle t1 msgs1) =
   timestampEq t0 t1 && msgsMatch msgs0 msgs1
 where
   msgsMatch [] [] = True
   msgsMatch [] (_:_) = False
   msgsMatch (_:_) [] = False
   msgsMatch (Right msg0:rest0) (Right msg1:rest1) =
      oscEq msg0 msg1 && msgsMatch rest0 rest1
   msgsMatch (Left msg0:rest0) (Left msg1:rest1) =
      (msg0 == msg1) && msgsMatch rest0 rest1
   -- This is the case where one is 'Left' and one is 'Right'.
   -- We could hypothetically test if they're equal.
   msgsMatch (_:_rest0) (_:_rest1) = False
{-
   msgsMatch (Left l:rest0) (Right r:rest1) =
      rightMatchesLeft l r
   msgsMatch (Right r:rest0) (Left l:rest1) =
      rightMatchesLeft l r

-- This is not ideal, using the encoding functions to test them
rightMatchesLeft :: Left ByteString -> Right OSC -> Bool
-}
runGet :: Get a -> ByteString -> Either String a
runGet = runGetWithNoLeftover

