{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.IPC
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Serialisation and deserialisation code.
--
-- Serialisation code works by allocating a correctly sized buffed and
-- and then filling it in using pointers. This is generally fairly unsafe,
-- however this is necessary in order to achieve perfomance.
--
-- I might benchmark later to see how this would compare to something
-- like binary.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.IPC (
    -- * Serializer
    -- $serializer
    asyncIPC
  , syncIPC

    -- * Parser
    -- $parser
  , ipcParser

    -- * Utils
    -- $utils
  , systemEndianess
  ) where

import           Control.Applicative         (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad               (replicateM, void)
import           Control.Monad.ST            (ST, runST)
import           Data.Array.ST               (MArray, STUArray, newArray,
                                              readArray)
import           Data.Array.Unsafe           (castSTUArray)
import qualified Data.Attoparsec.ByteString  as A
import           Data.Bits                   (FiniteBits (..), finiteBitSize,
                                              shiftL, shiftR, (.|.))
import qualified Data.ByteString             as B
import           Data.Int                    (Int16, Int32, Int64)
import qualified Data.Vector                 as V
import qualified Data.Vector.Storable        as SV
import           Data.Word                   (Word16, Word32, Word64, Word8)
import           Database.Kdb.Internal.Types (Atom (..), Value (..),
                                              Vector (..))
import qualified Database.Kdb.Internal.Types as KT
import           Foreign                     (Ptr, plusPtr, poke)
import           Foreign.C.Types             (CChar (..))
import qualified System.Endian               as End

-- Unsafe stuff
import           Data.ByteString.Internal    (ByteString, unsafeCreate)
import           Unsafe.Coerce               (unsafeCoerce)

-- Helper functions that write particular values to a pointer.

-- | Write a `Word16` to a pointer in little endian format and advance pointer.
putWord8 :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord8 w p = poke p w >> return (p `plusPtr` 1)
{-# INLINE putWord8 #-}

-- | Write a Word16 in little endian format
putWord16 :: Word16 -> Ptr Word8 -> IO (Ptr Word8)
putWord16 w p = do
    poke p               (fromIntegral w            :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 8) :: Word8)
    return (p `plusPtr` 2)
{-# INLINE putWord16 #-}

-- | Write a Word32 in little endian format
putWord32 :: Word32 -> Ptr Word8 -> IO (Ptr Word8)
putWord32 w p = do
  poke p (fromIntegral w                            :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftR w  8)  :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftR w  16) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftR w  24) :: Word8)
  return (p `plusPtr` 4)
{-# INLINE putWord32 #-}

-- | Write a Word64 in little endian format
putWord64 :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
putWord64 w p = do
  poke p               (fromIntegral w             :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftR w  8) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftR w 16) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftR w 24) :: Word8)
  poke (p `plusPtr` 4) (fromIntegral (shiftR w 32) :: Word8)
  poke (p `plusPtr` 5) (fromIntegral (shiftR w 40) :: Word8)
  poke (p `plusPtr` 6) (fromIntegral (shiftR w 48) :: Word8)
  poke (p `plusPtr` 7) (fromIntegral (shiftR w 56) :: Word8)
  return (p `plusPtr` 8)
{-# INLINE putWord64 #-}

-- | Function to generate putWord<N>V functions, N = 8,16,32,64
genFnPutWordNV :: (SV.Storable a) => (a -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector a ->  Ptr Word8 -> IO (Ptr Word8)
genFnPutWordNV f w p = SV.foldM' (flip f) p w
{-# INLINEABLE genFnPutWordNV #-}
{-# SPECIALIZE genFnPutWordNV :: (SV.Storable Word8) => (Word8 -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector Word8 -> Ptr Word8 -> IO (Ptr Word8) #-}
{-# SPECIALIZE genFnPutWordNV :: (SV.Storable Word16) => (Word16 -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector Word16 -> Ptr Word8 -> IO (Ptr Word8) #-}
{-# SPECIALIZE genFnPutWordNV :: (SV.Storable Word32) => (Word32 -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector Word32 -> Ptr Word8 -> IO (Ptr Word8) #-}
{-# SPECIALIZE genFnPutWordNV :: (SV.Storable Word64) => (Word64 -> Ptr Word8 -> IO (Ptr Word8)) -> SV.Vector Word64 -> Ptr Word8 -> IO (Ptr Word8) #-}

putWord8V :: SV.Vector Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord8V = genFnPutWordNV putWord8
{-# INLINE putWord8V #-}

putWord16V :: SV.Vector Word16 -> Ptr Word8 -> IO (Ptr Word8)
putWord16V = genFnPutWordNV putWord16
{-# INLINE putWord16V #-}

putWord32V :: SV.Vector Word32 -> Ptr Word8 -> IO (Ptr Word8)
putWord32V = genFnPutWordNV putWord32
{-# INLINE putWord32V #-}

putWord64V :: SV.Vector Word64 -> Ptr Word8 -> IO (Ptr Word8)
putWord64V = genFnPutWordNV putWord64
{-# INLINE putWord64V #-}

-- Kdb specific helper functions.

-- | Function to write Q type in ByteString.
putType :: Value -> Ptr Word8 -> IO (Ptr Word8)
putType x ptr = poke ptr (fromIntegral $ KT.qType x ::Word8) >> return (ptr `plusPtr` 1)
{-# INLINE putType #-}

-- | Function to write Q attribute in ByteString.
putAttr :: Ptr Word8 -> IO (Ptr Word8)
putAttr ptr = poke ptr (0 :: Word8) >> return (ptr `plusPtr` 1)
{-# INLINE putAttr #-}

-- write type, attr, length for list elements.
putListHeader :: Value -> Ptr Word8 -> IO (Ptr Word8)
putListHeader x ptr = putType x ptr >>= putAttr
                    >>= \p2 -> putWord32 (unsafeCoerce ((fromIntegral $ KT.numElements x) :: Int32)) p2
                    >> return (p2 `plusPtr` 4)
{-# INLINE putListHeader #-}

-- Actual IPC code.

bigEndian :: Word8
bigEndian = 0
{-# INLINE bigEndian #-}

littleEndian :: Word8
littleEndian = 1
{-# INLINE littleEndian #-}

-- | Gets the byte to represent the endianess.
endian :: End.Endianness -> Word8
endian End.BigEndian    = bigEndian
endian End.LittleEndian = littleEndian
{-# INLINE endian #-}

-- | Endianess of the system mapped to correct byte in Kdb IPC.
systemEndianess :: Word8
systemEndianess = endian End.getSystemEndianness
{-# INLINE systemEndianess #-}

-- | Allowed message types.
data MessageType
    = Async
    | Sync
    | Response

-- | Gets the correct byte for the message type in Kdb IPC.
msgType :: MessageType -> Word8
msgType Async    = 0
msgType Sync     = 1
msgType Response = 2
{-# INLINE msgType #-}

-- | Version of the Kdb+ IPC protocol.
data Version
    = -- | no compression, no timestamp, no timespan, no uuid
      V_25

      -- | compression, timestamp, timespan
    | V_26 | V_28

      -- | compression, timestamp, timespan, uuid
    | V_30

-- | Gets the capability byte for particular `Version`.
capability :: Version -> Word8
capability V_25 = 0
capability V_26 = 1
capability V_28 = 2
capability V_30 = 3

-- | Checks if the capability is valid.
uncapability :: Word8 -> Maybe Version
uncapability 0 = Just V_25
uncapability 1 = Just V_26
uncapability 2 = Just V_28
uncapability 3 = Just V_30
uncapability _ = Nothing

-- | Function to build Q IPC representation (except message header)
qBytes :: Value -> Ptr Word8 -> IO (Ptr Word8)
qBytes v@(A (KBool        !x)) !ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(A (KByte        !x)) !ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(A (KShort       !x)) !ptr = putType v ptr >>= putWord16 (unsafeCoerce x)
qBytes v@(A (KInt         !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KLong        !x)) !ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(A (KReal        !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KFloat       !x)) !ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(A (KChar        !x)) !ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(A (KSym         !x)) !ptr = putType v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(A (KTimestamp   !x)) !ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(A (KMonth       !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KDate        !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KDateTime    !x)) !ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(A (KTimespan    !x)) !ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(A (KMinute      !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KSecond      !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(A (KTime        !x)) !ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(V (KBoolV       !x)) !ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(V (KByteV       !x)) !ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(V (KShortV      !x)) !ptr = putListHeader v ptr >>= putWord16V (unsafeCoerce x)
qBytes v@(V (KIntV        !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KLongV       !x)) !ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(V (KRealV       !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KFloatV      !x)) !ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(V (KCharV       !x)) !ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(V (KSymV      _ !x)) !ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(V (KTimestampV  !x)) !ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(V (KMonthV      !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KDateV       !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KDateTimeV   !x)) !ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(V (KTimespanV   !x)) !ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(V (KMinuteV     !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KSecondV     !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(V (KTimeV       !x)) !ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(KList           !x)  !ptr = putListHeader v ptr >>= \p -> V.foldM' (flip qBytes) p x
qBytes v@(KTable        _ !x)  !ptr = putType v ptr >>= putAttr >>= \p2 -> poke p2 (99 :: Word8) >> V.foldM' (flip qBytes) (p2 `plusPtr` 1) x
qBytes v@(KDict        !l !r)  !ptr = putType v ptr >>= qBytes (V l) >>= qBytes (V r)

qIPCBytes :: MessageType -> Int -> Value -> Ptr Word8 -> IO ()
qIPCBytes mode size qobj ptr = do
       -- endianess,mode,0,0 - all Word8
       poke ptr systemEndianess
       poke (ptr `plusPtr` 1) (msgType mode)
       poke (ptr `plusPtr` 2) (0::Word8)
       poke (ptr `plusPtr` 3) (0::Word8)
       -- put total IPC byte length
       p1 <- putWord32 (unsafeCoerce (fromIntegral size :: Int32)) (ptr `plusPtr` 4)
       -- generate IPC bytes for Q object
       void $! qBytes qobj p1
{-# INLINE qIPCBytes #-}

qIPC :: MessageType -> Value -> ByteString
qIPC x y = let bsize = 8 + KT.size y
           in unsafeCreate bsize (qIPCBytes x bsize y)
{-# INLINE qIPC #-}

-- $serializer
--
-- Some haddocks on the serializer.

asyncIPC :: Value -> ByteString
asyncIPC = qIPC Async
{-# NOINLINE asyncIPC #-}

syncIPC :: Value -> ByteString
syncIPC = qIPC Sync
{-# NOINLINE syncIPC #-}

-- $parser
--
-- Some haddocks on the parser.

ipcParser :: A.Parser KT.Value
ipcParser = do
  -- Check endianness
  endianess <- endianessParser

  -- Skip the message type and some other stuff for now
  skipN 3

  -- Skip the message length - 4 bytes
  skipN 4

  -- Parse the actual message
  fullValueParser endianess

-- | Parser for the endianess.
endianessParser :: A.Parser End.Endianness
endianessParser = A.anyWord8 >>=
    \endianess ->
    case endianess of
     x | x == bigEndian    -> pure End.BigEndian
     x | x == littleEndian -> pure End.LittleEndian
     x                     -> fail $! "Unknown endianess: " ++ show x
{-# INLINE endianessParser #-}

-- | Parser for a symbol.
--
-- Consumes all bytes until and including the first '\0' byte and returns
-- @B.ByteString@ all bytes up to but excluding the first '\0' byte.
symParser :: A.Parser B.ByteString
symParser = A.takeTill (0 ==) <* A.anyWord8 -- Need to consume the \0 byte
{-# INLINE symParser #-}

symVParser :: Int -> A.Parser KT.Vector
symVParser !len = KT.symVV <$> replicateM len symParser
{-# INLINE symVParser #-}

-- | Parser for a vector type (@Word8@), followed by vector header,
-- followed by the vector body.
fullVectorParser :: End.Endianness     -- ^ Endianness to parse.
                 -> A.Parser KT.Vector -- ^ Parsed vector.
fullVectorParser !e = do
  t <- A.anyWord8
  len <- getVectorLength e
  vectorParser e t len
{-# INLINE fullVectorParser #-}


-- | Parser for a vector.
vectorParser :: End.Endianness      -- ^ Endianness to parse.
             -> Word8               -- ^ Type of the vector.
             -> Int                 -- ^ Number of elements in the vector.
             -> A.Parser KT.Vector  -- ^ Parsed vector.
vectorParser _  !t !len | t == KT.boolVT      = KBoolV      <$> SV.replicateM len A.anyWord8
vectorParser _  !t !len | t == KT.byteVT      = KByteV      <$> SV.replicateM len A.anyWord8
vectorParser !e !t !len | t == KT.shortVT     = KShortV     <$> SV.replicateM len (int16 e)
vectorParser !e !t !len | t == KT.intVT       = KIntV       <$> SV.replicateM len (int32 e)
vectorParser !e !t !len | t == KT.longVT      = KLongV      <$> SV.replicateM len (int64 e)
vectorParser !e !t !len | t == KT.realVT      = KRealV      <$> SV.replicateM len (float32 e)
vectorParser !e !t !len | t == KT.floatVT     = KFloatV     <$> SV.replicateM len (double64 e)
vectorParser _  !t !len | t == KT.charVT      = KCharV      <$> SV.replicateM len cchar8
vectorParser _  !t !len | t == KT.symVT       = symVParser len
vectorParser !e !t !len | t == KT.timestampVT = KTimestampV <$> SV.replicateM len (int64 e)
vectorParser !e !t !len | t == KT.monthVT     = KMonthV     <$> SV.replicateM len (int32 e)
vectorParser !e !t !len | t == KT.dateVT      = KDateV      <$> SV.replicateM len (int32 e)
vectorParser !e !t !len | t == KT.dateTimeVT  = KDateTimeV  <$> SV.replicateM len (double64 e)
vectorParser !e !t !len | t == KT.timespanVT  = KTimespanV  <$> SV.replicateM len (int64 e)
vectorParser !e !t !len | t == KT.minuteVT    = KMinuteV    <$> SV.replicateM len (int32 e)
vectorParser !e !t !len | t == KT.secondVT    = KSecondV    <$> SV.replicateM len (int32 e)
vectorParser !e !t !len | t == KT.timeVT      = KTimeV      <$> SV.replicateM len (int32 e)
vectorParser _  !t _                          = fail $ "Unknown kdb type: " ++ show t
{-# INLINEABLE vectorParser #-}

-- | Parser for a list.
listParser :: End.Endianness
           -> A.Parser KT.Value
listParser !e = do
  len <- getVectorLength e
  KList <$> V.replicateM len (fullValueParser e)
{-# INLINEABLE listParser #-}

dictParser :: End.Endianness
           -> A.Parser KT.Value
dictParser !e = KDict <$> fullVectorParser e <*> fullVectorParser e
{-# INLINEABLE dictParser #-}

tableParser :: End.Endianness
            -> A.Parser KT.Value
tableParser !e = do
  _ <- A.anyWord8       -- Skip attributes
  _ <- A.word8 KT.dictT -- Skip dict type
  _ <- A.word8 KT.symVT -- Skip symbol type
  -- Parse the symV
  symV <- KT.V <$> (getVectorLength e >>= symVParser)
  _ <- A.word8 KT.listT -- Skip the list type
  -- Parse the list
  list <- listParser e
  return $! KT.table [symV, list]
{-# INLINEABLE tableParser #-}

-- | Skips attributes and parses vector length.
getVectorLength :: End.Endianness -> A.Parser Int
getVectorLength !e = fromIntegral <$> (A.anyWord8 *> word32 e)
{-# INLINEABLE getVectorLength #-}

-- | Parser for type (@Word8@) followed by @KT.Value@.
fullValueParser :: End.Endianness -> A.Parser KT.Value
fullValueParser !e = A.anyWord8 >>= valueParser e
{-# INLINEABLE fullValueParser #-}

-- | Parser for values.
valueParser :: End.Endianness -> Word8 -> A.Parser KT.Value
valueParser _  !t | t == KT.boolT      = A . KBool      <$> A.anyWord8
valueParser _  !t | t == KT.byteT      = A . KByte      <$> A.anyWord8
valueParser !e !t | t == KT.shortT     = A . KShort     <$> int16 e
valueParser !e !t | t == KT.intT       = A . KInt       <$> int32 e
valueParser !e !t | t == KT.longT      = A . KLong      <$> int64 e
valueParser !e !t | t == KT.realT      = A . KReal      <$> float32 e
valueParser !e !t | t == KT.floatT     = A . KFloat     <$> double64 e
valueParser _  !t | t == KT.charT      = A . KChar      <$> cchar8
valueParser _  !t | t == KT.symT       = KT.s           <$> symParser
valueParser !e !t | t == KT.timestampT = A . KTimestamp <$> int64 e
valueParser !e !t | t == KT.monthT     = A . KMonth     <$> int32 e
valueParser !e !t | t == KT.dateT      = A . KDate      <$> int32 e
valueParser !e !t | t == KT.dateTimeT  = A . KDateTime  <$> double64 e
valueParser !e !t | t == KT.timespanT  = A . KTimespan  <$> int64 e
valueParser !e !t | t == KT.minuteT    = A . KMinute    <$> int32 e
valueParser !e !t | t == KT.secondT    = A . KSecond    <$> int32 e
valueParser !e !t | t == KT.timeT      = A . KTime      <$> int32 e
valueParser !e !t | t == KT.listT      = listParser  e
valueParser !e !t | t == KT.dictT      = dictParser  e
valueParser !e !t | t == KT.tableT     = tableParser e
valueParser !e !t                      = V          <$> (getVectorLength e >>= vectorParser e t)
{-# INLINE valueParser #-}

-- | Skips n bytes (if n <=0 the parser will not skip anything).
skipN :: Int -> A.Parser ()
skipN !n | n > 0 = A.anyWord8 *> skipN (n - 1)
skipN _          = pure ()
{-# INLINEABLE skipN #-}

-- Adapted from attoparsec-binary.

cchar8 :: A.Parser CChar
cchar8 = fromIntegral <$> A.anyWord8
{-# INLINE cchar8 #-}

int16 :: End.Endianness -> A.Parser Int16
int16 End.LittleEndian = anyWordN $ pack . B.reverse
int16 End.BigEndian    = anyWordN pack
{-# INLINE int16 #-}

int32 :: End.Endianness -> A.Parser Int32
int32 End.LittleEndian = anyWordN $ pack . B.reverse
int32 End.BigEndian    = anyWordN pack
{-# INLINE int32 #-}

int64 :: End.Endianness -> A.Parser Int64
int64 End.LittleEndian = anyWordN $ pack . B.reverse
int64 End.BigEndian    = anyWordN pack
{-# INLINE int64 #-}

word32 :: End.Endianness -> A.Parser Word32
word32 End.LittleEndian = anyWord32le
word32 End.BigEndian    = anyWord32be
{-# INLINE word32 #-}

float32 :: End.Endianness -> A.Parser Float
float32 !e = wordToFloat <$> case e of
                               End.LittleEndian -> anyWord32le
                               End.BigEndian    -> anyWord32be
{-# INLINE float32 #-}

double64 :: End.Endianness -> A.Parser Double
double64 !e = wordToDouble <$> case e of
                                End.LittleEndian -> anyWord64le
                                End.BigEndian    -> anyWord64be
{-# INLINE double64 #-}

byteSize :: (FiniteBits a) => a -> Int
byteSize = (`div` 8) . finiteBitSize
{-# INLINEABLE byteSize #-}
{-# SPECIALIZE byteSize :: (FiniteBits Int16) => Int16 -> Int #-}
{-# SPECIALIZE byteSize :: (FiniteBits Int32) => Int32 -> Int #-}
{-# SPECIALIZE byteSize :: (FiniteBits Int64) => Int64 -> Int #-}
{-# SPECIALIZE byteSize :: (FiniteBits Word32) => Word32 -> Int #-}
{-# SPECIALIZE byteSize :: (FiniteBits Word64) => Word64 -> Int #-}

pack :: (FiniteBits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0
{-# INLINEABLE pack #-}
{-# SPECIALIZE pack :: (FiniteBits Int16, Num Int16) => B.ByteString -> Int16 #-}
{-# SPECIALIZE pack :: (FiniteBits Int32, Num Int32) => B.ByteString -> Int32 #-}
{-# SPECIALIZE pack :: (FiniteBits Int64, Num Int64) => B.ByteString -> Int64 #-}
{-# SPECIALIZE pack :: (FiniteBits Word32, Num Word32) => B.ByteString -> Word32 #-}
{-# SPECIALIZE pack :: (FiniteBits Word64, Num Word64) => B.ByteString -> Word64 #-}

anyWordN :: (FiniteBits a) => (B.ByteString -> a) -> A.Parser a
anyWordN = anyWordN' undefined
  where anyWordN' :: (FiniteBits a) => a -> (B.ByteString -> a) -> A.Parser a
        anyWordN' d = flip fmap $ A.take $ byteSize d
{-# INLINEABLE anyWordN #-}
{-# SPECIALIZE anyWordN :: (FiniteBits Int16) => (B.ByteString -> Int16) -> A.Parser Int16 #-}
{-# SPECIALIZE anyWordN :: (FiniteBits Int32) => (B.ByteString -> Int32) -> A.Parser Int32 #-}
{-# SPECIALIZE anyWordN :: (FiniteBits Int64) => (B.ByteString -> Int64) -> A.Parser Int64 #-}
{-# SPECIALIZE anyWordN :: (FiniteBits Word32) => (B.ByteString -> Word32) -> A.Parser Word32 #-}
{-# SPECIALIZE anyWordN :: (FiniteBits Word64) => (B.ByteString -> Word64) -> A.Parser Word64 #-}

-- | Match any 32-bit big-endian word.
anyWord32be :: A.Parser Word32
anyWord32be = anyWordN pack
{-# INLINE anyWord32be #-}

-- | Match any 32-bit little-endian word.
anyWord32le :: A.Parser Word32
anyWord32le = anyWordN $ pack . B.reverse
{-# INLINE anyWord32le #-}

-- | Match any 64-bit big-endian word.
anyWord64be :: A.Parser Word64
anyWord64be = anyWordN pack
{-# INLINE anyWord64be #-}

-- | Match any 64-bit little-endian word.
anyWord64le :: A.Parser Word64
anyWord64le = anyWordN $ pack . B.reverse
{-# INLINE anyWord64le #-}

-- Copied from http://stackoverflow.com/a/7002812/263061.
wordToFloat :: Word32 -> Float
wordToFloat !x = runST (cast x)
{-# INLINE wordToFloat #-}

wordToDouble :: Word64 -> Double
wordToDouble !x = runST (cast x)
{-# INLINE wordToDouble #-}

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINEABLE cast #-}
{-# SPECIALIZE cast :: (MArray (STUArray Word32) Word32 (ST Word32),
                        MArray (STUArray Word32) Float (ST Word32)) => Word32 -> ST Word32 Float #-}
{-# SPECIALIZE cast :: (MArray (STUArray Word64) Word64 (ST Word64),
                        MArray (STUArray Word64) Double (ST Word64)) => Word64 -> ST Word64 Double #-}
