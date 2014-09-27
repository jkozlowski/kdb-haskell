-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.IPC
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Serialisation code.
--
-- Most functions in this module are fairly unsafe, however this is necessary
-- in order to achieve perfomance.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.IPC (
    asyncIPC
  ) where

import           Control.Monad (void)
import           Data.Int (Int32)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Bits (shiftR)
import           Database.Kdb.Internal.Types (Atom(..), Vector(..), Value(..))
import           Foreign   (Ptr, poke, plusPtr)
import qualified Database.Kdb.Internal.Types as KT
import qualified Data.Vector                 as V
import qualified Data.Vector.Storable        as SV

-- Unsafe stuff
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString.Internal (unsafeCreate, ByteString)

-- Helper functions that write particular values to a pointer.

-- | Write a `Word16` to a pointer in little endian format and advance pointer.
putWord8 :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord8 w p = poke p w >> return (p `plusPtr` 1)
{-# INLINE putWord8 #-}

-- | Write a Word16 in little endian format
putWord16 :: Word16 -> Ptr Word8 -> IO (Ptr Word8)
putWord16 w p = do
    poke p               (fromIntegral (w)          :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 8) :: Word8)
    return (p `plusPtr` 2)
{-# INLINE putWord16 #-}

-- | Write a Word32 in little endian format
putWord32 :: Word32 -> Ptr Word8 -> IO (Ptr Word8)
putWord32 w p = do
  poke p (fromIntegral (w) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftR w  8)  :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftR w  16) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftR w  24) :: Word8)
  return (p `plusPtr` 4)
{-# iNLINE putWord32 #-}

-- | Write a Word64 in little endian format
putWord64 :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
putWord64 w p = do
  poke p               (fromIntegral (w)           :: Word8)
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
{-# INLINE genFnPutWordNV #-}

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

-- | Function to write Q attribute in ByteString.
putAttr :: Ptr Word8 -> IO (Ptr Word8)
putAttr ptr = poke ptr (0 :: Word8) >> return (ptr `plusPtr` 1)

-- write type, attr, length for list elements.
putListHeader :: Value -> Ptr Word8 -> IO (Ptr Word8)
putListHeader x ptr = putType x ptr >>= putAttr
                    >>= \p2 -> putWord32 (unsafeCoerce ((fromIntegral $ KT.numElements x) :: Int32)) p2
                    >> return (p2 `plusPtr` 4)

-- Actual IPC code.

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
qBytes v@(A (B  x))   ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(V (BV x))   ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(A (X  x))   ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(V (XV x))   ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(A (H  x))   ptr = putType v ptr >>= putWord16 (unsafeCoerce x)
qBytes v@(V (HV x))   ptr = putListHeader v ptr >>= putWord16V (unsafeCoerce x)
qBytes v@(A (I  x))   ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(V (IV x))   ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(A (J  x))   ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(V (JV x))   ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(A (E  x))   ptr = putType v ptr >>= putWord32 (unsafeCoerce x)
qBytes v@(V (EV x))   ptr = putListHeader v ptr >>= putWord32V (unsafeCoerce x)
qBytes v@(A (F  x))   ptr = putType v ptr >>= putWord64 (unsafeCoerce x)
qBytes v@(V (FV x))   ptr = putListHeader v ptr >>= putWord64V (unsafeCoerce x)
qBytes v@(A (C  x))   ptr = putType v ptr >>= putWord8 (unsafeCoerce x)
qBytes v@(V (CV x))   ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(A (S  x))   ptr = putType v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(V (SV _ x)) ptr = putListHeader v ptr >>= putWord8V (unsafeCoerce x)
qBytes v@(L  x)       ptr = putListHeader v ptr >>= \p -> V.foldM' (flip qBytes) p x
qBytes v@(T  _ x)     ptr = putType v ptr >>= putAttr >>= \p2 -> poke p2 (99 :: Word8) >> V.foldM' (flip qBytes) (p2 `plusPtr` 1) x
qBytes v@(D l r)      ptr = putType v ptr >>= qBytes (V l) >>= qBytes (V r)

qIPCBytes :: Word8 -> Int -> Value -> Ptr Word8 -> IO ()
qIPCBytes mode size qobj ptr = do
       -- pack 1,mode,0,0 - all Word8
       poke ptr (1::Word8)
       poke (ptr `plusPtr` 1) mode
       poke (ptr `plusPtr` 2) (0::Word8)
       poke (ptr `plusPtr` 3) (0::Word8)
       -- put total IPC byte length
       p1 <- putWord32 (unsafeCoerce ((fromIntegral size) :: Int32)) (ptr `plusPtr` 4)
       -- generate IPC bytes for Q object
       void $! qBytes qobj p1

qIPC :: Word8 -> Value -> ByteString
qIPC x y = let bsize = 8 + KT.size y
           in unsafeCreate bsize (qIPCBytes x bsize y)
{-# INLINE qIPC #-}

asyncIPC :: Value -> ByteString
asyncIPC = qIPC 0

