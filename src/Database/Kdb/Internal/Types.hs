{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Kdb.Internal.Types
-- Copyright   :  (c) 2014, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Types used to work with Kdb+.
-----------------------------------------------------------------------------
module Database.Kdb.Internal.Types (
    -- * Data types
    Atom(..)
  , Vector(..)
  , Value(..)
    -- * Utility functions
  , size
  , qType
  , numElements
    -- * Constructors
  , by
  , byV
  , i
  , iV
  , iVV
  , cV
  , s
  , sV
  , sVV
  , lI
  , dict
  ) where

import           Control.Monad (foldM_)
import           Data.ByteString (ByteString)
import           Data.Word
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List (foldl')
import           Data.Typeable (Typeable)
import           Foreign.C.Types (CChar)
import           Foreign (sizeOf)
import qualified Data.ByteString           as B
import qualified Data.Vector.Storable      as SV
import qualified Data.Vector               as V

-- Unsafe functions
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable.Mutable as MSV

-- | Kdb atom represented as Haskell value.
data Atom
      -- | Boolean atom
    = B  {-# UNPACK #-} !Word8
      -- | Byte atom
    | X  {-# UNPACK #-} !Word8
      -- | Short atom
    | H  {-# UNPACK #-} !Int16
      -- | Int atom
    | I  {-# UNPACK #-} !Int32
      -- | Long atom
    | J  {-# UNPACK #-} !Int64
      -- | Real atom
    | E  {-# UNPACK #-} !Float
      -- | Float atom
    | F  {-# UNPACK #-} !Double
      -- | Character atom
    | C  {-# UNPACK #-} !CChar
      -- | Symbol - null terminated C string
    | S  {-# UNPACK #-} !(SV.Vector CChar)
  deriving (Eq, Show, Typeable)

-- | Kdb vector represented as Haskell value.
data Vector
      -- | Boolean vector
    = BV {-# UNPACK #-} !(SV.Vector Word8)
      -- | Byte vector
    | XV {-# UNPACK #-} !(SV.Vector Word8)
      -- | Short vector
    | HV {-# UNPACK #-} !(SV.Vector Int16)
      -- | Int vector
    | IV {-# UNPACK #-} !(SV.Vector Int32)
      -- | Long vector
    | JV {-# UNPACK #-} !(SV.Vector Int64)
      -- | Real vector
    | EV {-# UNPACK #-} !(SV.Vector Float)
      -- | Float vector
    | FV {-# UNPACK #-} !(SV.Vector Double)
      -- | Character vector: just a list of chars, no need to store length or total bytes
    | CV {-# UNPACK #-} !(SV.Vector CChar)
      -- | Symbol list: Int stores the number of null-terminated C Strings
    | SV {-# UNPACK #-}  !Int {-# UNPACK #-} !(SV.Vector CChar)
  deriving (Eq, Show, Typeable)

-- | Kdb value represented as Haskell value.
data Value
      -- | Atom value.
    = A   !Atom
      -- | Vector value.
    | V   !Vector
      -- | Q table: Int stores total bytes needed to encode table in `Vector`
    | T  {-# UNPACK #-} !Int {-# UNPACK #-} !(V.Vector Value)
      -- | Untyped vector: e.g. remote function call of the form ("insert";`t;table)
    | L  {-# UNPACK #-} !(V.Vector Value)
      -- | Dictionary: e.g. `a`b!2 3
    | D  !Vector !Vector
  deriving (Eq, Show, Typeable)

-- | Gets total size in bytes taken to store the data from `Value`.
--
-- Used to calculate total bytes needed to build the ByteString for IPC to Q server
size :: Value -> Int
size (A (B  _))   = 1 + sizeOf (undefined :: Word8)
size (V (BV x))   = 6 + (sizeOf (undefined :: Word8))  * (SV.length x)
size (A (X  _))   = 1 + sizeOf (undefined :: Word8)
size (V (XV x))   = 6 + (sizeOf (undefined :: Word8))  * (SV.length x)
size (A (H  _))   = 1 + sizeOf (undefined :: Int16)
size (V (HV x))   = 6 + (sizeOf (undefined :: Int16))  * (SV.length x)
size (A (I  _))   = 1 + sizeOf (undefined :: Int32)
size (V (IV x))   = 6 + (sizeOf (undefined :: Int32))  * (SV.length x)
size (A (J  _))   = 1 + sizeOf (undefined :: Int64)
size (V (JV x))   = 6 + (sizeOf (undefined :: Int64))  * (SV.length x)
size (A (E  _))   = 1 + sizeOf (undefined :: Float)
size (V (EV x))   = 6 + (sizeOf (undefined :: Float))  * (SV.length x)
size (A (F  _))   = 1 + sizeOf (undefined :: Double)
size (V (FV x))   = 6 + (sizeOf (undefined :: Double)) * (SV.length x)
size (A (C  _))   = 1 + sizeOf (undefined :: CChar)
size (V (CV x))   = 6 + (sizeOf (undefined :: CChar)) * (SV.length x)
size (A (S  x))   = 1 + (sizeOf (undefined :: CChar)) * (SV.length x)
size (V (SV _ x)) = size (V (CV x)) -- Really?
size (T  n _)     = n
size (L  x)       = V.foldl' (\x' y -> x' + size y) 6 x
size (D l r)      = 1 + (size . V $ l) + (size . V $ r)
{-# INLINE size #-}

-- | Gets q type of the object.
qType :: Value -> Int8
qType (A (B  _))   = -1
qType (V (BV _))   = 1
qType (A (X  _))   = -4
qType (V (XV _))   = 4
qType (A (H  _))   = -5
qType (V (HV _))   = 5
qType (A (I  _))   = -6
qType (V (IV _))   = 6
qType (A (J  _))   = -7
qType (V (JV _))   = 7
qType (A (E  _))   = -8
qType (V (EV _))   = 8
qType (A (F  _))   = -9
qType (V (FV _))   = 9
qType (A (C  _))   = -10
qType (V (CV _))   = 10
qType (A (S  _))   = -11
qType (V (SV _ _)) = 11
qType (T  _ _)     = 98
qType (L  _)       = 0
qType (D  _ _)     = 99

numElements :: Value -> Int
numElements (V (BV x))   = SV.length x
numElements (V (XV x))   = SV.length x
numElements (V (HV x))   = SV.length x
numElements (V (IV x))   = SV.length x
numElements (V (JV x))   = SV.length x
numElements (V (EV x))   = SV.length x
numElements (V (FV x))   = SV.length x
numElements (V (CV x))   = SV.length x
numElements (V (SV n _)) = n
numElements (L  x)       = V.length x
numElements _            = undefined

-- Constructors.

-- | Creates an integer atom.
i :: Int -> Value
i = A . I . fromIntegral

-- | Creates an integer vector.
iV :: [Int] -> Value
iV = V . iVV

iVV :: [Int] -> Vector
iVV = IV . SV.fromList . map fromIntegral

-- | Creates a byte atom.
by :: Word8 -> Value
by = A . X

byV :: [Word8] -> Value
byV = V . XV . SV.fromList

-- | Creates `CV` from `ByteString`.
cV :: ByteString -> Value
cV !bs = V . CV $ SV.generate (B.length bs) (fromIntegral . B.index bs)

-- | Creates a `L` from the list of values.
lI :: [Value] -> Value
lI = L . V.fromList

dict :: Vector -> Vector -> Value
dict = D

-- | Creates null-terminated `S` from `ByteString`.
--
-- The given `ByteString` should not have null-bytes.
s :: ByteString -> Value
s !bs = A . S $ unsafePerformIO $ do
    v <- MSV.new fl
    _ <- fill0 v 0 0 bs
    SV.unsafeFreeze v
  where
    fl = B.length bs + 1 -- '\0' terminated string

-- | Creates `V` from a list of `ByteString`.
sV :: [ByteString] -> Value
sV = V . sVV

-- | Creates `Vector` from a list of `ByteString`.
sVV :: [ByteString] -> Vector
sVV bss = SV bssl $ unsafePerformIO $ do
    v <- MSV.new sl
    foldM_ (fill0 v 0) 0 bss
    SV.unsafeFreeze v
  where sl = foldl' (\b a -> b + B.length a + 1) 0 bss
        bssl = length bss

-- Utility functions.

-- | Fills this `MSV.MVector` with this `ByteString` terminating with '\0' byte
-- and returns the next index to the vector.
--
-- This function assumes there is enough space in the vector and is in this shape
-- so that it is simple to pass to `foldM_`.
fill0 :: MSV.IOVector CChar -- Vector to fill
      -> Int                -- Current index into the `ByteString`
      -> Int                -- Current index into the vector
      -> ByteString         -- `ByteString` to write
      -> IO Int             -- Next index in the vector
fill0 v !bi' !vi' bs | B.length bs == bi' = MSV.unsafeWrite v vi' (0 :: CChar) >> return (vi' + 1)
fill0 v !bi' !vi' bs                      = MSV.unsafeWrite v vi' (fromIntegral $ B.index bs bi') >> fill0 v (bi' + 1) (vi' + 1) bs
