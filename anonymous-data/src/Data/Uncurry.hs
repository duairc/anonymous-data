{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
#if __GLASGOW_HASKELL__ < 704 || __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Uncurry
    ( Uncurry (Uncurry)
    , umap
    , umap2
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bits
                     ( Bits
#if MIN_VERSION_base(4, 7, 0)
                     , FiniteBits
#endif
                     , (.&.)
                     , (.|.)
                     , bit
                     , bitSize
#if MIN_VERSION_base(4, 7, 0)
                     , bitSizeMaybe
#endif
                     , clearBit
                     , complement
                     , complementBit
#if MIN_VERSION_base(4, 8, 0)
                     , countLeadingZeros
                     , countTrailingZeros
#endif
#if MIN_VERSION_base(4, 7, 0)
                     , finiteBitSize
#endif
                     , isSigned
#if MIN_VERSION_base(4, 5, 0)
                     , popCount
#endif
                     , rotate
                     , rotateL
                     , rotateR
                     , setBit
                     , shift
                     , shiftL
                     , shiftR
                     , testBit
#if MIN_VERSION_base(4, 5, 0)
                     , unsafeShiftL
                     , unsafeShiftR
#endif
                     , xor
#if MIN_VERSION_base(4, 7, 0)
                     , zeroBits
#endif
                     )
import           Data.Ix (Ix, range, index, inRange)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.String (IsString, fromString)
#ifdef PolyTypeable
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if MIN_VERSION_base(4, 4, 0)
import           GHC.Generics
                     ( D1
                     , C1
                     , Generic
                     , K1 (K1)
                     , M1 (M1)
                     , S1
                     , Rec0
                     , Rep
#if __GLASGOW_HASKELL__ >= 800
                     , FixityI (PrefixI)
                     , Meta (MetaCons, MetaData, MetaSel)
                     , SourceUnpackedness (NoSourceUnpackedness)
                     , SourceStrictness (SourceStrict)
                     , DecidedStrictness (DecidedStrict)
#else
                     , Constructor
                     , Datatype
                     , NoSelector
                     , conName
                     , datatypeName
                     , moduleName
#endif
                     , from
                     , to
                     )
#endif


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
data Uncurry f (p :: KPair (KPoly1, KPoly2)) where
    -- This is strict because Uncurry is morally a newtype
    Uncurry :: !(f a b) -> Uncurry f (Pair a b)
#ifdef PolyTypeable
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Eq (f a b) => Eq (Uncurry f (Pair a b)) where
    Uncurry a == Uncurry b = a == b


------------------------------------------------------------------------------
instance Ord (f a b) => Ord (Uncurry f (Pair a b)) where
    compare (Uncurry a) (Uncurry b) = compare a b


------------------------------------------------------------------------------
instance Read (f a b) => Read (Uncurry f (Pair a b)) where
    readsPrec p s = do
        (f, s') <- readsPrec p s
        return (Uncurry f, s')


------------------------------------------------------------------------------
instance Show (f a b) => Show (Uncurry f (Pair a b)) where
    showsPrec p (Uncurry a) = showsPrec p a


------------------------------------------------------------------------------
instance Bounded (f a b) => Bounded (Uncurry f (Pair a b)) where
    minBound = Uncurry minBound
    maxBound = Uncurry maxBound


------------------------------------------------------------------------------
instance Enum (f a b) => Enum (Uncurry f (Pair a b)) where
    toEnum = Uncurry . toEnum
    fromEnum (Uncurry f) = fromEnum f


------------------------------------------------------------------------------
instance Ix (f a b) => Ix (Uncurry f (Pair a b)) where
    range (Uncurry a, Uncurry b) = map Uncurry (range (a, b))
    index (Uncurry a, Uncurry b) (Uncurry i) = index (a, b) i
    inRange (Uncurry a, Uncurry b) (Uncurry i) = inRange (a, b) i


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (f a b) => Semigroup (Uncurry f (Pair a b)) where
    Uncurry a <> Uncurry b = Uncurry (a <> b)


#endif
------------------------------------------------------------------------------
instance Monoid (f a b) => Monoid (Uncurry f (Pair a b)) where
    mempty = Uncurry mempty
    mappend (Uncurry a) (Uncurry b) = Uncurry (mappend a b)


------------------------------------------------------------------------------
instance Storable (f a b) => Storable (Uncurry f (Pair a b)) where
    sizeOf _ = sizeOf (undefined :: f a b)
    alignment _ = alignment (undefined :: f a b)
    peek = fmap Uncurry . peek . castPtr
    poke ptr (Uncurry a) = poke (castPtr ptr) a


------------------------------------------------------------------------------
instance Num (f a b) => Num (Uncurry f (Pair a b)) where
    (+) = umap2 (+)
    (-) = umap2 (-)
    (*) = umap2 (*)
    negate = umap negate
    abs = umap abs
    signum = umap signum
    fromInteger = Uncurry . fromInteger


------------------------------------------------------------------------------
instance Real (f a b) => Real (Uncurry f (Pair a b)) where
    toRational (Uncurry a) = toRational a


------------------------------------------------------------------------------
instance Integral (f a b) => Integral (Uncurry f (Pair a b)) where
    quot = umap2 quot
    rem = umap2 rem
    div = umap2 div
    mod = umap2 mod
    quotRem (Uncurry a) (Uncurry b) = (Uncurry a', Uncurry b')
      where
        (a', b') = quotRem a b
    divMod (Uncurry a) (Uncurry b) = (Uncurry a', Uncurry b')
      where
        (a', b') = divMod a b
    toInteger (Uncurry a) = toInteger a


------------------------------------------------------------------------------
instance Fractional (f a b) => Fractional (Uncurry f (Pair a b)) where
    (/) = umap2 (/)
    recip = umap recip
    fromRational = Uncurry . fromRational


------------------------------------------------------------------------------
instance Floating (f a b) => Floating (Uncurry f (Pair a b)) where
    pi = Uncurry pi
    exp = umap exp
    log = umap log
    sqrt = umap sqrt
    sin = umap sin
    cos = umap cos
    tan = umap tan
    asin = umap asin
    acos = umap acos
    atan = umap atan
    sinh = umap sinh
    cosh = umap cosh
    tanh = umap tanh
    asinh = umap asinh
    acosh = umap acosh
    atanh = umap atanh
    (**) = umap2 (**)
    logBase = umap2 (**)


------------------------------------------------------------------------------
instance RealFrac (f a b) => RealFrac (Uncurry f (Pair a b)) where
    properFraction (Uncurry x) = (a, Uncurry b)
      where
        (a, b) = properFraction x
    truncate (Uncurry a) = truncate a
    round (Uncurry a) = round a
    ceiling (Uncurry a) = ceiling a
    floor (Uncurry a) = floor a


------------------------------------------------------------------------------
instance RealFloat (f a b) => RealFloat (Uncurry f (Pair a b)) where
    floatRadix (Uncurry a) = floatRadix a
    floatDigits (Uncurry a) = floatDigits a
    floatRange (Uncurry a) = floatRange a
    decodeFloat (Uncurry a) = decodeFloat a
    encodeFloat m n = Uncurry (encodeFloat m n)
    exponent (Uncurry a) = exponent a
    significand = umap significand
    scaleFloat n = umap (scaleFloat n)
    isNaN (Uncurry a) = isNaN a
    isInfinite (Uncurry a) = isInfinite a
    isDenormalized (Uncurry a) = isDenormalized a
    isNegativeZero (Uncurry a) = isNegativeZero a
    isIEEE (Uncurry a) = isIEEE a
    atan2 = umap2 atan2


------------------------------------------------------------------------------
instance Bits (f a b) => Bits (Uncurry f (Pair a b)) where
    Uncurry a .&. Uncurry b = Uncurry (a .&. b)
    Uncurry a .|. Uncurry b = Uncurry (a .|. b)
    xor (Uncurry a) (Uncurry b) = Uncurry (xor a b)
    complement (Uncurry a) = Uncurry (complement a)
    shift (Uncurry a) i = Uncurry (shift a i)
    shiftL (Uncurry a) i = Uncurry (shiftL a i)
    shiftR (Uncurry a) i = Uncurry (shiftR a i)
    rotate (Uncurry a) i = Uncurry (rotate a i)
    rotateL (Uncurry a) i = Uncurry (rotateL a i)
    rotateR (Uncurry a) i = Uncurry (rotateR a i)
    bit i = Uncurry (bit i)
    setBit (Uncurry a) i = Uncurry (setBit a i)
    clearBit (Uncurry a) i = Uncurry (clearBit a i)
    complementBit (Uncurry a) i = Uncurry (complementBit a i)
    testBit (Uncurry a) i = testBit a i
    isSigned (Uncurry a) = isSigned a
    bitSize (Uncurry a) = bitSize a
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL (Uncurry a) i = Uncurry (unsafeShiftL a i)
    unsafeShiftR (Uncurry a) i = Uncurry (unsafeShiftR a i)
    popCount (Uncurry a) = popCount a
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe (Uncurry a) = bitSizeMaybe a
    zeroBits = Uncurry zeroBits


------------------------------------------------------------------------------
instance FiniteBits (f a b) => FiniteBits (Uncurry f (Pair a b)) where
    finiteBitSize (Uncurry a) = finiteBitSize a
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Uncurry a) = countLeadingZeros a
    countTrailingZeros (Uncurry a) = countTrailingZeros a
#endif
#endif


------------------------------------------------------------------------------
instance IsString (f a b) => IsString (Uncurry f (Pair a b)) where
    fromString = Uncurry . fromString


------------------------------------------------------------------------------
umap :: (f a b -> g c d) -> Uncurry f (Pair a b) -> Uncurry g (Pair c d)
umap f (Uncurry a) = Uncurry (f a)


------------------------------------------------------------------------------
umap2
    :: (f a b -> g c d -> h e i)
    -> Uncurry f (Pair a b)
    -> Uncurry g (Pair c d)
    -> Uncurry h (Pair e i)
umap2 f (Uncurry a) (Uncurry b) = Uncurry (f a b)


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
type UncurryMetaData
    = 'MetaData "Uncurry" "Data.Uncurry" "anonymous-data" 'False
type UncurryMetaCons = 'MetaCons "Uncurry" 'PrefixI 'False
type UncurryMetaSel
    = 'MetaSel 'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict
#else
data UncurryMetaData
data UncurryMetaCons
type UncurryMetaSel = NoSelector


------------------------------------------------------------------------------
instance Datatype UncurryMetaData where
    datatypeName _ = "Uncurry"
    moduleName _ = "Data.Uncurry"


------------------------------------------------------------------------------
instance Constructor UncurryMetaCons where
    conName _ = "Uncurry"
#endif


------------------------------------------------------------------------------
instance Generic (Uncurry f (Pair a b)) where
    type Rep (Uncurry f (Pair a b)) = D1 UncurryMetaData
        (C1 UncurryMetaCons (S1 UncurryMetaSel (Rec0 (f a b))))
    from (Uncurry f) = M1 (M1 (M1 (K1 f)))
    to (M1 (M1 (M1 (K1 f)))) = Uncurry f


#endif
------------------------------------------------------------------------------
instance NFData (f a b) => NFData (Uncurry f (Pair a b)) where
    rnf (Uncurry a) = rnf a
