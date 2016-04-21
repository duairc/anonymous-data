{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#include "kinds.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Field
    ( Field (Field)
    , field
    )
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, (<*>), pure)
#endif
import           Control.Applicative (liftA2)
import           Control.Monad (guard)
import           Control.Monad.Fix (MonadFix, mfix)
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip, mzipWith, munzip)
#endif
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
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Foldable (Foldable, foldMap, foldr)
#endif
import           Data.Function (fix)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Functor ((<$>))
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Functor.Classes
                     ( Eq1
                     , Ord1
                     , Read1
                     , Show1
                     , liftEq
                     , liftCompare
                     , liftReadsPrec
                     , liftShowsPrec
                     )
#endif
import           Data.Ix (Ix, range, index, inRange)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.String (IsString, fromString)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Traversable (Traversable, traverse)
#endif
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
                     , Generic1
                     , K1 (K1)
                     , M1 (M1)
                     , Par1 (Par1)
                     , Rec0
                     , Rep
                     , Rep1
                     , S1
#if MIN_VERSION_base(4, 9, 0)
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
                     , from1
                     , to
                     , to1
                     )
#endif
#if !MIN_VERSION_base(4, 8, 0)
import           Prelude hiding (foldr)
#endif


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat (KnownSymbol, symbolVal)
import           Type.Meta (Proxy (Proxy))


------------------------------------------------------------------------------
data Field s a where
    Field :: KnownSymbol s => !a -> Field s a
#ifdef PolyTypeable
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
instance Functor (Field s) where
    fmap f (Field a) = Field (f a)


------------------------------------------------------------------------------
instance KnownSymbol s => Applicative (Field s) where
    pure = Field
    Field f <*> Field a = Field (f a)


------------------------------------------------------------------------------
instance KnownSymbol s => Monad (Field s) where
    return = Field
    Field a >>= f = f a


------------------------------------------------------------------------------
instance KnownSymbol s => MonadFix (Field s) where
    mfix f = Field (fix (\a -> let Field r = f a in r))


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance KnownSymbol s => MonadZip (Field s) where
    mzipWith f (Field a) (Field b) = Field (f a b)
    munzip (Field (a, b)) = (Field a, Field b)


#endif
------------------------------------------------------------------------------
instance Foldable (Field s) where
    foldMap f (Field a) = f a
    foldr f b (Field a) = f a b


------------------------------------------------------------------------------
instance Traversable (Field s) where
    traverse f (Field a) = Field <$> f a


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Eq1 (Field s) where
    liftEq eq (Field a) (Field b) = eq a b


------------------------------------------------------------------------------
instance Ord1 (Field s) where
    liftCompare compare_ (Field a) (Field b) = compare_ a b


------------------------------------------------------------------------------
instance KnownSymbol s => Read1 (Field s) where
    liftReadsPrec readsPrec_ _ p = readParen (p > 11) $ \s -> do
        (label, s') <- lex s
        guard $ label == symbolVal (Proxy :: Proxy s)
        ("=", s'') <- lex s'
        (value, s''') <- readsPrec_ 0 s''
        return $ (field (Proxy :: Proxy s) value, s''')


------------------------------------------------------------------------------
instance Show1 (Field s) where
    liftShowsPrec showsPrec_ _ p (Field a) = showParen (p > 10) $ foldr (.) id
        [ showString (symbolVal (Proxy :: Proxy s))
        , showString " = "
        , showsPrec_ 11 a
        ]


#endif
------------------------------------------------------------------------------
instance Eq a => Eq (Field s a) where
    Field a == Field b = a == b


------------------------------------------------------------------------------
instance Ord a => Ord (Field s a) where
    compare (Field a) (Field b) = compare a b


------------------------------------------------------------------------------
instance (KnownSymbol s, Read a) => Read (Field s a) where
    readsPrec p = readParen (p > 11) $ \s -> do
        (label, s') <- lex s
        guard $ label == symbolVal (Proxy :: Proxy s)
        ("=", s'') <- lex s'
        (value, s''') <- readsPrec 0 s''
        return $ (field (Proxy :: Proxy s) value, s''')


------------------------------------------------------------------------------
instance Show a => Show (Field s a) where
    showsPrec p (Field a) = showParen (p > 10) $ foldr (.) id
        [ showString (symbolVal (Proxy :: Proxy s))
        , showString " = "
        , shows a
        ]


------------------------------------------------------------------------------
instance (KnownSymbol s, Bounded a) => Bounded (Field s a) where
    minBound = Field minBound
    maxBound = Field maxBound


------------------------------------------------------------------------------
instance (KnownSymbol s, Enum a) => Enum (Field s a) where
    toEnum n = Field (toEnum n)
    fromEnum (Field a) = fromEnum a


------------------------------------------------------------------------------
instance (KnownSymbol s, Ix a) => Ix (Field s a) where
    range (Field a, Field b) = Field <$> range (a, b)
    index (Field a, Field b) (Field i) = index (a, b) i
    inRange (Field a, Field b) (Field i) = inRange (a, b) i


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup a => Semigroup (Field s a) where
    Field a <> Field b = Field (a <> b)


#endif
------------------------------------------------------------------------------
instance (KnownSymbol s, Monoid a) => Monoid (Field s a) where
    mempty = Field mempty
    mappend (Field a) (Field b) = Field (mappend a b)


------------------------------------------------------------------------------
instance (KnownSymbol s, Storable a) => Storable (Field s a) where
    sizeOf _ = sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek = fmap Field . peek . castPtr
    poke ptr (Field a) = poke (castPtr ptr) a


------------------------------------------------------------------------------
instance (KnownSymbol s, Num a) => Num (Field s a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Field . fromInteger


------------------------------------------------------------------------------
instance (KnownSymbol s, Real a) => Real (Field s a) where
    toRational (Field a) = toRational a


------------------------------------------------------------------------------
instance (KnownSymbol s, Integral a) => Integral (Field s a) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Field a) (Field b) = (Field a', Field b')
      where
        (a', b') = quotRem a b
    divMod (Field a) (Field b) = (Field a', Field b')
      where
        (a', b') = divMod a b
    toInteger (Field a) = toInteger a


------------------------------------------------------------------------------
instance (KnownSymbol s, Fractional a) => Fractional (Field s a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = Field . fromRational


------------------------------------------------------------------------------
instance (KnownSymbol s, Floating a) => Floating (Field s a) where
    pi = Field pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
    (**) = liftA2 (**)
    logBase = liftA2 (**)


------------------------------------------------------------------------------
instance (KnownSymbol s, RealFrac a) => RealFrac (Field s a) where
    properFraction (Field x) = (a, Field b)
      where
        (a, b) = properFraction x
    truncate (Field a) = truncate a
    round (Field a) = round a
    ceiling (Field a) = ceiling a
    floor (Field a) = floor a


------------------------------------------------------------------------------
instance (KnownSymbol s, RealFloat a) => RealFloat (Field s a) where
    floatRadix (Field a) = floatRadix a
    floatDigits (Field a) = floatDigits a
    floatRange (Field a) = floatRange a
    decodeFloat (Field a) = decodeFloat a
    encodeFloat m n = Field (encodeFloat m n)
    exponent (Field a) = exponent a
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Field a) = isNaN a
    isInfinite (Field a) = isInfinite a
    isDenormalized (Field a) = isDenormalized a
    isNegativeZero (Field a) = isNegativeZero a
    isIEEE (Field a) = isIEEE a
    atan2 = liftA2 atan2


------------------------------------------------------------------------------
instance (KnownSymbol s, Bits a) => Bits (Field s a) where
    Field a .&. Field b = Field (a .&. b)
    Field a .|. Field b = Field (a .|. b)
    xor (Field a) (Field b) = Field (xor a b)
    complement (Field a) = Field (complement a)
    shift (Field a) i = Field (shift a i)
    shiftL (Field a) i = Field (shiftL a i)
    shiftR (Field a) i = Field (shiftR a i)
    rotate (Field a) i = Field (rotate a i)
    rotateL (Field a) i = Field (rotateL a i)
    rotateR (Field a) i = Field (rotateR a i)
    bit i = Field (bit i)
    setBit (Field a) i = Field (setBit a i)
    clearBit (Field a) i = Field (clearBit a i)
    complementBit (Field a) i = Field (complementBit a i)
    testBit (Field a) i = testBit a i
    isSigned (Field a) = isSigned a
    bitSize (Field a) = bitSize a
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL (Field a) i = Field (unsafeShiftL a i)
    unsafeShiftR (Field a) i = Field (unsafeShiftR a i)
    popCount (Field a) = popCount a
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe (Field a) = bitSizeMaybe a
    zeroBits = Field zeroBits


------------------------------------------------------------------------------
instance (KnownSymbol s, FiniteBits a) => FiniteBits (Field s a) where
    finiteBitSize (Field a) = finiteBitSize a
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Field a) = countLeadingZeros a
    countTrailingZeros (Field a) = countTrailingZeros a
#endif
#endif


------------------------------------------------------------------------------
instance (KnownSymbol s, IsString a) => IsString (Field s a) where
    fromString = Field . fromString


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
type FieldMetaData = 'MetaData "Field" "Data.Field" "types" 'False
type FieldMetaCons = 'MetaCons "Field" 'PrefixI 'False
type FieldMetaSel
    = 'MetaSel 'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict
#else
data FieldMetaData
data FieldMetaCons
type FieldMetaSel = NoSelector


------------------------------------------------------------------------------
instance Datatype FieldMetaData where
    datatypeName _ = "Field"
    moduleName _ = "Data.Field"


------------------------------------------------------------------------------
instance Constructor FieldMetaCons where
    conName _ = "Field"
#endif


------------------------------------------------------------------------------
instance KnownSymbol s => Generic (Field s a) where
    type Rep (Field s a) = D1 FieldMetaData
        (C1 FieldMetaCons (S1 FieldMetaSel (Rec0 a)))
    from (Field a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Field a


------------------------------------------------------------------------------
instance KnownSymbol s => Generic1 (Field s) where
    type Rep1 (Field s) = D1 FieldMetaData
        (C1 FieldMetaCons (S1 FieldMetaSel Par1))
    from1 (Field a) = M1 (M1 (M1 (Par1 a)))
    to1 (M1 (M1 (M1 (Par1 a)))) = Field a


#endif
------------------------------------------------------------------------------
instance NFData a => NFData (Field s a) where
    rnf (Field a) = rnf a


------------------------------------------------------------------------------
field :: KnownSymbol s => proxy s -> a -> Field s a
field _ = Field