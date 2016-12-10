{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

#if MIN_VERSION_base(4, 7, 0) && !MIN_VERSION_base(4, 8, 0)
{-# OPTIONS_GHC -fno-warn-amp #-}
#endif

module Data.Field
    ( Field (Field)
    , field
    , (.=)
    , Option (Option)
    , option
    , (?=)
    , at
    , Field1 (Field1)
    , Option1 (Option1)
    )
where

#ifdef GenericDeriving
-- anonymous-data ------------------------------------------------------------
import qualified Symbols as S


#endif
-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, (<*>), pure)
#endif
#if MIN_VERSION_base(4, 4, 0)
import           Control.Applicative (liftA2)
#endif
import           Control.Monad (guard)
import           Control.Monad.Fix (MonadFix, mfix)
#if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip, munzip, mzipWith)
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
import           Data.Foldable (Foldable, foldr)
#endif
import           Data.Function (fix)
import           Data.Functor.Classes
                     ( Eq1
                     , Ord1
                     , Read1
                     , Show1
                     , liftEq
                     , liftCompare
                     , liftReadsPrec
                     , liftShowsPrec
                     , readsData
                     , readsUnaryWith
                     , showsUnaryWith
                     )
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
#if !MIN_VERSION_base(4, 8, 0)
import           Prelude hiding (foldr)
#endif


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
#ifdef GenericDeriving
import           GHC.Generics.Compat
                     ( D1
                     , C1
                     , Generic
                     , Generic1
                     , K1 (K1)
                     , M1 (M1)
                     , Par1 (Par1)
                     , Rec0
                     , Rec1 (Rec1)
                     , Rep
                     , Rep1
                     , S1
                     , MetaCons
                     , MetaData
                     , MetaSel
                     , NoSourceUnpackedness
                     , PrefixI
                     , SourceStrict
                     , DecidedStrict
                     , from
                     , from1
                     , to
                     , to1
                     )
#endif
import           GHC.TypeLits.Compat
                     ( KnownSymbol
#ifdef DataPolyKinds
                     , Symbol
#endif
                     , symbolVal
                     )
#ifdef GenericDeriving
import           Type.Bool (False, True)
import           Type.Maybe (Just, Nothing)
#endif
import           Type.Meta (Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
data Field (p :: KPair (KString, *)) where
    Field :: KnownSymbol s => !a -> Field (Pair s a)
#ifdef PolyTypeable
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
field :: KnownSymbol s => proxy s -> a -> Field (Pair s a)
field _ = Field
{-# INLINE field #-}


------------------------------------------------------------------------------
(.=) :: KnownSymbol s => proxy s -> a -> Field (Pair s a)
(.=) _ = Field
infix 6 .=
{-# INLINE (.=) #-}


------------------------------------------------------------------------------
at :: forall s. Proxy s
at = Proxy
{-# INLINE at #-}


------------------------------------------------------------------------------
lift :: (a -> b) -> Field (Pair s a) -> Field (Pair s b)
lift f (Field a) = Field (f a)
{-# INLINE lift #-}


------------------------------------------------------------------------------
lift2
    :: (a -> b -> c)
    -> Field (Pair s a)
    -> Field (Pair s b)
    -> Field (Pair s c)
lift2 f (Field a) (Field b) = Field (f a b)
{-# INLINE lift2 #-}


------------------------------------------------------------------------------
instance Eq a => Eq (Field (Pair s a)) where
    Field a == Field b = a == b


------------------------------------------------------------------------------
instance Ord a => Ord (Field (Pair s a)) where
    compare (Field a) (Field b) = compare a b


------------------------------------------------------------------------------
instance (KnownSymbol s, Read a) => Read (Field (Pair s a)) where
    readsPrec = readsPrecHelper readsPrec


------------------------------------------------------------------------------
instance Show a => Show (Field (Pair s a)) where
    showsPrec = showsPrecHelper showsPrec


------------------------------------------------------------------------------
readsPrecHelper :: forall a s. KnownSymbol s
    => (Int -> ReadS a)
    -> Int
    -> (ReadS (Field (Pair s a)))
readsPrecHelper rp p = readParen (p > 6) $ \s -> do
    ("at", s') <- lex s
    ("@", s'') <- lex s'
    (label, s''') <- readsPrec 11 s''
    guard $ label == symbolVal (Proxy :: Proxy s)
    (".=", s'''') <- lex s'''
    (value, s''''') <- rp 7 s''''
    return $ (Field value, s''''')


------------------------------------------------------------------------------
showsPrecHelper :: forall a s. (Int -> a -> ShowS)
    -> Int
    -> Field (Pair s a)
    -> ShowS
showsPrecHelper sp p (Field a) = showParen (p > 6) $ showString "at @"
    . showsPrec 11 (symbolVal (Proxy :: Proxy s))
    . showString " .= "
    . sp 7 a


------------------------------------------------------------------------------
instance (KnownSymbol s, Bounded a) => Bounded (Field (Pair s a)) where
    minBound = Field minBound
    maxBound = Field maxBound


------------------------------------------------------------------------------
instance (KnownSymbol s, Enum a) => Enum (Field (Pair s a)) where
    toEnum n = Field (toEnum n)
    fromEnum (Field a) = fromEnum a


------------------------------------------------------------------------------
instance (KnownSymbol s, Ix a) => Ix (Field (Pair s a)) where
    range (Field a, Field b) = fmap Field $ range (a, b)
    index (Field a, Field b) (Field i) = index (a, b) i
    inRange (Field a, Field b) (Field i) = inRange (a, b) i


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup a => Semigroup (Field (Pair s a)) where
    Field a <> Field b = Field (a <> b)


#endif
------------------------------------------------------------------------------
instance (KnownSymbol s, Monoid a) => Monoid (Field (Pair s a)) where
    mempty = Field mempty
    mappend (Field a) (Field b) = Field (mappend a b)


------------------------------------------------------------------------------
instance (KnownSymbol s, Storable a) => Storable (Field (Pair s a)) where
    sizeOf _ = sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek = fmap Field . peek . castPtr
    poke ptr (Field a) = poke (castPtr ptr) a


------------------------------------------------------------------------------
instance (KnownSymbol s, Num a) => Num (Field (Pair s a)) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    negate = lift negate
    abs = lift abs
    signum = lift signum
    fromInteger = Field . fromInteger


------------------------------------------------------------------------------
instance (KnownSymbol s, Real a) => Real (Field (Pair s a)) where
    toRational (Field a) = toRational a


------------------------------------------------------------------------------
instance (KnownSymbol s, Integral a) => Integral (Field (Pair s a)) where
    quot = lift2 quot
    rem = lift2 rem
    div = lift2 div
    mod = lift2 mod
    quotRem (Field a) (Field b) = (Field a', Field b')
      where
        (a', b') = quotRem a b
    divMod (Field a) (Field b) = (Field a', Field b')
      where
        (a', b') = divMod a b
    toInteger (Field a) = toInteger a


------------------------------------------------------------------------------
instance (KnownSymbol s, Fractional a) => Fractional (Field (Pair s a)) where
    (/) = lift2 (/)
    recip = lift recip
    fromRational = Field . fromRational


------------------------------------------------------------------------------
instance (KnownSymbol s, Floating a) => Floating (Field (Pair s a)) where
    pi = Field pi
    exp = lift exp
    log = lift log
    sqrt = lift sqrt
    sin = lift sin
    cos = lift cos
    tan = lift tan
    asin = lift asin
    acos = lift acos
    atan = lift atan
    sinh = lift sinh
    cosh = lift cosh
    tanh = lift tanh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh
    (**) = lift2 (**)
    logBase = lift2 (**)


------------------------------------------------------------------------------
instance (KnownSymbol s, RealFrac a) => RealFrac (Field (Pair s a)) where
    properFraction (Field x) = (a, Field b)
      where
        (a, b) = properFraction x
    truncate (Field a) = truncate a
    round (Field a) = round a
    ceiling (Field a) = ceiling a
    floor (Field a) = floor a


------------------------------------------------------------------------------
instance (KnownSymbol s, RealFloat a) => RealFloat (Field (Pair s a)) where
    floatRadix (Field a) = floatRadix a
    floatDigits (Field a) = floatDigits a
    floatRange (Field a) = floatRange a
    decodeFloat (Field a) = decodeFloat a
    encodeFloat m n = Field (encodeFloat m n)
    exponent (Field a) = exponent a
    significand = lift significand
    scaleFloat n = lift (scaleFloat n)
    isNaN (Field a) = isNaN a
    isInfinite (Field a) = isInfinite a
    isDenormalized (Field a) = isDenormalized a
    isNegativeZero (Field a) = isNegativeZero a
    isIEEE (Field a) = isIEEE a
    atan2 = lift2 atan2


------------------------------------------------------------------------------
instance (KnownSymbol s, Bits a) => Bits (Field (Pair s a)) where
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
instance (KnownSymbol s, FiniteBits a) => FiniteBits (Field (Pair s a)) where
    finiteBitSize (Field a) = finiteBitSize a
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Field a) = countLeadingZeros a
    countTrailingZeros (Field a) = countTrailingZeros a
#endif
#endif


------------------------------------------------------------------------------
instance (KnownSymbol s, IsString a) => IsString (Field (Pair s a)) where
    fromString = Field . fromString


#ifdef GenericDeriving
------------------------------------------------------------------------------
type FieldMetaData = MetaData S.Field S.DataField S.AnonymousData False
type FieldMetaCons = MetaCons S.Field PrefixI True
type FieldMetaSel s
    = MetaSel (Just s) NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
instance KnownSymbol s => Generic (Field (Pair s a)) where
    type Rep (Field (Pair s a)) = D1 FieldMetaData
        (C1 FieldMetaCons (S1 (FieldMetaSel s) (Rec0 a)))
    from (Field a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Field a


#endif
------------------------------------------------------------------------------
instance NFData a => NFData (Field (Pair s a)) where
    rnf (Field a) = rnf a


------------------------------------------------------------------------------
data Option (p :: KPair (KString, *)) where
    Option :: KnownSymbol s => !(Maybe a) -> Option (Pair s a)
#ifdef PolyTypeable
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
option :: KnownSymbol s => proxy s -> Maybe a -> Option (Pair s a)
option _ = Option
{-# INLINE option #-}


------------------------------------------------------------------------------
(?=) :: KnownSymbol s => proxy s -> Maybe a -> Option (Pair s a)
(?=) _ = Option
infix 6 ?=
{-# INLINE (?=) #-}


------------------------------------------------------------------------------
instance Eq a => Eq (Option (Pair s a)) where
    Option a == Option b = a == b


------------------------------------------------------------------------------
instance Ord a => Ord (Option (Pair s a)) where
    compare (Option a) (Option b) = compare a b


------------------------------------------------------------------------------
instance (KnownSymbol s, Read a) => Read (Option (Pair s a)) where
    readsPrec = readsPrecHelperOption readsPrec readList


------------------------------------------------------------------------------
instance Show a => Show (Option (Pair s a)) where
    showsPrec = showsPrecHelperOption showsPrec showList


------------------------------------------------------------------------------
readsPrecHelperOption :: forall a s. KnownSymbol s
    => (Int -> ReadS a)
    -> ReadS [a]
    -> Int
    -> (ReadS (Option (Pair s a)))
readsPrecHelperOption rp rl p = readParen (p > 6) $ \s -> do
    ("at", s') <- lex s
    ("@", s'') <- lex s'
    (label, s''') <- readsPrec 11 s''
    guard $ label == symbolVal (Proxy :: Proxy s)
    ("?=", s'''') <- lex s'''
    (value, s''''') <- liftReadsPrec rp rl 7 s''''
    return $ (Option value, s''''')


------------------------------------------------------------------------------
showsPrecHelperOption :: forall a s. (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> Option (Pair s a)
    -> ShowS
showsPrecHelperOption sp sl p (Option a)
    = showParen (p > 6) $ showString "at @"
        . showsPrec 11 (symbolVal (Proxy :: Proxy s))
        . showString " ?= "
        . liftShowsPrec sp sl 7 a


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (Option (Pair s a)) where
    a@(Option (Just _)) <> _ = a
    Option Nothing <> b = b


#endif
------------------------------------------------------------------------------
instance KnownSymbol s => Monoid (Option (Pair s a)) where
    mempty = Option Nothing
    mappend a@(Option (Just _)) _ = a
    mappend (Option Nothing) b = b


#ifdef GenericDeriving
------------------------------------------------------------------------------
type OptionMetaData = MetaData S.Option S.DataField S.AnonymousData False
type OptionMetaCons = MetaCons S.Option PrefixI True
type OptionMetaSel s
    = MetaSel (Just s) NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
instance KnownSymbol s => Generic (Option (Pair s a)) where
    type Rep (Option (Pair s a)) = D1 OptionMetaData
        (C1 OptionMetaCons (S1 (OptionMetaSel s) (Rec0 (Maybe a))))
    from (Option a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Option a


#endif
------------------------------------------------------------------------------
instance NFData a => NFData (Option (Pair s a)) where
    rnf (Option a) = rnf a


------------------------------------------------------------------------------
-- | A newtype for all the so we can make instances of all the @* -> *@
-- classes of which 'Field' is morally an instance.
newtype Field1 s a = Field1 (Field (Pair s a))
  deriving
    ( Eq
    , Ord
    , Show
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup
#endif
#ifdef PolyTypeable
    , Typeable
#endif
    )
deriving instance (KnownSymbol s, Read a) => Read (Field1 s a)
deriving instance (KnownSymbol s, Bounded a) => Bounded (Field1 s a)
deriving instance (KnownSymbol s, Enum a) => Enum (Field1 s a)
deriving instance (KnownSymbol s, Ix a) => Ix (Field1 s a)
deriving instance (KnownSymbol s, Storable a) => Storable (Field1 s a)
deriving instance (KnownSymbol s, Monoid a) => Monoid (Field1 s a)
deriving instance (KnownSymbol s, Num a) => Num (Field1 s a)
deriving instance (KnownSymbol s, Real a) => Real (Field1 s a)
deriving instance (KnownSymbol s, Integral a) => Integral (Field1 s a)
deriving instance (KnownSymbol s, Fractional a) => Fractional (Field1 s a)
deriving instance (KnownSymbol s, Floating a) => Floating (Field1 s a)
deriving instance (KnownSymbol s, RealFrac a) => RealFrac (Field1 s a)
deriving instance (KnownSymbol s, RealFloat a) => RealFloat (Field1 s a)
deriving instance (KnownSymbol s, Bits a) => Bits (Field1 s a)
#if MIN_VERSION_base(4, 7, 0)
deriving instance (KnownSymbol s, FiniteBits a) => FiniteBits (Field1 s a)
#endif
deriving instance (KnownSymbol s, IsString a) => IsString (Field1 s a)
#ifdef GenericDeriving


------------------------------------------------------------------------------
type Field1MetaData = MetaData S.Field1 S.DataField S.AnonymousData True
type Field1MetaCons = MetaCons S.Field1 PrefixI False
type Field1MetaSel
    = MetaSel Nothing NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
instance KnownSymbol s => Generic (Field1 s a) where
    type Rep (Field1 s a) = D1 Field1MetaData (C1 Field1MetaCons
        (S1 Field1MetaSel (Rep (Field (Pair s a)))))
    from (Field1 a) = M1 (M1 (M1 (from a)))
    to (M1 (M1 (M1 a))) = Field1 (to a)


------------------------------------------------------------------------------
instance KnownSymbol s => Generic1 (Field1 s) where
    type Rep1 (Field1 s) =
        D1 Field1MetaData (C1 Field1MetaCons (S1 Field1MetaSel
        (D1 FieldMetaData (C1 FieldMetaCons (S1 (FieldMetaSel s) Par1)))))
    from1 (Field1 (Field a)) = M1 (M1 (M1 (M1 (M1 (M1 (Par1 a))))))
    to1 (M1 (M1 (M1 (M1 (M1 (M1 (Par1 a))))))) = Field1 (Field a)
#endif


------------------------------------------------------------------------------
instance Eq1 (Field1 s) where
    liftEq eq (Field1 (Field a)) (Field1 (Field b)) = eq a b
    {-# INLINE liftEq #-}


------------------------------------------------------------------------------
instance Ord1 (Field1 s) where
    liftCompare cmp (Field1 (Field a)) (Field1 (Field b)) = cmp a b
    {-# INLINE liftCompare #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Read1 (Field1 s) where
    liftReadsPrec rp _ = readsData $
        readsUnaryWith (readsPrecHelper rp) "Field1" Field1
    {-# INLINE liftReadsPrec #-}


------------------------------------------------------------------------------
instance Show1 (Field1 s) where
    liftShowsPrec sp _ p (Field1 a) =
        showsUnaryWith (showsPrecHelper sp) "Field1" p a
    {-# INLINE liftShowsPrec #-}


------------------------------------------------------------------------------
instance Functor (Field1 s) where
    fmap f (Field1 (Field a)) = Field1 (Field (f a))
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Foldable (Field1 s) where
    foldr f b (Field1 (Field a)) = f a b
    {-# INLINE foldr #-}


------------------------------------------------------------------------------
instance Traversable (Field1 s) where
    traverse f (Field1 (Field a)) = fmap (Field1 . Field) $ f a
    {-# INLINE traverse #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Applicative (Field1 s) where
    pure = Field1 . Field
    {-# INLINE pure #-}
    Field1 (Field f) <*> Field1 (Field a) = Field1 (Field (f a))
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Monad (Field1 s) where
    return = pure
    {-# INLINE return #-}
    Field1 (Field a) >>= f = f a
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
instance KnownSymbol s => MonadFix (Field1 s) where
    mfix = mfix_
    {-# INLINE mfix #-}


------------------------------------------------------------------------------
mfix_ :: forall s a. KnownSymbol s => (a -> Field1 s a) -> Field1 s a
mfix_ f = Field1 (Field (fix go))
  where
    go :: a -> a
    go a = case f a of
        Field1 (Field r) -> r
{-# INLINABLE mfix_ #-}
#if MIN_VERSION_base(4, 4, 0)


------------------------------------------------------------------------------
instance KnownSymbol s => MonadZip (Field1 s) where
    munzip (Field1 (Field (a, b))) = (Field1 (Field a), Field1 (Field b))
    {-# INLINE munzip #-}
    mzipWith f (Field1 (Field a)) (Field1 (Field b)) = Field1 (Field (f a b))
    {-# INLINE mzipWith #-}
#endif


------------------------------------------------------------------------------
-- | A newtype for all the so we can make instances of all the @* -> *@
-- classes of which 'Option' is morally an instance.
newtype Option1 s a = Option1 (Option (Pair s a))
  deriving
    ( Eq
    , Ord
    , Show
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup
#endif
#ifdef PolyTypeable
    , Typeable
#endif
    )
deriving instance (KnownSymbol s, Read a) => Read (Option1 s a)
deriving instance (KnownSymbol s, Monoid a) => Monoid (Option1 s a)
#ifdef GenericDeriving


------------------------------------------------------------------------------
type Option1MetaData = MetaData S.Option1 S.DataField S.AnonymousData True
type Option1MetaCons = MetaCons S.Option1 PrefixI False
type Option1MetaSel
    = MetaSel Nothing NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
instance KnownSymbol s => Generic (Option1 s a) where
    type Rep (Option1 s a) = D1 Option1MetaData (C1 Option1MetaCons
        (S1 Option1MetaSel (Rep (Option (Pair s a)))))
    from (Option1 a) = M1 (M1 (M1 (from a)))
    to (M1 (M1 (M1 a))) = Option1 (to a)


------------------------------------------------------------------------------
instance KnownSymbol s => Generic1 (Option1 s) where
    type Rep1 (Option1 s) =
        D1 Option1MetaData (C1 Option1MetaCons (S1 Option1MetaSel
        (D1 OptionMetaData (C1 OptionMetaCons (S1 (OptionMetaSel s)
        (Rec1 Maybe))))))
    from1 (Option1 (Option a)) = M1 (M1 (M1 (M1 (M1 (M1 (Rec1 a))))))
    to1 (M1 (M1 (M1 (M1 (M1 (M1 (Rec1 a))))))) = Option1 (Option a)
#endif


------------------------------------------------------------------------------
instance Eq1 (Option1 s) where
    liftEq eq (Option1 (Option a)) (Option1 (Option b)) = liftEq eq a b
    {-# INLINE liftEq #-}


------------------------------------------------------------------------------
instance Ord1 (Option1 s) where
    liftCompare cmp (Option1 (Option a)) (Option1 (Option b)) =
        liftCompare cmp a b
    {-# INLINE liftCompare #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Read1 (Option1 s) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (readsPrecHelperOption rp rl) "Option1" Option1
    {-# INLINE liftReadsPrec #-}


------------------------------------------------------------------------------
instance Show1 (Option1 s) where
    liftShowsPrec sp sl p (Option1 a) =
        showsUnaryWith (showsPrecHelperOption sp sl) "Option1" p a
    {-# INLINE liftShowsPrec #-}


------------------------------------------------------------------------------
instance Functor (Option1 s) where
    fmap f (Option1 (Option a)) = Option1 (Option (fmap f a))
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Foldable (Option1 s) where
    foldr f b (Option1 (Option a)) = foldr f b a
    {-# INLINE foldr #-}


------------------------------------------------------------------------------
instance Traversable (Option1 s) where
    traverse f (Option1 (Option a)) = fmap (Option1 . Option) $ traverse f a
    {-# INLINE traverse #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Applicative (Option1 s) where
    pure = Option1 . Option . pure
    {-# INLINE pure #-}
    Option1 (Option f) <*> Option1 (Option a) = Option1 (Option (f <*> a))
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance KnownSymbol s => Monad (Option1 s) where
    return = pure
    {-# INLINE return #-}
    Option1 (Option a) >>= f = Option1 $ Option $ a >>= unwrap . f
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
instance KnownSymbol s => MonadFix (Option1 s) where
    mfix f = Option1 $ Option $ mfix (unwrap . f)
    {-# INLINE mfix #-}


------------------------------------------------------------------------------
unwrap :: Option1 s a -> Maybe a
unwrap (Option1 (Option a)) = a
{-# INLINE unwrap #-}
#if MIN_VERSION_base(4, 4, 0)


------------------------------------------------------------------------------
instance KnownSymbol s => MonadZip (Option1 s) where
    munzip (Option1 (Option (Just (a, b)))) = (wrap (Just a), wrap (Just b))
    munzip (Option1 (Option Nothing)) = (wrap Nothing, wrap Nothing)
    {-# INLINE munzip #-}
    mzipWith f (Option1 (Option a)) (Option1 (Option b)) = wrap (liftA2 f a b)
    {-# INLINE mzipWith #-}


------------------------------------------------------------------------------
wrap :: KnownSymbol s => Maybe a -> Option1 s a
wrap a = Option1 (Option a)
{-# INLINE wrap #-}
#endif
