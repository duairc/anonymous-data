{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"
#include "overlap.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef LanguagePatternSynonyms
{-# LANGUAGE PatternSynonyms #-}
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

module Data.Labeled
    ( Labeled (Labeled)
    , labeled
    , (.=.)
    , Field
#ifdef LanguagePatternSynonyms
    , pattern Field
#endif
    , field
    , (.=)
    , Option
#ifdef LanguagePatternSynonyms
    , pattern Option
#endif
    , option
    , (?=)
    , at
    , hmap
    , hfoldr
    , htraverse
    , Labeled1 (Labeled1)
    , Field1
#ifdef LanguagePatternSynonyms
    , pattern Field1
#endif
    , Option1
#ifdef LanguagePatternSynonyms
    , pattern Option1
#endif
    )
where

#ifdef GenericDeriving
-- anonymous-data ------------------------------------------------------------
import qualified Symbols as S


#endif
-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, (<$>), (<*>), pure)
#endif
import           Control.Applicative (Alternative, (<|>), empty)
#if MIN_VERSION_base(4, 4, 0)
import           Control.Arrow ((***))
#endif
import           Control.Monad (MonadPlus, guard, mplus, msum, mzero)
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
                     , readsPrec1
                     , readsUnaryWith
                     , showsUnaryWith
                     , showsPrec1
                     )
import           Data.Functor.Identity (Identity (Identity))
import           Data.Ix (Ix, range, index, inRange)
import           Data.Monoid
                     ( First (First)
#if !MIN_VERSION_base(4, 8, 0)
                     , Monoid
                     , mappend
                     , mempty
#endif
                     )
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
#ifdef DataPolyKinds
import           GHC.TypeLits.Compat (Symbol)
#endif
#ifdef GenericDeriving
import           Type.Bool (False, True)
import           Type.Maybe (Just, Nothing)
#endif
import           Type.Meta (Known, Val, val, Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
data Labeled (f :: KPoly2 -> *) (p :: KPair (KPoly1, KPoly2)) where
    Labeled :: Known s => !(f a) -> Labeled f (Pair s a)
#ifdef PolyTypeable
  deriving (Typeable)
#endif


------------------------------------------------------------------------------
labeled :: Known s => proxy s -> f a -> Labeled f (Pair s a)
labeled _ a = Labeled a
{-# INLINE labeled #-}


------------------------------------------------------------------------------
(.=.) :: Known s => proxy s -> f a -> Labeled f (Pair s a)
(.=.) _ a = Labeled a
infix 6 .=.
{-# INLINE (.=.) #-}


------------------------------------------------------------------------------
type Field = Labeled Identity


#ifdef LanguagePatternSynonyms
------------------------------------------------------------------------------
pattern Field :: Known s => a -> Field (Pair s a)
pattern Field a = Labeled (Identity a)


#endif
------------------------------------------------------------------------------
field :: Known s => proxy s -> a -> Field (Pair s a)
field _ a = Labeled (Identity a)
{-# INLINE field #-}


------------------------------------------------------------------------------
(.=) :: Known s => proxy s -> a -> Field (Pair s a)
(.=) _ a = Labeled (Identity a)
infix 6 .=
{-# INLINE (.=) #-}


------------------------------------------------------------------------------
type Option = Labeled First


#ifdef LanguagePatternSynonyms
------------------------------------------------------------------------------
pattern Option :: Known s => Maybe a -> Option (Pair s a)
pattern Option a = Labeled (First a)


#endif
------------------------------------------------------------------------------
option :: Known s => proxy s -> Maybe a -> Option (Pair s a)
option _ a = Labeled (First a)
{-# INLINE option #-}


------------------------------------------------------------------------------
(?=) :: Known s => proxy s -> Maybe a -> Option (Pair s a)
(?=) _ a = Labeled (First a)
infix 6 ?=
{-# INLINE (?=) #-}


------------------------------------------------------------------------------
at :: forall s. Proxy s
at = Proxy
{-# INLINE at #-}


------------------------------------------------------------------------------
instance Eq (f a) => Eq (Labeled f (Pair s a)) where
    Labeled a == Labeled b = a == b


------------------------------------------------------------------------------
instance Ord (f a) => Ord (Labeled f (Pair s a)) where
    compare (Labeled a) (Labeled b) = compare a b


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Known s, Eq (Val s), Read (Val s), Read (f a)) =>
    Read (Labeled f (Pair s a))
  where
    readsPrec = readsLabeled readsPrec


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s), Read a) =>
    Read (Field (Pair s a))
  where
    readsPrec p s = readsField readsPrec p s <|> readsLabeled readsPrec p s


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s), Read a) =>
    Read (Option (Pair s a))
  where
    readsPrec p s =
        readsOption readsPrec readList p s <|> readsLabeled readsPrec p s


------------------------------------------------------------------------------
instance __INCOHERENT__ (Show (Val s), Show (f a)) =>
    Show (Labeled f (Pair s a))
  where
    showsPrec = showsLabeled showsPrec


------------------------------------------------------------------------------
instance (Show (Val s), Show a) => Show (Field (Pair s a)) where
    showsPrec = showsField showsPrec


------------------------------------------------------------------------------
instance (Show (Val s), Show a) => Show (Option (Pair s a)) where
    showsPrec = showsOption showsPrec showList


------------------------------------------------------------------------------
readsLabeled :: forall a s f. (Known s, Eq (Val s), Read (Val s))
    => (Int -> ReadS (f a))
    -> Int
    -> (ReadS (Labeled f (Pair s a)))
readsLabeled rp p = readParen (p > 6) $ \s -> do
    ("at", s') <- lex s
    ("@", s'') <- lex s'
    (t, s''') <- readsPrec 11 s''
    guard $ t == val (Proxy :: Proxy s)
    (".=.", s'''') <- lex s'''
    (value, s''''') <- rp 7 s''''
    return $ (Labeled value, s''''')


------------------------------------------------------------------------------
showsLabeled :: forall a s f. Show (Val s)
    => (Int -> f a -> ShowS)
    -> Int
    -> Labeled f (Pair s a)
    -> ShowS
showsLabeled sp p (Labeled a) = showParen (p > 6) $ showString "at @"
    . showsPrec 11 (val (Proxy :: Proxy s))
    . showString " .=. "
    . sp 7 a


------------------------------------------------------------------------------
readsField :: forall a s. (Known s, Eq (Val s), Read (Val s))
    => (Int -> ReadS a)
    -> Int
    -> (ReadS (Field (Pair s a)))
readsField rp p = readParen (p > 6) $ \s -> do
    ("at", s') <- lex s
    ("@", s'') <- lex s'
    (t, s''') <- readsPrec 11 s''
    guard $ t == val (Proxy :: Proxy s)
    (".=", s'''') <- lex s'''
    (value, s''''') <- rp 7 s''''
    return $ (Labeled (Identity value), s''''')


------------------------------------------------------------------------------
showsField :: forall a s. Show (Val s)
    => (Int -> a -> ShowS)
    -> Int
    -> Field (Pair s a)
    -> ShowS
showsField sp p (Labeled (Identity a)) = showParen (p > 6) $ showString "at @"
    . showsPrec 11 (val (Proxy :: Proxy s))
    . showString " .= "
    . sp 7 a


------------------------------------------------------------------------------
readsOption :: forall a s. (Known s, Eq (Val s), Read (Val s))
    => (Int -> ReadS a)
    -> ReadS [a]
    -> Int
    -> (ReadS (Option (Pair s a)))
readsOption rp rl p = readParen (p > 6) $ \s -> do
    ("at", s') <- lex s
    ("@", s'') <- lex s'
    (t, s''') <- readsPrec 11 s''
    guard $ t == val (Proxy :: Proxy s)
    ("?=", s'''') <- lex s'''
    (value, s''''') <- liftReadsPrec rp rl 7 s''''
    return $ (Labeled (First value), s''''')


------------------------------------------------------------------------------
showsOption :: forall a s. Show (Val s)
    => (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> Option (Pair s a)
    -> ShowS
showsOption sp sl p (Labeled (First a)) = showParen (p > 6)
    $ showString "at @"
    . showsPrec 11 (val (Proxy :: Proxy s))
    . showString " ?= "
    . liftShowsPrec sp sl 7 a


------------------------------------------------------------------------------
instance (Known s, Bounded (f a)) => Bounded (Labeled f (Pair s a)) where
    minBound = Labeled minBound
    maxBound = Labeled maxBound


------------------------------------------------------------------------------
instance (Known s, Enum (f a)) => Enum (Labeled f (Pair s a)) where
    toEnum n = Labeled (toEnum n)
    fromEnum (Labeled a) = fromEnum a


------------------------------------------------------------------------------
instance (Known s, Ix (f a)) => Ix (Labeled f (Pair s a)) where
    range (Labeled a, Labeled b) = fmap Labeled $ range (a, b)
    index (Labeled a, Labeled b) (Labeled i) = index (a, b) i
    inRange (Labeled a, Labeled b) (Labeled i) = inRange (a, b) i


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (f a) => Semigroup (Labeled f (Pair s a)) where
    Labeled a <> Labeled b = Labeled (a <> b)


#endif
------------------------------------------------------------------------------
instance (Known s, Monoid (f a)) => Monoid (Labeled f (Pair s a)) where
    mempty = Labeled mempty
    mappend (Labeled a) (Labeled b) = Labeled (mappend a b)


------------------------------------------------------------------------------
instance (Known s, Storable (f a)) => Storable (Labeled f (Pair s a)) where
    sizeOf _ = sizeOf (undefined :: f a)
    alignment _ = alignment (undefined :: f a)
    peek = fmap Labeled . peek . castPtr
    poke ptr (Labeled a) = poke (castPtr ptr) a


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Num (f a)
    )
  =>
    Num (Labeled f (Pair s a))
  where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    negate = lift negate
    abs = lift abs
    signum = lift signum
    fromInteger = Labeled . fromInteger


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Real (f a)
    )
  =>
    Real (Labeled f (Pair s a))
  where
    toRational (Labeled a) = toRational a


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Integral (f a)
    )
  =>
    Integral (Labeled f (Pair s a))
  where
    quot = lift2 quot
    rem = lift2 rem
    div = lift2 div
    mod = lift2 mod
    quotRem (Labeled a) (Labeled b) = (Labeled a', Labeled b')
      where
        (a', b') = quotRem a b
    divMod (Labeled a) (Labeled b) = (Labeled a', Labeled b')
      where
        (a', b') = divMod a b
    toInteger (Labeled a) = toInteger a


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Fractional (f a)
    )
  =>
    Fractional (Labeled f (Pair s a))
  where
    (/) = lift2 (/)
    recip = lift recip
    fromRational = Labeled . fromRational


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Floating (f a)
    )
  =>
    Floating (Labeled f (Pair s a))
  where
    pi = Labeled pi
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
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , RealFrac (f a)
    )
  =>
    RealFrac (Labeled f (Pair s a))
  where
    properFraction (Labeled x) = (a, Labeled b)
      where
        (a, b) = properFraction x
    truncate (Labeled a) = truncate a
    round (Labeled a) = round a
    ceiling (Labeled a) = ceiling a
    floor (Labeled a) = floor a


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , RealFloat (f a)
    )
  =>
    RealFloat (Labeled f (Pair s a))
  where
    floatRadix (Labeled a) = floatRadix a
    floatDigits (Labeled a) = floatDigits a
    floatRange (Labeled a) = floatRange a
    decodeFloat (Labeled a) = decodeFloat a
    encodeFloat m n = Labeled (encodeFloat m n)
    exponent (Labeled a) = exponent a
    significand = lift significand
    scaleFloat n = lift (scaleFloat n)
    isNaN (Labeled a) = isNaN a
    isInfinite (Labeled a) = isInfinite a
    isDenormalized (Labeled a) = isDenormalized a
    isNegativeZero (Labeled a) = isNegativeZero a
    isIEEE (Labeled a) = isIEEE a
    atan2 = lift2 atan2


------------------------------------------------------------------------------
instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Bits (f a)
    )
  =>
    Bits (Labeled f (Pair s a))
  where
    (.&.) = lift2 (.&.)
    (.|.) = lift2 (.|.)
    xor = lift2 xor
    complement = lift complement
    shift a i = lift (flip shift i) a
    shiftL a i = lift (flip shiftL i) a
    shiftR a i = lift (flip shiftR i) a
    rotate a i = lift (flip shift i) a
    rotateL a i = lift (flip rotateL i) a
    rotateR a i = lift (flip rotateR i) a
    bit i = Labeled (bit i)
    setBit a i = lift (flip setBit i) a
    clearBit a i = lift (flip clearBit i) a
    complementBit a i = lift (flip complementBit i) a
    testBit (Labeled a) i = testBit a i
    isSigned (Labeled a) = isSigned a
    bitSize (Labeled a) = bitSize a
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL (Labeled a) i = Labeled (unsafeShiftL a i)
    unsafeShiftR (Labeled a) i = Labeled (unsafeShiftR a i)
    popCount (Labeled a) = popCount a
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe (Labeled a) = bitSizeMaybe a
    zeroBits = Labeled zeroBits


------------------------------------------------------------------------------
instance (Known s, FiniteBits (f a)) =>
    FiniteBits (Labeled f (Pair s a))
  where
    finiteBitSize (Labeled a) = finiteBitSize a
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Labeled a) = countLeadingZeros a
    countTrailingZeros (Labeled a) = countTrailingZeros a
#endif
#endif


------------------------------------------------------------------------------
instance (Known s, IsString (f a)) => IsString (Labeled f (Pair s a)) where
    fromString = Labeled . fromString


#ifdef GenericDeriving
------------------------------------------------------------------------------
type LabeledMetaData = MetaData S.Labeled S.DataLabeled S.AnonymousData False
type LabeledMetaCons = MetaCons S.Labeled PrefixI True
type LabeledMetaSel s
    = MetaSel (MaybeSymbol s) NoSourceUnpackedness SourceStrict DecidedStrict
#ifdef ClosedTypeFamilies
#define Ks KPoly1
type family MaybeSymbol (s :: KPoly1) :: KMaybe (KString) where
    MaybeSymbol s = Just s
    MaybeSymbol _s = Nothing
#else
#define Ks KString
type MaybeSymbol s = Just s
#endif


------------------------------------------------------------------------------
instance Known s => Generic (Labeled f (Pair (s :: Ks) a)) where
    type Rep (Labeled f (Pair s a)) = D1 LabeledMetaData
        (C1 LabeledMetaCons (S1 (LabeledMetaSel s) (Rec0 (f a))))
    from (Labeled a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Labeled a


#endif
------------------------------------------------------------------------------
instance NFData (f a) => NFData (Labeled f (Pair s a)) where
    rnf (Labeled a) = rnf a


------------------------------------------------------------------------------
lift :: (f a -> f b) -> Labeled f (Pair s a) -> Labeled f (Pair s b)
lift f (Labeled a) = Labeled (f a)
{-# INLINE lift #-}


------------------------------------------------------------------------------
lift2
    :: (f a -> f b -> f c)
    -> Labeled f (Pair s a)
    -> Labeled f (Pair s b)
    -> Labeled f (Pair s c)
lift2 f (Labeled a) (Labeled b) = Labeled (f a b)
{-# INLINE lift2 #-}


------------------------------------------------------------------------------
hmap :: (forall x. f x -> g x) -> Labeled f a -> Labeled g a
hmap f (Labeled a) = Labeled (f a)
{-# INLINE hmap #-}


------------------------------------------------------------------------------
hfoldr :: (forall x. f x -> b -> b) -> b -> Labeled f a -> b
hfoldr f b (Labeled a) = f a b
{-# INLINE hfoldr #-}


------------------------------------------------------------------------------
htraverse :: Applicative h
    => (forall x. f x -> h (g x))
    -> Labeled f a
    -> h (Labeled g a)
htraverse f (Labeled a) = Labeled <$> f a
{-# INLINE htraverse #-}


------------------------------------------------------------------------------
-- | A newtype for all the so we can make instances of all the @* -> *@
-- classes of which 'Labeled' is morally an instance.
newtype Labeled1 f s a = Labeled1 (Labeled f (Pair s a))
  deriving
    ( Eq
    , Ord
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup
#endif
#ifdef PolyTypeable
    , Typeable
#endif
    )


------------------------------------------------------------------------------
type Field1 = Labeled1 Identity


#ifdef LanguagePatternSynonyms
------------------------------------------------------------------------------
pattern Field1 :: Field (Pair s a) -> Field1 s a
pattern Field1 a = Labeled1 a


#endif
------------------------------------------------------------------------------
type Option1 = Labeled1 First


#ifdef LanguagePatternSynonyms
------------------------------------------------------------------------------
pattern Option1 :: Option (Pair s a) -> Option1 s a
pattern Option1 a = Labeled1 a


#endif
------------------------------------------------------------------------------
deriving instance (Known s, Bounded (f a)) => Bounded (Labeled1 f s a)
deriving instance (Known s, Enum (f a)) => Enum (Labeled1 f s a)
deriving instance (Known s, Ix (f a)) => Ix (Labeled1 f s a)
deriving instance (Known s, Storable (f a)) => Storable (Labeled1 f s a)
deriving instance (Known s, Monoid (f a)) => Monoid (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Num (f a)
    )
  => Num (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Real (f a)
    )
  => Real (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Integral (f a)
    )
  => Integral (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Fractional (f a)
    )
  => Fractional (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Floating (f a)
    )
  => Floating (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , RealFrac (f a)
    )
  => RealFrac (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , RealFloat (f a)
    )
  => RealFloat (Labeled1 f s a)
deriving instance
    ( Known s
#if !MIN_VERSION_base(4, 5, 0)
    , Show (Val s)
#endif
    , Bits (f a)
    )
  => Bits (Labeled1 f s a)
#if MIN_VERSION_base(4, 7, 0)
deriving instance (Known s, FiniteBits (f a)) => FiniteBits (Labeled1 f s a)
#endif
deriving instance (Known s, IsString (f a)) => IsString (Labeled1 f s a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Known s, Eq (Val s), Read (Val s), Read (f a)) =>
    Read (Labeled1 f s a)
  where
    readsPrec = readsData $
        readsUnaryWith (readsLabeled readsPrec) "Labeled1" Labeled1


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s), Read a) => Read (Field1 s a)
  where
    readsPrec = readsPrec1


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s), Read a) => Read (Option1 s a)
  where
    readsPrec = readsPrec1


------------------------------------------------------------------------------
instance __INCOHERENT__ (Show (Val s), Show (f a)) => Show (Labeled1 f s a)
  where
    showsPrec p (Labeled1 a) =
        showsUnaryWith (showsLabeled showsPrec) "Labeled1" p a


------------------------------------------------------------------------------
instance (Show (Val s), Show a) => Show (Field1 s a) where
    showsPrec = showsPrec1


------------------------------------------------------------------------------
instance (Show (Val s), Show a) => Show (Option1 s a) where
    showsPrec = showsPrec1
#ifdef GenericDeriving


------------------------------------------------------------------------------
type Labeled1MetaData = MetaData S.Labeled1 S.DataLabeled S.AnonymousData True
type Labeled1MetaCons = MetaCons S.Labeled1 PrefixI False
type Labeled1MetaSel
    = MetaSel Nothing NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
instance Known s => Generic (Labeled1 f (s :: Ks) a) where
    type Rep (Labeled1 f s a) = D1 Labeled1MetaData (C1 Labeled1MetaCons
        (S1 Labeled1MetaSel (Rep (Labeled f (Pair s a)))))
    from (Labeled1 a) = M1 (M1 (M1 (from a)))
    to (M1 (M1 (M1 a))) = Labeled1 (to a)


------------------------------------------------------------------------------
instance Known s => Generic1 (Labeled1 f (s :: Ks)) where
    type Rep1 (Labeled1 f s) =
        D1 Labeled1MetaData (C1 Labeled1MetaCons (S1 Labeled1MetaSel
        (D1 LabeledMetaData (C1 LabeledMetaCons (S1 (LabeledMetaSel s)
        (Rec1 f))))))
    from1 (Labeled1 (Labeled a)) = M1 (M1 (M1 (M1 (M1 (M1 (Rec1 a))))))
    to1 (M1 (M1 (M1 (M1 (M1 (M1 (Rec1 a))))))) = Labeled1 (Labeled a)
#endif


------------------------------------------------------------------------------
instance Eq1 f => Eq1 (Labeled1 f s) where
    liftEq eq (Labeled1 (Labeled a)) (Labeled1 (Labeled b)) = liftEq eq a b


------------------------------------------------------------------------------
instance Ord1 f => Ord1 (Labeled1 f s) where
    liftCompare cmp (Labeled1 (Labeled a)) (Labeled1 (Labeled b)) =
        liftCompare cmp a b


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Known s, Eq (Val s), Read (Val s), Read1 f) =>
    Read1 (Labeled1 f s)
  where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (readsLabeled (liftReadsPrec rp rl))
        "Labeled1"
        Labeled1


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s)) => Read1 (Field1 s) where
    liftReadsPrec rp rl p s = msum
        [ readsData (readsUnaryWith (readsField rp) "Field1" Labeled1) p s
        , readsData
            (readsUnaryWith (readsLabeled (liftReadsPrec rp rl))
                "Labeled1"
                Labeled1)
            p
            s
        ]


------------------------------------------------------------------------------
instance (Known s, Eq (Val s), Read (Val s)) => Read1 (Option1 s) where
    liftReadsPrec rp rl p s = msum
        [ readsData
            (readsUnaryWith (readsOption rp rl) "Option1" Labeled1)
            p
            s
{- TODO: Submit patch to make First, Last, Dual, Sum, Product, Alt,
    Option, Min, Max, First, Last, Arg instances of Eq1, Ord1, Read1, Show1
        , readsData
            (readsUnaryWith (readsLabeled (liftReadsPrec rp rl))
                "Labeled1"
                Labeled1)
            p
            s
-}
        ]


------------------------------------------------------------------------------
instance __INCOHERENT__ (Show (Val s), Show1 f) => Show1 (Labeled1 f s) where
    liftShowsPrec sp sl p (Labeled1 a) =
        showsUnaryWith (showsLabeled (liftShowsPrec sp sl)) "Labeled1" p a


------------------------------------------------------------------------------
instance Show (Val s) => Show1 (Field1 s) where
    liftShowsPrec sp _ p (Labeled1 a) =
        showsUnaryWith (showsField sp) "Field1" p a


------------------------------------------------------------------------------
instance Show (Val s) => Show1 (Option1 s) where
    liftShowsPrec sp sl p (Labeled1 a) =
        showsUnaryWith (showsOption sp sl) "Option1" p a


------------------------------------------------------------------------------
instance Functor f => Functor (Labeled1 f s) where
    fmap f (Labeled1 (Labeled a)) = Labeled1 (Labeled (fmap f a))
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Foldable f => Foldable (Labeled1 f s) where
    foldr f b (Labeled1 (Labeled a)) = foldr f b a
    {-# INLINE foldr #-}


------------------------------------------------------------------------------
instance Traversable f => Traversable (Labeled1 f s) where
    traverse f (Labeled1 (Labeled a)) = Labeled1 . Labeled <$> traverse f a
    {-# INLINE traverse #-}


------------------------------------------------------------------------------
instance (Known s, Applicative f) => Applicative (Labeled1 f s) where
    pure = Labeled1 . Labeled . pure
    {-# INLINE pure #-}
    Labeled1 (Labeled f) <*> Labeled1 (Labeled a) =
        Labeled1 (Labeled (f <*> a))
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance (Known s, Alternative f) => Alternative (Labeled1 f s) where
    empty = Labeled1 (Labeled empty)
    Labeled1 (Labeled a) <|> Labeled1 (Labeled b) =
        Labeled1 (Labeled (a <|> b))


------------------------------------------------------------------------------
instance (Known s, Monad f) => Monad (Labeled1 f s) where
    return = Labeled1 . Labeled . return
    {-# INLINE return #-}
    Labeled1 (Labeled a) >>= f = Labeled1 $ Labeled $ a >>= unwrap . f
    {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
instance (Known s, MonadPlus f) => MonadPlus (Labeled1 f s) where
    mzero = Labeled1 (Labeled mzero)
    mplus (Labeled1 (Labeled a)) (Labeled1 (Labeled b)) =
        Labeled1 (Labeled (mplus a b))

------------------------------------------------------------------------------
instance (Known s, MonadFix f) => MonadFix (Labeled1 f s) where
    mfix f = Labeled1 $ Labeled $ mfix (unwrap . f)
    {-# INLINE mfix #-}


------------------------------------------------------------------------------
unwrap :: Labeled1 f s a -> f a
unwrap (Labeled1 (Labeled a)) = a
{-# INLINE unwrap #-}
#if MIN_VERSION_base(4, 4, 0)


------------------------------------------------------------------------------
instance (Known s, MonadZip f) => MonadZip (Labeled1 f s) where
    munzip (Labeled1 (Labeled a)) = Labeled1 . Labeled *** Labeled1 . Labeled
        $ munzip a
    {-# INLINE munzip #-}
    mzipWith f (Labeled1 (Labeled a)) (Labeled1 (Labeled b)) =
        Labeled1 (Labeled (mzipWith f a b))
    {-# INLINE mzipWith #-}
#endif
