{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "kinds.h"
#include "overlap.h"

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Classes
    ( module Data.Classes.Internal
    )
where

-- anonymous-data ------------------------------------------------------------
import          Data.Classes.Internal
#if __GLASGOW_HASKELL__ >= 700


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>), Const (Const))
import           Control.Arrow (first)
import           Control.Category ((.))
import qualified Data.Bits as P
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
import qualified Data.Eq as P (Eq, (==), (/=))
import           Data.Functor (fmap)
import           Data.Functor.Compose (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))
import           Data.List (map)
import qualified Data.Ix as P (Ix, index, inRange, range, rangeSize)
import qualified Data.Monoid as P (Monoid, mappend, mconcat, mempty)
#if MIN_VERSION_base(4, 9, 0)
import qualified Data.Semigroup as P (Semigroup, (<>), sconcat, stimes)
#endif
import qualified Foreign.Storable as P
                     ( Storable
                     , alignment
                     , peek
                     , peekByteOff
                     , peekElemOff
                     , poke
                     , pokeByteOff
                     , pokeElemOff
                     , sizeOf
                     )
import qualified Prelude as P
                     ( Bounded
                     , maxBound
                     , minBound
                     , Enum
                     , enumFrom
                     , enumFromThen
                     , enumFromThenTo
                     , enumFromTo
                     , fromEnum
                     , pred
                     , succ
                     , toEnum
                     , Floating
                     , (**)
                     , acos
                     , acosh
                     , asin
                     , asinh
                     , atan
                     , atanh
                     , cos
                     , cosh
                     , exp
                     , log
                     , logBase
                     , pi
                     , sin
                     , sinh
                     , sqrt
                     , tan
                     , tanh
                     , Fractional
                     , (/)
                     , fromRational
                     , recip
                     , Integral
                     , div
                     , divMod
                     , mod
                     , quot
                     , quotRem
                     , rem
                     , toInteger
                     , Num
                     , (+)
                     , (-)
                     , (*)
                     , abs
                     , fromInteger
                     , negate
                     , signum
                     , Ord
                     , (<)
                     , (<=)
                     , (>)
                     , (>=)
                     , compare
                     , max
                     , min
                     , Read
                     , readList
                     , readsPrec
                     , Real
                     , toRational
                     , RealFloat
                     , atan2
                     , decodeFloat
                     , encodeFloat
                     , exponent
                     , floatDigits
                     , floatRadix
                     , floatRange
                     , isDenormalized
                     , isIEEE
                     , isInfinite
                     , isNaN
                     , isNegativeZero
                     , scaleFloat
                     , significand
                     , RealFrac
                     , ceiling
                     , floor
                     , properFraction
                     , round
                     , truncate
                     , Show
                     , show
                     , showList
                     , showsPrec
                     )


-- deepseq -------------------------------------------------------------------
import qualified Control.DeepSeq as P (NFData, rnf)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Bits a => Bits a where
    (.&.) = (P..&.)
    {-# INLINE (.&.) #-}
    (.|.) = (P..|.)
    {-# INLINE (.|.) #-}
    bit = P.bit
    {-# INLINE bit #-}
    bitSize = P.bitSize
    {-# INLINE bitSize #-}
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe = P.bitSizeMaybe
    {-# INLINE bitSizeMaybe #-}
#endif
    clearBit = P.clearBit
    {-# INLINE clearBit #-}
    complement = P.complement
    {-# INLINE complement #-}
    complementBit = P.complementBit
    {-# INLINE complementBit #-}
    isSigned = P.isSigned
    {-# INLINE isSigned #-}
#if MIN_VERSION_base(4, 5, 0)
    popCount = P.popCount
    {-# INLINE popCount #-}
#endif
    rotate = P.rotate
    {-# INLINE rotate #-}
    rotateL = P.rotateL
    {-# INLINE rotateL #-}
    rotateR = P.rotateR
    {-# INLINE rotateR #-}
    setBit = P.setBit
    {-# INLINE setBit #-}
    shift = P.shift
    {-# INLINE shift #-}
    shiftL = P.shiftL
    {-# INLINE shiftL #-}
    shiftR = P.shiftR
    {-# INLINE shiftR #-}
    testBit = P.testBit
    {-# INLINE testBit #-}
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL = P.unsafeShiftL
    {-# INLINE unsafeShiftL #-}
    unsafeShiftR = P.unsafeShiftR
    {-# INLINE unsafeShiftR #-}
#endif
    xor = P.xor
    {-# INLINE xor #-}
#if MIN_VERSION_base(4, 7, 0)
    zeroBits = P.zeroBits
    {-# INLINE zeroBits #-}
#endif


------------------------------------------------------------------------------
deriving instance Bits (f (g a)) => Bits (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Bounded a => Bounded a where
    maxBound = P.maxBound
    {-# INLINE maxBound #-}
    minBound = P.minBound
    {-# INLINE minBound #-}


------------------------------------------------------------------------------
deriving instance Bounded (f (g a)) => Bounded (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Enum a => Enum a where
    enumFrom = P.enumFrom
    {-# INLINE enumFrom #-}
    enumFromThen = P.enumFromThen
    {-# INLINE enumFromThen #-}
    enumFromThenTo = P.enumFromThenTo
    {-# INLINE enumFromThenTo #-}
    enumFromTo = P.enumFromTo
    {-# INLINE enumFromTo #-}
    fromEnum = toInteger . P.fromEnum
    {-# INLINE fromEnum #-}
    pred = P.pred
    {-# INLINE pred #-}
    succ = P.succ
    {-# INLINE succ #-}
    toEnum = P.toEnum . fromInteger
    {-# INLINE toEnum #-}


------------------------------------------------------------------------------
deriving instance Enum (f (g a)) => Enum (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Eq a => Eq a where
    (==) = (P.==)
    {-# INLINE (==) #-}
    (/=) = (P./=)
    {-# INLINE (/=) #-}


------------------------------------------------------------------------------
deriving instance Eq (f (g a)) => Eq (Compose f g a)


#if MIN_VERSION_base(4, 7, 0)
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.FiniteBits a => FiniteBits a where
    finiteBitSize = P.finiteBitSize
    {-# INLINE finiteBitSize #-}
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros = P.countLeadingZeros
    {-# INLINE countLeadingZeros #-}
    countTrailingZeros = P.countTrailingZeros
    {-# INLINE countTrailingZeros #-}
#endif


------------------------------------------------------------------------------
deriving instance FiniteBits (f (g a)) => FiniteBits (Compose f g a)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Fractional a => Fractional a where
    (/) = (P./)
    {-# INLINE (/) #-}
    fromRational = P.fromRational
    {-# INLINE fromRational #-}
    recip = P.recip
    {-# INLINE recip #-}


------------------------------------------------------------------------------
deriving instance Fractional (f (g a)) => Fractional (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (P.Floating a, Fractional a) => Floating a where
    (**) = (P.**)
    {-# INLINE (**) #-}
    acos = P.acos
    {-# INLINE acos #-}
    acosh = P.acosh
    {-# INLINE acosh #-}
    asin = P.asin
    {-# INLINE asin #-}
    asinh = P.asinh
    {-# INLINE asinh #-}
    atan = P.atan
    {-# INLINE atan #-}
    atanh = P.atanh
    {-# INLINE atanh #-}
    cos = P.cos
    {-# INLINE cos #-}
    cosh = P.cosh
    {-# INLINE cosh #-}
    exp = P.exp
    {-# INLINE exp #-}
    log = P.log
    {-# INLINE log #-}
    logBase = P.logBase
    {-# INLINE logBase #-}
    pi = P.pi
    {-# INLINE pi #-}
    sin = P.sin
    {-# INLINE sin #-}
    sinh = P.sinh
    {-# INLINE sinh #-}
    sqrt = P.sqrt
    {-# INLINE sqrt #-}
    tan = P.tan
    {-# INLINE tan #-}
    tanh = P.tanh
    {-# INLINE tanh #-}


------------------------------------------------------------------------------
deriving instance Floating (f (g a)) => Floating (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Integral a => Integral a where
    div = P.div
    {-# INLINE div #-}
    divMod = P.divMod
    {-# INLINE divMod #-}
    mod = P.mod
    {-# INLINE mod #-}
    quot = P.quot
    {-# INLINE quot #-}
    quotRem = P.quotRem
    {-# INLINE quotRem #-}
    rem = P.rem
    {-# INLINE rem #-}
    toInteger = P.toInteger
    {-# INLINE toInteger #-}


------------------------------------------------------------------------------
deriving instance Integral (f (g a)) => Integral (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Ix a => Ix a where
    index = \r -> toInteger . P.index r
    {-# INLINE index #-}
    inRange = P.inRange
    {-# INLINE inRange #-}
    range = P.range
    {-# INLINE range #-}
    rangeSize = toInteger . P.rangeSize
    {-# INLINE rangeSize #-}


------------------------------------------------------------------------------
deriving instance Ix (f (g a)) => Ix (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Monoid a => Monoid a where
    mconcat = P.mconcat
    {-# INLINE mconcat #-}
    mappend = P.mappend
    {-# INLINE mappend #-}
    mempty = P.mempty
    {-# INLINE mempty #-}


------------------------------------------------------------------------------
deriving instance Monoid (f (g a)) => Monoid (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.NFData a => NFData a where
    rnf = P.rnf
    {-# INLINE rnf #-}


------------------------------------------------------------------------------
deriving instance NFData (f (g a)) => NFData (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Num a => Num a where
    (+) = (P.+)
    {-# INLINE (+) #-}
    (-) = (P.-)
    {-# INLINE (-) #-}
    (*) = (P.*)
    {-# INLINE (*) #-}
    abs = P.abs
    {-# INLINE abs #-}
    fromInteger = P.fromInteger
    {-# INLINE fromInteger #-}
    negate = P.negate
    {-# INLINE negate #-}
    signum = P.signum
    {-# INLINE signum #-}


------------------------------------------------------------------------------
deriving instance Num (f (g a)) => Num (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Ord a => Ord a where
    (<) = (P.<)
    {-# INLINE (<) #-}
    (<=) = (P.<=)
    {-# INLINE (<=) #-}
    (>) = (P.>)
    {-# INLINE (>) #-}
    (>=) = (P.>=)
    {-# INLINE (>=) #-}
    compare = P.compare
    {-# INLINE compare #-}
    min = P.min
    {-# INLINE min #-}
    max = P.max
    {-# INLINE max #-}


------------------------------------------------------------------------------
deriving instance Ord (f (g a)) => Ord (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Read a => Read a where
    readsPrec = P.readsPrec
    {-# INLINE readsPrec #-}
    readList = P.readList
    {-# INLINE readList #-}


------------------------------------------------------------------------------
instance (Read (f (g a)), P.Read (Compose f g a)) => Read (Compose f g a)
  where
    readsPrec p s = fmap (first Compose) (readsPrec p s) <|> P.readsPrec p s
    readList s = fmap (first (map Compose)) (readList s) <|> P.readList s


------------------------------------------------------------------------------
instance (Read a, P.Read (Const a b)) => Read (Const a b) where
    readsPrec p s = fmap (first Const) (readsPrec p s) <|> P.readsPrec p s
    readList s = fmap (first (map Const)) (readList s) <|> P.readList s


------------------------------------------------------------------------------
instance (Read a, P.Read (Identity a)) => Read (Identity a) where
    readsPrec p s = fmap (first Identity) (readsPrec p s) <|> P.readsPrec p s
    readList s = fmap (first (map Identity)) (readList s) <|> P.readList s


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Real a => Real a where
    toRational = P.toRational
    {-# INLINE toRational #-}


------------------------------------------------------------------------------
deriving instance Real (f (g a)) => Real (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.RealFloat a => RealFloat a where
    atan2 = P.atan2
    {-# INLINE atan2 #-}
    decodeFloat = P.decodeFloat
    {-# INLINE decodeFloat #-}
    encodeFloat = P.encodeFloat
    {-# INLINE encodeFloat #-}
    exponent = P.exponent
    {-# INLINE exponent #-}
    floatDigits = P.floatDigits
    {-# INLINE floatDigits #-}
    floatRadix = P.floatRadix
    {-# INLINE floatRadix #-}
    floatRange = P.floatRange
    {-# INLINE floatRange #-}
    isDenormalized = P.isDenormalized
    {-# INLINE isDenormalized #-}
    isIEEE = P.isIEEE
    {-# INLINE isIEEE #-}
    isInfinite = P.isInfinite
    {-# INLINE isInfinite #-}
    isNaN = P.isNaN
    {-# INLINE isNaN #-}
    isNegativeZero = P.isNegativeZero
    {-# INLINE isNegativeZero #-}
    scaleFloat = P.scaleFloat
    {-# INLINE scaleFloat #-}
    significand = P.significand
    {-# INLINE significand #-}


------------------------------------------------------------------------------
deriving instance RealFloat (f (g a)) => RealFloat (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.RealFrac a => RealFrac a where
    ceiling = fromInteger . P.ceiling
    {-# INLINE ceiling #-}
    floor = fromInteger . P.floor
    {-# INLINE floor #-}
    properFraction = first fromInteger . P.properFraction
    {-# INLINE properFraction #-}
    round = fromInteger . P.round
    {-# INLINE round #-}
    truncate = fromInteger . P.truncate
    {-# INLINE truncate #-}


------------------------------------------------------------------------------
deriving instance RealFrac (f (g a)) => RealFrac (Compose f g a)


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Semigroup a => Semigroup a where
    (<>) = (P.<>)
    {-# INLINE (<>) #-}
    sconcat = P.sconcat
    {-# INLINE sconcat #-}
    stimes = P.stimes . toInteger 
    {-# INLINE stimes #-}


------------------------------------------------------------------------------
deriving instance Semigroup (f (g a)) => Semigroup (Compose f g a)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Show a => Show a where
    show = P.show
    {-# INLINE show #-}
    showList = P.showList
    {-# INLINE showList #-}
    showsPrec = P.showsPrec
    {-# INLINE showsPrec #-}


------------------------------------------------------------------------------
deriving instance Show (f (g a)) => Show (Compose f g a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Storable a => Storable a where
    alignment = P.alignment
    {-# INLINE alignment #-}
    peek = P.peek
    {-# INLINE peek #-}
    peekByteOff = P.peekByteOff
    {-# INLINE peekByteOff #-}
    peekElemOff = P.peekElemOff
    {-# INLINE peekElemOff #-}
    poke = P.poke
    {-# INLINE poke #-}
    pokeByteOff = P.pokeByteOff
    {-# INLINE pokeByteOff #-}
    pokeElemOff = P.pokeElemOff
    {-# INLINE pokeElemOff #-}
    sizeOf = P.sizeOf
    {-# INLINE sizeOf #-}


------------------------------------------------------------------------------
deriving instance Storable (f (g a)) => Storable (Compose f g a)
#endif
