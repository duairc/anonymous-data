{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"
#include "overlap.h"

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Classes
    ( Bits
        ( (.&.)
        , (.|.)
        , bit
        , bitSize
#if MIN_VERSION_base(4, 7, 0)
        , bitSizeMaybe
#endif
        , clearBit
        , complement
        , complementBit
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
    , Bounded (maxBound, minBound)
    , Enum
        ( enumFrom
        , enumFromThen
        , enumFromThenTo
        , enumFromTo
        , fromEnum
        , pred
        , succ
        , toEnum
        )
    , Eq ((==), (/=))
#if MIN_VERSION_base(4, 7, 0)
    , FiniteBits
        ( finiteBitSize
#if MIN_VERSION_base(4, 8, 0)
        , countLeadingZeros
        , countTrailingZeros
#endif
        )
#endif
    , Floating
        ( (**)
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
        )
    , Fractional ((/), fromRational, recip)
    , Integral (div, divMod, mod, quot, quotRem, rem, toInteger)
    , Ix (index, inRange, range, rangeSize)
    , Monoid (mappend, mconcat, mempty)
    , NFData (rnf)
    , Num ((+), (-), (*), abs, fromInteger, negate, signum)
    , Ord
        ( (<)
        , (<=)
        , (>)
        , (>=)
        , compare
        , max
        , min
        )
    , Read (readList, readsPrec)
    , Real (toRational)
    , RealFrac (ceiling, floor, properFraction, round, truncate)
    , RealFloat
        ( atan2
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
        )
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup ((<>), sconcat, stimes)
#endif
    , Show (show, showList, showsPrec)
    , Storable
         ( alignment
         , peek
         , peekByteOff
         , peekElemOff
         , poke
         , pokeByteOff
         , pokeElemOff
         , sizeOf
         )
    )
where

#if __GLASGOW_HASKELL__ < 700
-- base ----------------------------------------------------------------------
import           Data.Bits (Bits (..))
import           Data.Ix (Ix (..))
import           Data.Monoid (Monoid (..))
import           Foreign.Storable (Storable (..))
import           Prelude


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData (..))
#else
-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const), (<|>), empty)
import           Control.Arrow (first)
import           Control.Category ((.), id)
import           Control.Monad
                     (
#if __GLASGOW_HASKELL__ < 700
                       (>>)
                     , (>>=)
                     , fail
                     ,
#endif
                       return
                     )
import qualified Data.Bits as P
import           Data.Bool (Bool (False, True), (&&), (||), not, otherwise)
import           Data.Foldable (foldr)
import           Data.Function (($))
import           Data.Functor (fmap)
import           Data.Functor.Compose (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int)
import qualified Data.Ix as P
import           Data.List (map)
#if MIN_VERSION_base(4, 9, 0)
import           Data.List.NonEmpty (NonEmpty ((:|)))
#endif
#if MIN_VERSION_base(4, 7, 0)
import           Data.Maybe (Maybe)
#endif
import qualified Data.Monoid as P hiding
                     (
#if MIN_VERSION_base(4, 5, 0)
                       (<>)
#endif
                     )
import           Data.Ord (Ordering (EQ, GT, LT))
#if MIN_VERSION_base(4, 9, 0)
import qualified Data.Semigroup as P
#endif
import           Data.Tuple (fst, snd)
import           Foreign.Ptr (Ptr, plusPtr)
import qualified Foreign.Storable as P
import           Prelude
                     ( Integer
                     , IO
                     , Rational
                     , ReadS
                     , ShowS
                     , String
#if (MIN_VERSION_base(4, 5, 0) && !MIN_VERSION_base(4, 7, 0)) \
    || MIN_VERSION_base(4, 9, 0)
                     , error
#endif
                     , lex
                     , subtract
                     , undefined
                     )
import qualified Prelude as P


-- deepseq -------------------------------------------------------------------
import qualified Control.DeepSeq as P (NFData, rnf)


------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 7, 0)
{-# DEPRECATED bitSize "Use 'bitSizeMaybe' or 'finiteBitSize' instead" #-}
#endif
class
#if MIN_VERSION_base(4, 6, 0)
    Eq a
#elif MIN_VERSION_base(4, 5, 0)
    (Eq a, Num a)
#else
    Num a
#endif
  =>
    Bits a
  where
#ifdef MinimalPragma
    {-# MINIMAL
          (.&.)
        , (.|.)
        , bit
        , bitSize
        , bitSizeMaybe
        , complement
        , isSigned
        , popCount
        , (rotate | (rotateL, rotateR))
        , (shift | (shiftL, shiftR))
        , testBit
        , xor
        #-}
#endif
    (.&.) :: a -> a -> a
    infixl 7 .&.

    (.|.) :: a -> a -> a
    infixl 5 .|.

    bit :: Int -> a
#if !MIN_VERSION_base(4, 6, 0)
    bit = shiftL (fromInteger 1)
    {-# INLINE bit #-}
#endif
    bitSize :: a -> Int
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe :: a -> Maybe Int
#endif
    clearBit :: a -> Int -> a
    clearBit a i = a .&. complement (bit i)
    {-# INLINE clearBit #-}

    complement :: a -> a

    complementBit :: a -> Int -> a
    complementBit a i = xor a (bit i)
    {-# INLINE complementBit #-}

    isSigned :: a -> Bool
#if MIN_VERSION_base(4, 5, 0)
    popCount :: a -> Int
#if !MIN_VERSION_base(4, 6, 0)
    popCount = go (P.fromInteger 0)
      where
        go c w
            | w == fromInteger 0 = c
            | otherwise =
                go (c P.+ P.fromInteger 1) (w .&. w - fromInteger 1)
#endif
#endif
    rotate :: a -> Int -> a
    rotate a i
        | i P.< P.fromInteger 0 = rotateR a (P.negate i)
        | i P.> P.fromInteger 0 = rotateL a i
        | otherwise = a
    infixl 8 `rotate`

    rotateL :: a -> Int -> a
    rotateL a i = rotate a i
    infixl 8 `rotateL`
    {-# INLINE rotateL #-}

    rotateR :: a -> Int -> a
    rotateR a i = rotate a (P.negate i)
    infixl 8 `rotateR`
    {-# INLINE rotateR #-}

    setBit :: a -> Int -> a
    setBit a i = a .|. bit i
    {-# INLINE setBit #-}

    shift :: a -> Int -> a
    shift a i
        | i P.< P.fromInteger 0 = shiftR a (P.negate i)
        | i P.> P.fromInteger 0 = shiftL a i
        | otherwise = a
    infixl 8 `shift`

    shiftL :: a -> Int -> a
    shiftL a i = shift a i
    infixl 8 `shiftL`
    {-# INLINE shiftL #-}

    shiftR :: a -> Int -> a
    shiftR a i = shift a (P.negate i)
    infixl 8 `shiftR`
    {-# INLINE shiftR #-}

    testBit :: a -> Int -> Bool
#if !MIN_VERSION_base(4, 6, 0)
    testBit a i = (a .&. bit i) /= fromInteger 0
    {-# INLINE testBit #-}

#endif
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL :: a -> Int -> a
    unsafeShiftL = shiftL
    {-# INLINE unsafeShiftL #-}

    unsafeShiftR :: a -> Int -> a
    unsafeShiftR = shiftR
    {-# INLINE unsafeShiftR #-}

#endif
    xor :: a -> a -> a
    infixl 6 `xor`
#if MIN_VERSION_base(4, 7, 0)

    zeroBits :: a
    zeroBits = clearBit (bit (P.fromInteger 0)) (P.fromInteger 0)
#endif


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
class Bounded a where
    maxBound :: a
    minBound :: a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Bounded a => Bounded a where
    maxBound = P.maxBound
    {-# INLINE maxBound #-}
    minBound = P.minBound
    {-# INLINE minBound #-}


------------------------------------------------------------------------------
deriving instance Bounded (f (g a)) => Bounded (Compose f g a)


------------------------------------------------------------------------------
class Enum a where
    enumFrom :: a -> [a]
    enumFrom a = map toEnum $ P.enumFrom (fromEnum a)

    enumFromThen :: a -> a -> [a]
    enumFromThen a b = map toEnum $ P.enumFromThen (fromEnum a) (fromEnum b)

    enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo a b c = map toEnum $
        P.enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c)

    enumFromTo :: a -> a -> [a]
    enumFromTo a b = map toEnum $ P.enumFromTo (fromEnum a) (fromEnum b)

    fromEnum :: a -> Integer

    pred :: a -> a
    pred = toEnum . (subtract 1) . fromEnum

    succ :: a -> a
    succ = toEnum . (P.+ 1)  . fromEnum

    toEnum :: Integer -> a


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
class Eq a where
#ifdef MinimalPragma
    {-# MINIMAL (==) | (/=) #-}
#endif
    (==) :: a -> a -> Bool
    a == b = not (a /= b)
    infix 4 ==
    {-# INLINE (==) #-}

    (/=) :: a -> a -> Bool
    a /= b = not (a == b)
    infix 4 /=
    {-# INLINE (/=) #-}


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
class Bits a => FiniteBits a where
    finiteBitSize :: a -> Int
#if MIN_VERSION_base(4, 8, 0)

    countLeadingZeros :: a -> Int
    countLeadingZeros x =
        (w P.- P.fromInteger 1) P.- go (w P.- P.fromInteger 1)
      where
        go i
            | i P.< P.fromInteger 0 = i
            | testBit x i = i
            | otherwise = go (i P.- P.fromInteger 1)
        w = finiteBitSize x

    countTrailingZeros :: a -> Int
    countTrailingZeros x = go (P.fromInteger 0)
      where
        go i
            | i P.>= w = i
            | testBit x i = i
            | otherwise = go (i P.+ P.fromInteger 1)
        w = finiteBitSize x
#endif


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
class Fractional a => Floating a where
    (**) :: a -> a -> a
    a ** b = exp (log a * b)
    infixr 8 **
    {-# INLINE (**) #-}

    acos :: a -> a
    acosh :: a -> a
    asin :: a -> a
    asinh :: a -> a
    atan :: a -> a
    atanh :: a -> a
    cos :: a -> a
    cosh :: a -> a
    exp :: a -> a
    log :: a -> a

    logBase :: a -> a -> a
    logBase a b = log b / log a
    {-# INLINE logBase #-}

    pi :: a

    sin :: a -> a
    sinh :: a -> a

    sqrt :: a -> a
    sqrt = (** fromRational 0.5)
    {-# INLINE sqrt #-}

    tan :: a -> a
    tan a = sin a / cos a
    {-# INLINE tan #-}

    tanh :: a -> a
    tanh a = sinh a / cosh a
    {-# INLINE tanh #-}


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
class Num a => Fractional a where
#ifdef MinimalPragma
    {-# MINIMAL ((/) | recip), fromRational #-}
#endif
    (/) :: a -> a -> a
    x / y = x * recip y
    infixl 7 /
    {-# INLINE (/) #-}

    fromRational :: Rational -> a

    recip :: a -> a
    recip x = fromInteger 1 / x
    {-# INLINE recip #-}


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
class (Real a, Enum a) => Integral a where
    div :: a -> a -> a
    div a b = fst $ divMod a b
    infixl 7 `div`
    {-# INLINE div #-}

    divMod :: a -> a -> (a, a)
    divMod a b = if signum a == negate (signum b)
        then (q - fromInteger 1, r + b)
        else qr
      where
        qr@(q, r) = quotRem a b

    mod :: a -> a -> a
    mod a b = snd $ divMod a b
    infixl 7 `mod`
    {-# INLINE mod #-}

    quot :: a -> a -> a
    quot a b = fst $ quotRem a b
    infixl 7 `quot`
    {-# INLINE quot #-}

    quotRem :: a -> a -> (a, a)

    rem :: a -> a -> a
    rem a b = snd $ quotRem a b
    infixl 7 `rem`
    {-# INLINE rem #-}

    toInteger :: a -> Integer


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
class Ord a => Ix a where
#ifdef MinimalPragma
    {-# MINIMAL index, inRange, range #-}
#endif
    index :: (a, a) -> a -> Integer
    inRange :: (a, a) -> a -> Bool
    range :: (a, a) -> [a]

    rangeSize :: (a, a) -> Integer
    rangeSize b@(_, h)
        | inRange b h = index b h P.+ 1
        | otherwise = 0


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
class Monoid a where
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
    mappend :: a -> a -> a
    mempty :: a


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
class NFData a where
    rnf :: a -> ()


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.NFData a => NFData a where
    rnf = P.rnf
    {-# INLINE rnf #-}


------------------------------------------------------------------------------
deriving instance NFData (f (g a)) => NFData (Compose f g a)


------------------------------------------------------------------------------
class
#if !MIN_VERSION_base(4, 5, 0)
    (Eq a, Show a)
  =>
#endif
    Num a
  where
#ifdef MinimalPragma
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
#endif
    (+) :: a -> a -> a
    infixl 6 +
    (-) :: a -> a -> a
    a - b = a + negate b
    infixl 6 -
    {-# INLINE (-) #-}
    (*) :: a -> a -> a
    infixl 7 *
    abs :: a -> a
    fromInteger :: Integer -> a
    negate :: a -> a
    negate a = fromInteger 0 - a
    {-# INLINE negate #-}
    signum :: a -> a


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
class Eq a => Ord a where
#ifdef MinimalPragma
    {-# MINIMAL (<=) | compare #-}
#endif
    (<) :: a -> a -> Bool
    a < b = case compare a b of
        LT -> True
        _ -> False

    (<=) :: a -> a -> Bool
    a <= b = case compare a b of
        GT -> False
        _ -> True

    (>) :: a -> a -> Bool
    a >  b = case compare a b of
        GT -> True
        _ -> False

    (>=) :: a -> a -> Bool
    a >= b = case compare a b of
        LT -> False
        _ -> True

    compare :: a -> a -> Ordering
    compare a b = if a == b
        then EQ
        else if a <= b
            then LT
            else GT

    max :: a -> a -> a
    max a b = if a <= b then b else a

    min :: a -> a -> a
    min a b = if a <= b then a else b


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
class Read a where
#ifdef MinimalPragma
    {-# MINIMAL readsPrec #-}
#endif
    readsPrec :: Int -> ReadS a
    readList :: ReadS [a]
    readList s = do
        ("[", s') <- lex s
        go id s' <|> do
            ("]", s'') <- lex s'
            return ([], s'')
      where
        go dlist s' = do
            (a, s'') <- readsPrec (P.fromInteger 0) s'
            (c, s''') <- lex s''
            case c of
                "]" -> return (dlist [a], s''')
                "," -> go (dlist . (a :)) s'''
                _ -> empty


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
class (Num a, Ord a) => Real a where
    toRational :: a -> Rational


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ P.Real a => Real a where
    toRational = P.toRational
    {-# INLINE toRational #-}


------------------------------------------------------------------------------
deriving instance Real (f (g a)) => Real (Compose f g a)


------------------------------------------------------------------------------
class (RealFrac a, Floating a) => RealFloat a where
    atan2 :: a -> a -> a
    atan2 b a
        | a > fromInteger 0 = atan (b / a)
        | a == fromInteger 0 && b > fromInteger 0 = pi / fromInteger 2
        | a < fromInteger 0 && b > fromInteger 0 = pi + atan (b / a)
        | a <= fromInteger 0 && b < fromInteger 0
            || a < fromInteger 0 && isNegativeZero b
            || isNegativeZero a && isNegativeZero b
          = negate (atan2 (negate b) a)
        | b == fromInteger 0 && (a < fromInteger 0 || isNegativeZero a)
            = pi
        | a == fromInteger 0 && b == fromInteger 0 = b
        | otherwise = a + b

    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a

    exponent :: a -> Int
    exponent a = if m P.== P.fromInteger 0
        then P.fromInteger 0
        else n P.+ floatDigits a
      where
        (m, n) = decodeFloat a

    floatDigits :: a -> Int
    floatRadix :: a -> Integer
    floatRange :: a -> (Int, Int)
    isDenormalized :: a -> Bool
    isIEEE :: a -> Bool
    isInfinite :: a -> Bool
    isNaN :: a -> Bool
    isNegativeZero :: a -> Bool

    scaleFloat :: Int -> a -> a
    scaleFloat k a = encodeFloat m (n P.+ clamp b k)
      where
        (m, n) = decodeFloat a
        (l, h) = floatRange a
        d = floatDigits a
        b = h P.- l P.+ P.fromInteger 4 P.* d
        clamp bd j = P.max (P.negate bd) (P.min bd j)

    significand :: a -> a
    significand a =
        encodeFloat (fst (decodeFloat a)) (P.negate (floatDigits a))


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
class (Real a, Fractional a) => RealFrac a where
    ceiling :: Integral b => a -> b
    ceiling a = if r > fromInteger 0
        then n + fromInteger 1
        else n
      where
        (n, r) = properFraction a

    floor :: Integral b => a -> b
    floor a = if r < fromInteger 0
        then n - fromInteger 1
        else n
      where
        (n, r) = properFraction a

    properFraction :: Integral b => a -> (b, a)

    round :: Integral b => a -> b
    round a = do
        let (n, r) = properFraction a
        let m = if r < fromInteger 0
             then n - fromInteger 1
             else n + fromInteger 1
        case compare (signum (abs r - fromRational 0.5)) (fromInteger 0) of
            LT -> n
            EQ
                | rem n (fromInteger 2) == fromInteger 0 -> n
                | otherwise -> m
            GT -> m

    truncate :: Integral b => a -> b
    truncate = fst . properFraction
    {-# INLINE truncate #-}


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
class Semigroup a where
    (<>) :: a -> a -> a
    infixr 6 <>

    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as
      where
        go b (c : cs) = b <> go c cs
        go b [] = b

    stimes :: Integral b => b -> a -> a
    stimes y0 x0
        | y0 <= fromInteger 0 =
            error "stimes: positive multiplier expected"
        | otherwise = f x0 y0
      where
        f x y
            | rem y (fromInteger 2) == fromInteger 0
                = f (x <> x) (quot y (fromInteger 2))
            | y == fromInteger 1 = x
            | otherwise = g (x <> x) (quot (pred y) (fromInteger 2)) x
        g x y z
            | rem y (fromInteger 2) == fromInteger 0 =
                g (x <> x) (quot y (fromInteger 2)) z
            | y == fromInteger 1 = x <> z
            | otherwise =
                g (x <> x) (quot (pred y) (fromInteger 2)) (x <> z)


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
class Show a where
#ifdef MinimalPragma
    {-# MINIMAL show | showsPrec #-}
#endif
    show :: a -> String
    show a = showsPrec (P.fromInteger 0) a ""

    showList :: [a] -> ShowS
    showList [] s = P.mappend "[]" s
    showList (a : as) s = '[' : showsPrec (P.fromInteger 0) a (go as)
      where
        go [] = ']' : s
        go (a' : as') = ',' : showsPrec (P.fromInteger 0) a' (go as')

    showsPrec :: Int -> a -> ShowS
    showsPrec _ a s = P.mappend (show a) s


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
class Storable a where
#ifdef MinimalPragma
    {-# MINIMAL
          alignment
        , (peek | peekByteOff | peekElemOff)
        , (poke | pokeByteOff | pokeElemOff)
        , sizeOf
        #-}
#endif
    alignment :: a -> Int

    peek :: Ptr a -> IO a
    peek p = peekElemOff p (P.fromInteger 0)

    peekByteOff :: Ptr b -> Int -> IO a
    peekByteOff p i = peek (plusPtr p i)

    peekElemOff :: Ptr a -> Int -> IO a
    peekElemOff p i = peekByteOff p (i P.* sizeOf (undefined :: a))

    poke :: Ptr a -> a -> IO ()
    poke p = pokeElemOff p (P.fromInteger 0)

    pokeByteOff :: Ptr b -> Int -> a -> IO ()
    pokeByteOff p i = poke (plusPtr p i)

    pokeElemOff :: Ptr a -> Int -> a -> IO ()
    pokeElemOff p i a = pokeByteOff p (i P.* sizeOf a) a

    sizeOf :: a -> Int


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
