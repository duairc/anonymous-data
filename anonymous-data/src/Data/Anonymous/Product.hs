{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "kinds.h"
#include "overlap.h"

#ifdef UseAmbiguousTypes
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

#ifdef ClosedTypeFamilies
{-# LANGUAGE ConstraintKinds #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef LanguagePatternSynonyms
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Anonymous.Product
    ( Product (Cons, Nil)
    , Tuple
    , Record
    , Options

    , (<::>)
    , (<:>)
    , (<:.>)
    , (<:?>)
#ifdef LanguagePatternSynonyms
    , pattern (:<::>)
    , pattern (:<:>)
    , pattern (:<:.>)
    , pattern (:<:?>)
#endif

    , (:<++>) ((<++>))

    , fromOptions
    , fromOptionsNoDefaults

    , LookupIndex'
#ifdef ClosedTypeFamilies
    , LookupIndex
#endif
    , lookupIndex'
    , lookupIndex
    , UpdateIndex'
#ifdef ClosedTypeFamilies
    , UpdateIndex
#endif
    , updateIndex'
    , updateIndex
    , index'
    , index
    , LookupElement'
#ifdef ClosedTypeFamilies
    , LookupElement
#endif
    , lookupElement'
    , lookupElement
    , UpdateElement'
#ifdef ClosedTypeFamilies
    , UpdateElement
#endif
    , updateElement'
    , updateElement
    , element'
    , element
    , LookupKey'
#ifdef ClosedTypeFamilies
    , LookupKey
#endif
    , lookupKey'
    , lookupKey
    , UpdateKey'
#ifdef ClosedTypeFamilies
    , UpdateKey
#endif
    , updateKey'
    , updateKey
    , key'
    , key
    , LookupValue'
#ifdef ClosedTypeFamilies
    , LookupValue
#endif
    , lookupValue'
    , lookupValue
    , UpdateValue'
#ifdef ClosedTypeFamilies
    , UpdateValue
#endif
    , updateValue'
    , updateValue
    , value'
    , value
    , LookupIndices'
#ifdef ClosedTypeFamilies
    , LookupIndices
#endif
    , lookupIndices'
    , lookupIndices
    , UpdateIndices'
#ifdef ClosedTypeFamilies
    , UpdateIndices
#endif
    , updateIndices'
    , updateIndices
    , indices'
    , indices
    , LookupElements'
#ifdef ClosedTypeFamilies
    , LookupElements
#endif
    , lookupElements'
    , lookupElements
    , UpdateElements'
#ifdef ClosedTypeFamilies
    , UpdateElements
#endif
    , updateElements'
    , updateElements
    , elements'
    , elements
    , LookupKeys'
#ifdef ClosedTypeFamilies
    , LookupKeys
#endif
    , lookupKeys'
    , lookupKeys
    , UpdateKeys'
#ifdef ClosedTypeFamilies
    , UpdateKeys
#endif
    , updateKeys'
    , updateKeys
    , keys'
    , keys
    , LookupValues'
#ifdef ClosedTypeFamilies
    , LookupValues
#endif
    , lookupValues'
    , lookupValues
    , UpdateValues'
#ifdef ClosedTypeFamilies
    , UpdateValues
#endif
    , updateValues'
    , updateValues
    , values'
    , values
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Field (Field (Field), Option (Option))
#ifdef GenericDeriving
import qualified Symbols as S
#endif
#ifdef ClosedTypeFamilies
import qualified Type.List.Fields as T
#endif


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Control.Monad (guard, msum)
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
import           Data.Functor.Identity (Identity (Identity))
import           Data.Ix (Ix, inRange, range)
import qualified Data.Ix (index)
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
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
#ifdef GenericDeriving
import           GHC.Generics.Compat
                     ( (:*:) ((:*:))
                     , D1
                     , C1
                     , Generic
                     , K1 (K1)
                     , M1 (M1)
                     , S1
                     , U1 (U1)
                     , Rec0
                     , Rep
                     , PrefixI
                     , DecidedStrict
                     , MetaCons
                     , MetaData
                     , MetaSel
                     , NoSourceUnpackedness
                     , SourceStrict
                     , from
                     , to
                     )
#endif
import           GHC.TypeLits.Compat
                     ( (:-)
                     , KnownSymbol
#ifdef DataPolyKinds
                     , Nat
#ifdef ClosedTypeFamilies
                     , Symbol
#endif
#endif
                     , One
                     , Zero
                     , symbolVal
                     )
#ifdef GenericDeriving
import           Type.Bool (False, True)
#endif
import           Type.List (Cons, Nil)
#ifdef GenericDeriving
import           Type.Maybe (Just, Nothing)
#endif
import           Type.Meta (Proxy (Proxy))
#if __GLASGOW_HASKELL__ < 700
import           Type.Natural (Natural)
#endif
import           Type.Semigroup ((:<>))
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
data Product (f :: KPoly1 -> *) (as :: KList (KPoly1)) where
    Nil :: Product f Nil
    Cons :: !(f a) -> !(Product f as) -> Product f (Cons a as)
#if defined(PolyTypeable)
  deriving (Typeable)
#endif
infixr 5 `Cons`


------------------------------------------------------------------------------
instance Eq (Product g Nil) where
    Nil == Nil = True


------------------------------------------------------------------------------
instance (Eq (g a), Eq (Product g as)) => Eq (Product g (Cons a as)) where
    Cons a as == Cons b bs = a == b && as == bs


------------------------------------------------------------------------------
instance Ord (Product g Nil) where
    compare Nil Nil = EQ


------------------------------------------------------------------------------
instance (Ord (g a), Ord (Product g as)) => Ord (Product g (Cons a as))
  where
    compare (Cons a as) (Cons b bs) = mappend (compare a b) (compare as bs)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ PlainRead g as => Read (Product g as) where
    readsPrec _ = plainReads


------------------------------------------------------------------------------
instance (ReadHelper Identity as, PlainRead Identity as) => Read (Tuple as)
  where
    readsPrec _ s = msum
        [ do
            ("(", s') <- lex s
            (as, s'') <- readsHelper s'
            (")", s''') <- lex s''
            return (as, s''')
        , plainReads s
        ]


------------------------------------------------------------------------------
instance (ReadHelper (Const b) as, PlainRead (Const b) as) =>
    Read (Product (Const b) as)
  where
    readsPrec _ s = msum
        [ do
            ("[", s') <- lex s
            (as, s'') <- readsHelper s'
            ("]", s''') <- lex s''
            return (as, s''')
        , plainReads s
        ]


------------------------------------------------------------------------------
instance (ReadHelper Field as, PlainRead Field as) => Read (Record as) where
    readsPrec _ s = msum
        [ do
            ("{", s') <- lex s
            (as, s'') <- readsHelper s'
            ("}", s''') <- lex s''
            return (as, s''')
        , plainReads s
        ]


------------------------------------------------------------------------------
instance (ReadHelper Option as, PlainRead Option as) => Read (Options as)
  where
    readsPrec _ s = msum
        [ do
            ("{", s') <- lex s
            (as, s'') <- readsHelper s'
            ("}", s''') <- lex s''
            return (as, s''')
        , plainReads s
        ]


------------------------------------------------------------------------------
class PlainRead g as where
    plainReads :: ReadS (Product g as)


------------------------------------------------------------------------------
instance PlainRead g Nil where
    plainReads s = do
        ("<>", s') <- lex s
        return (Nil, s')


------------------------------------------------------------------------------
instance PlainReadHelper g (Cons a as) => PlainRead g (Cons a as) where
    plainReads s = do
        ("<", s') <- lex s
        (as, s'') <- plainReadsHelper s'
        (">", s''') <- lex s''
        return (as, s''')


------------------------------------------------------------------------------
class PlainReadHelper g as where
    plainReadsHelper :: ReadS (Product g as)


------------------------------------------------------------------------------
instance PlainReadHelper g Nil where
    plainReadsHelper s = return (Nil, s)


------------------------------------------------------------------------------
instance Read (g a) => PlainReadHelper g (Cons a Nil) where
    plainReadsHelper s = do
        (a, s') <- readsPrec 0 s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read (g a), PlainReadHelper g as) =>
    PlainReadHelper g (Cons a as)
  where
    plainReadsHelper s = do
        (a, s') <- readsPrec 0 s
        (",", s'') <- lex s'
        (as, s''') <- plainReadsHelper s''
        return (Cons a as, s''')


------------------------------------------------------------------------------
class ReadHelper g as where
    readsHelper :: ReadS (Product g as)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ PlainReadHelper g as => ReadHelper g as where
    readsHelper = plainReadsHelper


------------------------------------------------------------------------------
instance Read a => ReadHelper Identity (Cons a Nil) where
    readsHelper s = do
        (a, s') <- readsPrec 0 s
        return (Cons (Identity a) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read a, ReadHelper Identity as) =>
    ReadHelper Identity (Cons a as)
  where
    readsHelper s = do
        (a, s') <- readsPrec 0 s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Identity a) as, s''')


------------------------------------------------------------------------------
instance Read b => ReadHelper (Const b) (Cons a Nil) where
    readsHelper s = do
        (b, s') <- readsPrec 0 s
        return (Cons (Const b) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read b, ReadHelper (Const b) as) =>
    ReadHelper (Const b) (Cons a as)
  where
    readsHelper s = do
        (b, s') <- readsPrec 0 s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Const b) as, s''')


------------------------------------------------------------------------------
instance (Read a, KnownSymbol s) =>
    ReadHelper Field (Cons (Pair s a) Nil)
  where
    readsHelper s = do
        (a, s') <- readsField s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read a, KnownSymbol s, ReadHelper Field as) =>
    ReadHelper Field (Cons (Pair s a) as)
  where
    readsHelper s = do
        (a, s') <- readsField s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons a as, s''')


------------------------------------------------------------------------------
instance (Read a, KnownSymbol s) => ReadHelper Option (Cons (Pair s a) Nil)
  where
    readsHelper s = do
        (a, s') <- readsOption (return . (,) ()) s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read a, KnownSymbol s, ReadHelper Option as) =>
    ReadHelper Option (Cons (Pair s a) as)
  where
    readsHelper s = do
        (a, s') <- readsOption comma s
        (as, s'') <- readsHelper s'
        return (Cons a as, s'')
      where
        comma s' = do
            (",", s'') <- lex s'
            return ((), s'')


------------------------------------------------------------------------------
readsField :: forall a s. (Read a, KnownSymbol s)
    => ReadS (Field (Pair s a))
readsField s = do
    (label, s') <- lex s
    guard $ label == symbolVal (Proxy :: Proxy s)
    ("=", s'') <- lex s'
    (a, s''') <- readsPrec 0 s''
    return $ (Field a, s''')


------------------------------------------------------------------------------
readsOption :: forall a b s. (Read a, KnownSymbol s)
    => ReadS b
    -> ReadS (Option (Pair s a))
readsOption f s = msum
    [ do
        (label, s') <- lex s
        guard $ label == symbolVal (Proxy :: Proxy s)
        ("=", s'') <- lex s'
        (a, s''') <- readsPrec 0 s''
        (_, s'''') <- f s'''
        return (Option (Just a), s'''')
    , return (Option Nothing, s)
    ]


------------------------------------------------------------------------------
instance __INCOHERENT__ ShowHelper g as => Show (Product g as) where
    showsPrec _ as = foldr (.) id $
        [ showString "<"
        , showsHelper as
        , showString ">"
        ]


------------------------------------------------------------------------------
instance ShowHelper Identity as => Show (Tuple as) where
    showsPrec _ as = foldr (.) id $
        [ showString "("
        , showsHelper as
        , showString ")"
        ]


------------------------------------------------------------------------------
instance ShowHelper (Const b) as => Show (Product (Const b) as) where
    showsPrec _ as = foldr (.) id $
        [ showString "["
        , showsHelper as
        , showString "]"
        ]


------------------------------------------------------------------------------
instance ShowHelper Field as => Show (Record as) where
    showsPrec _ as = foldr (.) id $
        [ showString "{"
        , showsHelper as
        , showString "}"
        ]


------------------------------------------------------------------------------
instance ShowHelper Option as => Show (Options as) where
    showsPrec _ as = foldr (.) id $
        [ showString "{"
        , showsHelper as
        , showString "}"
        ]


------------------------------------------------------------------------------
class ShowHelper g as where
    showsHelper :: Product g as -> ShowS


------------------------------------------------------------------------------
instance ShowHelper g Nil where
    showsHelper Nil = id


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    (Show (g a), ShowHelper g as)
  =>
    ShowHelper g (Cons a as)
  where
    showsHelper (Cons a Nil) = showsPrec 0 a
    showsHelper (Cons a as) = foldr (.) id $
        [ showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show a, ShowHelper Identity as) => ShowHelper Identity (Cons a as)
  where
    showsHelper (Cons (Identity a) Nil) = showsPrec 0 a
    showsHelper (Cons (Identity a) as) = foldr (.) id $
        [ showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show b, ShowHelper (Const b) as) =>
    ShowHelper (Const b) (Cons a as)
  where
    showsHelper (Cons (Const a) Nil) = showsPrec 0 a
    showsHelper (Cons (Const a) as) = foldr (.) id $
        [ showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show a, ShowHelper Field as) =>
    ShowHelper Field (Cons (Pair s a) as)
  where
    showsHelper (Cons a Nil) = showsField a
    showsHelper (Cons a as) = foldr (.) id $
        [ showsField a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show a, ShowHelper Option as) =>
    ShowHelper Option (Cons (Pair s a) as)
  where
    showsHelper (Cons a Nil) = showsOption a id
    showsHelper (Cons a as) = showsOption a (showString ", ") . showsHelper as


------------------------------------------------------------------------------
showsField :: forall a s. Show a => Field (Pair s a) -> ShowS
showsField (Field a) = foldr (.) id $
    [ showString $ symbolVal (Proxy :: Proxy s)
    , showString " = "
    , showsPrec 0 a
    ]


------------------------------------------------------------------------------
showsOption :: forall a s. Show a
    => Option (Pair s a)
    -> ShowS
    -> ShowS
showsOption (Option (Just a)) f = foldr (.) id $
    [ showString $ symbolVal (Proxy :: Proxy s)
    , showString " = "
    , showsPrec 0 a
    , f
    ]
showsOption (Option Nothing) _ = id


------------------------------------------------------------------------------
instance Bounded (Product g Nil) where
    minBound = Nil
    maxBound = Nil


------------------------------------------------------------------------------
instance (Bounded (g a), Bounded (Product g as)) =>
    Bounded (Product g (Cons a as))
  where
    minBound = Cons minBound minBound
    maxBound = Cons maxBound maxBound


------------------------------------------------------------------------------
instance Enum (Product g Nil) where
    fromEnum _ = 0
    toEnum 0 = Nil
    toEnum _ = error "Enum{Data.Product}.toEnum: bad argument"


------------------------------------------------------------------------------
instance
    ( Enum (g a)
    , Eq (g a)
    , Bounded (Product g as)
    , Enum (Product g as)
    , Eq (Product g as)
    )
  =>
    Enum (Product g (Cons a as))
  where
    succ (Cons a as)
        | as == maxBound = Cons (succ a) minBound
        | otherwise = Cons a (succ as)
    pred (Cons a as)
        | as == minBound = Cons (pred a) maxBound
        | otherwise = Cons a (pred as)
    fromEnum (Cons a as) = fromEnum as +
        (fromEnum a * (fromEnum (maxBound :: Product g as) + 1))
    toEnum n = Cons (toEnum a) (toEnum as)
      where
        (a, as) = divMod n (fromEnum (maxBound :: Product g as) + 1)
    enumFrom a = a : enumFrom (succ a)
    enumFromTo a b
        | a == b = [a]
        | otherwise = a : enumFromTo (succ a) b


------------------------------------------------------------------------------
instance (Enum (Product g as), Ord (Product g as)) => Ix (Product g as)
  where
    range = uncurry enumFromTo
    index (a, b) i
        | inRange (a, b) i = fromEnum i - fromEnum a
        | otherwise = error "Ix{Data.Product}.index: error in array index"
    {-# INLINE index #-}
    inRange (a, b) i = i >= a && i <= b


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (Product g Nil) where
    Nil <> Nil = Nil


------------------------------------------------------------------------------
instance (Semigroup (g a), Semigroup (Product g as)) =>
    Semigroup (Product g (Cons a as))
  where
    Cons a as <> Cons b bs = Cons (a <> b) (as <> bs)


#endif
------------------------------------------------------------------------------
instance Monoid (Product g Nil) where
    mempty = Nil
    mappend Nil Nil = Nil


------------------------------------------------------------------------------
instance (Monoid (g a), Monoid (Product g as)) =>
    Monoid (Product g (Cons a as))
  where
    mempty = Cons mempty mempty
    mappend (Cons a as) (Cons b bs) = Cons (mappend a b) (mappend as bs)


------------------------------------------------------------------------------
instance Storable (Product g Nil) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return Nil
    poke _ Nil = return ()


------------------------------------------------------------------------------
instance (Storable (g a), Storable (Product g as)) =>
    Storable (Product g (Cons a as))
  where
    sizeOf _ = roundUpToNearestMultipleOf
        (sizeOf (undefined :: g a))
        (alignment (undefined :: Product g as)) +
            sizeOf (undefined :: Product g as)
    alignment _ = alignment (undefined :: g a)
    peek ptr = do
        a <- peek (castPtr ptr)
        as <- peek (plusPtr ptr
            (roundUpToNearestMultipleOf
                (sizeOf a)
                (alignment (undefined :: Product g as))))
        return $ Cons a as
    poke ptr (Cons a as) = do
        poke (castPtr ptr) a
        poke
            (plusPtr ptr
                (roundUpToNearestMultipleOf (sizeOf a) (alignment as)))
            as


------------------------------------------------------------------------------
roundUpToNearestMultipleOf :: Integral a => a -> a -> a
roundUpToNearestMultipleOf n m = n + m - mod n m


------------------------------------------------------------------------------
instance Num (f a) => Num (Product f (Cons a Nil)) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    negate = lift negate
    abs = lift abs
    signum = lift signum
    fromInteger n = Cons (fromInteger n) Nil


------------------------------------------------------------------------------
instance Real (f a) => Real (Product f (Cons a Nil)) where
    toRational (Cons a Nil) = toRational a
#if __GLASGOW_HASKELL__ < 800
    toRational _ = undefined
#endif


------------------------------------------------------------------------------
instance Integral (f a) => Integral (Product f (Cons a Nil)) where
    quot = lift2 quot
    rem = lift2 rem
    div = lift2 div
    mod = lift2 mod
    quotRem (Cons a Nil) (Cons b Nil) = (Cons a' Nil, Cons b' Nil)
      where
        (a', b') = quotRem a b
#if __GLASGOW_HASKELL__ < 800
    quotRem _ _ = undefined
#endif
    divMod (Cons a Nil) (Cons b Nil) = (Cons a' Nil, Cons b' Nil)
      where
        (a', b') = divMod a b
#if __GLASGOW_HASKELL__ < 800
    divMod _ _ = undefined
#endif
    toInteger (Cons a Nil) = toInteger a
#if __GLASGOW_HASKELL__ < 800
    toInteger _ = undefined
#endif


------------------------------------------------------------------------------
instance Fractional (f a) => Fractional (Product f (Cons a Nil)) where
    (/) = lift2 (/)
    recip = lift recip
    fromRational n = Cons (fromRational n) Nil


------------------------------------------------------------------------------
instance Floating (f a) => Floating (Product f (Cons a Nil)) where
    pi = Cons pi Nil
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
instance RealFrac (f a) => RealFrac (Product f (Cons a Nil)) where
    properFraction (Cons x Nil) = (a, Cons b Nil)
      where
        (a, b) = properFraction x
#if __GLASGOW_HASKELL__ < 800
    properFraction _ = undefined
#endif
    truncate (Cons a Nil) = truncate a
#if __GLASGOW_HASKELL__ < 800
    truncate _ = undefined
#endif
    round (Cons a Nil) = round a
#if __GLASGOW_HASKELL__ < 800
    round _ = undefined
#endif
    ceiling (Cons a Nil) = ceiling a
#if __GLASGOW_HASKELL__ < 800
    ceiling _ = undefined
#endif
    floor (Cons a Nil) = floor a
#if __GLASGOW_HASKELL__ < 800
    floor _ = undefined
#endif


------------------------------------------------------------------------------
instance RealFloat (f a) => RealFloat (Product f (Cons a Nil)) where
    floatRadix (Cons a Nil) = floatRadix a
#if __GLASGOW_HASKELL__ < 800
    floatRadix _ = undefined
#endif
    floatDigits (Cons a Nil) = floatDigits a
#if __GLASGOW_HASKELL__ < 800
    floatDigits _ = undefined
#endif
    floatRange (Cons a Nil) = floatRange a
#if __GLASGOW_HASKELL__ < 800
    floatRange _ = undefined
#endif
    decodeFloat (Cons a Nil) = decodeFloat a
#if __GLASGOW_HASKELL__ < 800
    decodeFloat _ = undefined
#endif
    encodeFloat m n = Cons (encodeFloat m n) Nil
    exponent (Cons a Nil) = exponent a
#if __GLASGOW_HASKELL__ < 800
    exponent _ = undefined
#endif
    significand = lift significand
    scaleFloat n = lift (scaleFloat n)
    isNaN (Cons a Nil) = isNaN a
#if __GLASGOW_HASKELL__ < 800
    isNaN _ = undefined
#endif
    isInfinite (Cons a Nil) = isInfinite a
#if __GLASGOW_HASKELL__ < 800
    isInfinite _ = undefined
#endif
    isDenormalized (Cons a Nil) = isDenormalized a
#if __GLASGOW_HASKELL__ < 800
    isDenormalized _ = undefined
#endif
    isNegativeZero (Cons a Nil) = isNegativeZero a
#if __GLASGOW_HASKELL__ < 800
    isNegativeZero _ = undefined
#endif
    isIEEE (Cons a Nil) = isIEEE a
#if __GLASGOW_HASKELL__ < 800
    isIEEE _ = undefined
#endif
    atan2 = lift2 atan2


------------------------------------------------------------------------------
instance Bits (f a) => Bits (Product f (Cons a Nil)) where
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
    bit i = Cons (bit i) Nil
    setBit a i = lift (flip setBit i) a
    clearBit a i = lift (flip clearBit i) a
    complementBit a i = lift (flip complementBit i) a
    testBit (Cons a Nil) i = testBit a i
#if __GLASGOW_HASKELL__ < 800
    testBit _ _ = undefined
#endif
    isSigned (Cons a Nil) = isSigned a
#if __GLASGOW_HASKELL__ < 800
    isSigned _ = undefined
#endif
    bitSize (Cons a Nil) = bitSize a
#if __GLASGOW_HASKELL__ < 800
    bitSize _ = undefined
#endif
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL a i = lift (flip unsafeShiftL i) a
    unsafeShiftR a i = lift (flip unsafeShiftR i) a
    popCount (Cons a Nil) = popCount a
#if __GLASGOW_HASKELL__ < 800
    popCount _ = undefined
#endif
#endif
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe (Cons a Nil) = bitSizeMaybe a
#if __GLASGOW_HASKELL__ < 800
    bitSizeMaybe _ = undefined
#endif
    zeroBits = Cons zeroBits Nil


------------------------------------------------------------------------------
instance FiniteBits (f a) => FiniteBits (Product f (Cons a Nil)) where
    finiteBitSize (Cons a Nil) = finiteBitSize a
#if __GLASGOW_HASKELL__ < 800
    finiteBitSize _ = undefined
#endif
#if MIN_VERSION_base(4, 8, 0)
    countLeadingZeros (Cons a Nil) = countLeadingZeros a
#if __GLASGOW_HASKELL__ < 800
    countLeadingZeros _ = undefined
#endif
    countTrailingZeros (Cons a Nil) = countTrailingZeros a
#if __GLASGOW_HASKELL__ < 800
    countTrailingZeros _ = undefined
#endif
#endif
#endif


------------------------------------------------------------------------------
instance IsString (f a) => IsString (Product f (Cons a Nil)) where
    fromString s = Cons (fromString s) Nil


------------------------------------------------------------------------------
lift :: (f a -> f b) -> Product f (Cons a Nil) -> Product f (Cons b Nil)
lift f (Cons a Nil) = Cons (f a) Nil
#if __GLASGOW_HASKELL__ < 800
lift _ _ = undefined
#endif
{-# INLINE lift #-}


------------------------------------------------------------------------------
lift2
    :: (f a -> f b -> f c)
    -> Product f (Cons a Nil)
    -> Product f (Cons b Nil)
    -> Product f (Cons c Nil)
lift2 f (Cons a Nil) (Cons b Nil) = Cons (f a b) Nil
#if __GLASGOW_HASKELL__ < 800
lift2 _ _ _ = undefined
#endif
{-# INLINE lift2 #-}


#ifdef GenericDeriving
------------------------------------------------------------------------------
type MetaTupleSel =
    MetaSel Nothing NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
type MetaRecordSel s =
    MetaSel (Just s) NoSourceUnpackedness SourceStrict DecidedStrict


------------------------------------------------------------------------------
type family Selectors (f :: KPoly1 -> *) (as :: KList (KPoly1)) :: * -> *
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Identity Nil = U1
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Identity (Cons a Nil) = S1 MetaTupleSel (Rec0 a)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Identity (Cons a (Cons a' as)) =
        S1 MetaTupleSel (Rec0 a) :*: Selectors Identity (Cons a' as)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors (Const b) Nil = U1
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors (Const b) (Cons a Nil) = S1 MetaTupleSel (Rec0 b)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors (Const b) (Cons a (Cons a' as)) =
        S1 MetaTupleSel (Rec0 b) :*: Selectors (Const b) (Cons a' as)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Field Nil = U1
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Field (Cons (Pair s a) Nil) = S1 (MetaRecordSel s) (Rec0 a)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Field (Cons (Pair s a) (Cons a' as)) =
        S1 (MetaRecordSel s) (Rec0 a) :*: Selectors Field (Cons a' as)
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Option Nil = U1
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Option (Cons (Pair s a) Nil) =
        S1 (MetaRecordSel s) (Rec0 (Maybe a))
#ifndef ClosedTypeFamilies
type instance
#endif
    Selectors Option (Cons (Pair s a) (Cons a' as)) =
        S1 (MetaRecordSel s) (Rec0 (Maybe a)) :*:
            Selectors Option (Cons a' as)
#ifdef ClosedTypeFamilies
    Selectors f Nil = U1
    Selectors f (Cons (Pair s a) Nil) =
        S1 (MetaRecordSel s) (Rec0 (f (Pair s a)))
    Selectors f (Cons (Pair s a) (Cons a' as)) =
        S1 (MetaRecordSel s) (Rec0 (f (Pair s a))) :*: Selectors f (Cons a' as)
    Selectors f (Cons a Nil) = S1 MetaTupleSel (Rec0 (f a))
    Selectors f (Cons a (Cons a' as)) =
        S1 MetaTupleSel (Rec0 (f a)) :*: Selectors f (Cons a' as)
#endif


------------------------------------------------------------------------------
type MetaTupleCons = MetaCons S.Empty_ PrefixI False


------------------------------------------------------------------------------
type MetaRecordCons = MetaCons S.Empty_ PrefixI True


------------------------------------------------------------------------------
type family Constructors (f :: KPoly1 -> *) :: (* -> *) -> * -> *
#ifdef ClosedTypeFamilies
  where
#endif
#ifndef ClosedTypeFamilies
type instance
#endif
    Constructors Identity = C1 MetaTupleCons
#ifndef ClosedTypeFamilies
type instance
#endif
    Constructors (Const b) = C1 MetaTupleCons
#ifndef ClosedTypeFamilies
type instance
#endif
    Constructors Field = C1 MetaRecordCons
#ifndef ClosedTypeFamilies
type instance
#endif
    Constructors Option = C1 MetaRecordCons
#ifdef ClosedTypeFamilies
    Constructors (f :: KPair (KString, KPoly2) -> *) = C1 MetaRecordCons
    Constructors f = C1 MetaTupleCons
#endif


------------------------------------------------------------------------------
type MetaProductData =
    MetaData S.Product S.DataAnonymousProduct S.AnonymousData False


------------------------------------------------------------------------------
type Data f as = D1 MetaProductData (Constructors f (Selectors f as))


------------------------------------------------------------------------------
class SGeneric f as where
    sto :: Selectors f as x -> Product f as
    sfrom :: Product f as -> Selectors f as x


------------------------------------------------------------------------------
instance SGeneric Identity Nil where
    sto U1 = Nil
    sfrom Nil = U1


------------------------------------------------------------------------------
instance SGeneric Identity (Cons a Nil) where
    sto (M1 (K1 a)) = Cons (Identity a) Nil
    sfrom (Cons (Identity a) _) = M1 (K1 a)


------------------------------------------------------------------------------
instance (SGeneric Identity (Cons a' as)) =>
    SGeneric Identity (Cons a (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons (Identity a) (sto as)
    sfrom (Cons (Identity a) as) = M1 (K1 a) :*: sfrom as


------------------------------------------------------------------------------
instance SGeneric (Const b) Nil where
    sto U1 = Nil
    sfrom Nil = U1


------------------------------------------------------------------------------
instance SGeneric (Const b) (Cons a Nil) where
    sto (M1 (K1 a)) = Cons (Const a) Nil
    sfrom (Cons (Const a) _) = M1 (K1 a)


------------------------------------------------------------------------------
instance (SGeneric (Const b) (Cons a' as)) =>
    SGeneric (Const b) (Cons a (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons (Const a) (sto as)
    sfrom (Cons (Const a) as) = M1 (K1 a) :*: sfrom as


------------------------------------------------------------------------------
instance SGeneric Field Nil where
    sto U1 = Nil
    sfrom Nil = U1


------------------------------------------------------------------------------
instance KnownSymbol s => SGeneric Field (Cons (Pair s a) Nil) where
    sto (M1 (K1 a)) = Cons (Field a) Nil
    sfrom (Cons (Field a) _) = M1 (K1 a)


------------------------------------------------------------------------------
instance (KnownSymbol s, SGeneric Field (Cons a' as)) =>
    SGeneric Field (Cons (Pair s a) (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons (Field a) (sto as)
    sfrom (Cons (Field a) as) = M1 (K1 a) :*: sfrom as


------------------------------------------------------------------------------
instance SGeneric Option Nil where
    sto U1 = Nil
    sfrom Nil = U1


------------------------------------------------------------------------------
instance KnownSymbol s => SGeneric Option (Cons (Pair s a) Nil) where
    sto (M1 (K1 a)) = Cons (Option a) Nil
    sfrom (Cons (Option a) _) = M1 (K1 a)


------------------------------------------------------------------------------
instance (KnownSymbol s, SGeneric Option (Cons a' as)) =>
    SGeneric Option (Cons (Pair s a) (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons (Option a) (sto as)
    sfrom (Cons (Option a) as) = M1 (K1 a) :*: sfrom as


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ SGeneric f Nil where
    sto U1 = Nil
    sfrom Nil = U1


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Selectors f (Cons a Nil) ~ S1 MetaTupleSel (Rec0 (f a))
    )
  =>
    SGeneric f (Cons a Nil)
  where
    sto (M1 (K1 a)) = Cons a Nil
    sfrom (Cons a _) = M1 (K1 a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( SGeneric f (Cons a' as)
    , Selectors f (Cons a (Cons a' as))
        ~ (S1 MetaTupleSel (Rec0 (f a)) :*: Selectors f (Cons a' as))
    )
  =>
    SGeneric f (Cons a (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons a (sto as)
    sfrom (Cons a as) = M1 (K1 a) :*: sfrom as


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Selectors f (Cons (Pair s a) Nil) ~
        S1 (MetaRecordSel s) (Rec0 (f (Pair s a)))
    )
  =>
    SGeneric (f :: KPair (KString, KPoly1) -> *) (Cons (Pair s a) Nil)
  where
    sto (M1 (K1 a)) = Cons a Nil
    sfrom (Cons a _) = M1 (K1 a)


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( SGeneric f (Cons a' as)
    , Selectors f (Cons (Pair s a) (Cons a' as)) ~
        (S1 (MetaRecordSel s) (Rec0 (f (Pair s a))) :*:
            Selectors f (Cons a' as))
    )
  =>
    SGeneric
        (f :: KPair (KString, KPoly1) -> *)
        (Cons (Pair s a) (Cons a' as))
  where
    sto (M1 (K1 a) :*: as) = Cons a (sto as)
    sfrom (Cons a as) = M1 (K1 a) :*: sfrom as


#endif
------------------------------------------------------------------------------
class SGeneric f as => CGeneric f as where
    cto :: Constructors f (Selectors f as) x -> Product f as
    cfrom :: Product f as -> Constructors f (Selectors f as) x


------------------------------------------------------------------------------
instance SGeneric Identity as => CGeneric Identity as where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


------------------------------------------------------------------------------
instance SGeneric (Const b) as => CGeneric (Const b) as where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


------------------------------------------------------------------------------
instance SGeneric Field as => CGeneric Field as where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


------------------------------------------------------------------------------
instance SGeneric Option as => CGeneric Option as
  where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( SGeneric f as
    , Constructors f ~ C1 MetaTupleCons
    )
  =>
    CGeneric f as
  where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( SGeneric f as
    , Constructors f ~ C1 MetaRecordCons
    )
  =>
    CGeneric (f :: KPair (KString, KPoly1) -> *) as
  where
    cto (M1 a) = sto a
    cfrom a = M1 (sfrom a)


#endif
------------------------------------------------------------------------------
instance CGeneric f as => Generic (Product f as) where
    type Rep (Product f as) = Data f as
    to (M1 a) = cto a
    from = M1 . cfrom


#endif
------------------------------------------------------------------------------
instance NFData (Product g Nil) where
    rnf Nil = ()


------------------------------------------------------------------------------
instance (NFData (g a), NFData (Product g as)) =>
    NFData (Product g (Cons a as))
  where
    rnf (Cons a as) = rnf a `seq` rnf as


------------------------------------------------------------------------------
type Record = Product Field


------------------------------------------------------------------------------
type Tuple = Product Identity


------------------------------------------------------------------------------
type Options = Product Option


------------------------------------------------------------------------------
(<::>) :: f a -> Product f as -> Product f (Cons a as)
(<::>) = Cons
infixr 5 <::>


------------------------------------------------------------------------------
(<:>) :: a -> Tuple as -> Tuple (Cons a as)
(<:>) = Cons . Identity
infixr 5 <:>


------------------------------------------------------------------------------
(<:.>) :: forall s a as. KnownSymbol s
    => a
    -> Record as
    -> Record (Cons (Pair s a) as)
(<:.>) = Cons . Field
infixr 5 <:.>


------------------------------------------------------------------------------
(<:?>) :: forall s a as. KnownSymbol s
    => Maybe a
    -> Options as
    -> Options (Cons (Pair s a) as)
(<:?>) = Cons . Option
infixr 5 <:?>


#ifdef LanguagePatternSynonyms
------------------------------------------------------------------------------
pattern (:<::>) :: f a -> Product f as -> Product f (Cons a as)
pattern (:<::>) a as = Cons a as
infixr 5 :<::>


------------------------------------------------------------------------------
pattern (:<:>) :: a -> Tuple as -> Tuple (Cons a as)
pattern (:<:>) a as = Cons (Identity a) as
infixr 5 :<:>


------------------------------------------------------------------------------
pattern (:<:.>) :: forall s a as. KnownSymbol s
    => a
    -> Record as
    -> Record (Cons (Pair s a) as)
pattern (:<:.>) a as = Cons (Field a) as
infixr 5 :<:.>


------------------------------------------------------------------------------
pattern (:<:?>) :: forall s a as. KnownSymbol s
    => Maybe a
    -> Options as
    -> Options (Cons (Pair s a) as)
pattern (:<:?>) a as = Cons (Option a) as
infixr 5 :<:?>


#endif
------------------------------------------------------------------------------
class (as :: KList (KPoly1)) :<++> (bs :: KList (KPoly1)) where
    (<++>) :: Product f as -> Product f bs -> Product f (as :<> bs)
infixr 5 :<++>
infixr 5 <++>


------------------------------------------------------------------------------
instance (bs ~ (Nil :<> bs)) => (:<++>) Nil bs where
    Nil <++> bs = bs


------------------------------------------------------------------------------
instance (as :<++> bs, (Cons a as :<> bs) ~ (Cons a (as :<> bs))) =>
    (:<++>) (Cons a as) bs
  where
    Cons a as <++> bs = Cons a (as <++> bs)


------------------------------------------------------------------------------
fromOptions :: Options as -> Record as -> Record as
fromOptions Nil Nil = Nil
#if __GLASGOW_HASKELL__ >= 700
fromOptions (Cons (Option a) as) (Cons b bs) =
    Cons (maybe b Field a) (fromOptions as bs)
#else
fromOptions (Cons (Option a) as) (Cons (Field b) bs) =
    Cons (maybe (Field b) Field a) (fromOptions as bs)
#endif
#if __GLASGOW_HASKELL__ < 800
fromOptions _ _ = undefined
#endif


------------------------------------------------------------------------------
fromOptionsNoDefaults :: Options as -> Record as
fromOptionsNoDefaults Nil = Nil
fromOptionsNoDefaults (Cons o@(Option (Just a)) as) =
    Cons (fieldFromOption o a) (fromOptionsNoDefaults as)
  where
    fieldFromOption :: forall s a. Option (Pair s a) -> a -> Field (Pair s a)
    fieldFromOption (Option _) b = Field b
fromOptionsNoDefaults (Cons o@(Option Nothing) _) =
    error $ "Cannot get record from options: option "
        ++ symbol o
        ++ " is missing!"
  where
    symbol :: forall s a. Option (Pair s a) -> String
    symbol (Option _) = symbolVal (Proxy :: Proxy s)


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    a ~ T.LookupIndex n as
  =>
#endif
    LookupIndex'
        (n :: KNatural)
        (as :: KList (KPoly1))
        (a :: KPoly1)
    | n as -> a
  where
    lookupIndex' :: proxy n -> Product f as -> f a


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupIndex n as = LookupIndex' n as (T.LookupIndex n as)


#endif
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 700
instance __OVERLAPPING__ a ~ b => LookupIndex' Zero (Cons b as) a where
#else
instance a ~ b => LookupIndex' (Natural Nil) (Cons b as) a where
#endif
    lookupIndex' _ (Cons a _) = a


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 700
instance __OVERLAPPABLE__
    ( LookupIndex' (n :- One) as a
#ifdef ClosedTypeFamilies
    , a ~ T.LookupIndex n (Cons b as)
#endif
    )
  =>
    LookupIndex' n (Cons b as) a
  where
    lookupIndex' _ (Cons _ as) = lookupIndex' (Proxy :: Proxy (n :- One)) as
#else
instance LookupIndex' (Natural (Cons n ns) :- One) as a =>
    LookupIndex' (Natural (Cons n ns)) (Cons b as) a
  where
    lookupIndex' _ (Cons _ as) =
        lookupIndex' (Proxy :: Proxy (Natural (Cons n ns) :- One)) as
#endif


------------------------------------------------------------------------------
lookupIndex :: forall n as a f. LookupIndex' n as a => Product f as -> f a
lookupIndex = lookupIndex' (Proxy :: Proxy n)
{-# INLINE lookupIndex #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateIndex n b as
  =>
#endif
    UpdateIndex'
        (n :: KNatural)
        (b :: KPoly1)
        (as :: KList (KPoly1))
        (bs :: KList (KPoly1))
    | n as b -> bs
  where
    updateIndex' :: proxy n -> f b -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateIndex n b as = UpdateIndex' n b as (T.UpdateIndex n b as)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ (t ~ b, as ~ bs) =>
    UpdateIndex' Zero t (Cons a as) (Cons b bs)
  where
    updateIndex' _ b (Cons _ as) = Cons b as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( a ~ b
    , UpdateIndex' (n :- One) t as bs
#ifdef ClosedTypeFamilies
    , Cons b bs ~ T.UpdateIndex n t (Cons b as)
#endif
    )
  =>
    UpdateIndex' n t (Cons a as) (Cons b bs)
  where
    updateIndex' _ b (Cons a as) =
        Cons a (updateIndex' (Proxy :: Proxy (n :- One)) b as)


------------------------------------------------------------------------------
updateIndex :: forall n b as bs f. UpdateIndex' n b as bs
    => f b
    -> Product f as
    -> Product f bs
updateIndex = updateIndex' (Proxy :: Proxy n)
{-# INLINE updateIndex #-}


------------------------------------------------------------------------------
index' :: (LookupIndex' n as a, UpdateIndex' n b as bs, Functor f)
    => proxy' n
    -> (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
index' p = lens (lookupIndex' p) (updateIndex' p)
{-# INLINE index' #-}


------------------------------------------------------------------------------
index
    :: forall n as a b bs g f.
        ( LookupIndex' n as a
        , UpdateIndex' n b as bs, Functor f
        )
    => (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
index = index' (Proxy :: Proxy n)
{-# INLINE index #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    n ~ T.LookupElement n as
  =>
#endif
    LookupElement'
        (n :: KPoly1)
        (as :: KList (KPoly1))
  where
    lookupElement' :: proxy n -> Product f as -> f n


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupElement n as = LookupElement' n as


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ LookupElement' n (Cons n as) where
    lookupElement' _ (Cons a _) = a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( LookupElement' n as
#ifdef ClosedTypeFamilies
    , n ~ T.LookupElement n (Cons b as)
#endif
    )
  =>
    LookupElement' n (Cons b as)
  where
    lookupElement' p (Cons _ as) = lookupElement' p as


------------------------------------------------------------------------------
lookupElement :: forall n as f. LookupElement' n as => Product f as -> f n
lookupElement = lookupElement' (Proxy :: Proxy n)
{-# INLINE lookupElement #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    as ~ T.UpdateElement n as
  =>
#endif
    UpdateElement' 
        (n :: KPoly1)
        (as :: KList (KPoly1))
  where
    updateElement' :: f n -> Product f as -> Product f as


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateElement n as = UpdateElement' n as


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ UpdateElement' n (Cons n as) where
    updateElement' b (Cons _ as) = Cons b as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( UpdateElement' n as
#ifdef ClosedTypeFamilies
    , Cons b as ~ T.UpdateElement n (Cons b as)
#endif
    )
  =>
    UpdateElement' n (Cons b as)
  where
    updateElement' b (Cons a as) = Cons a (updateElement' b as)


------------------------------------------------------------------------------
updateElement :: forall n as f. UpdateElement' n as
    => f n
    -> Product f as
    -> Product f as
updateElement = updateElement'
{-# INLINE updateElement #-}


------------------------------------------------------------------------------
element' :: (LookupElement' n as, UpdateElement' n as, Functor f)
    => proxy n
    -> (g n -> f (g n))
    -> Product g as
    -> f (Product g as)
element' p = lens (lookupElement' p) updateElement'
{-# INLINE element' #-}


------------------------------------------------------------------------------
element
    :: forall n as g f.
        ( LookupElement' n as
        , UpdateElement' n as
        , Functor f
        )
    => (g n -> f (g n))
    -> Product g as
    -> f (Product g as)
element = element' (Proxy :: Proxy n)
{-# INLINE element #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    a ~ T.LookupKey n as
  =>
#endif
    LookupKey'
        (n :: KPoly1)
        (as :: KList (KPair (KPoly1, KPoly2)))
        (a :: KPoly2)
    | n as -> a
  where
    lookupKey' :: proxy n -> Product f as -> f (Pair n a)


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupKey n as = LookupKey' n as (T.LookupKey n as)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ LookupKey' n (Cons (Pair n a) as) a where
    lookupKey' _ (Cons a _) = a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( LookupKey' n as a
#ifdef ClosedTypeFamilies
    , a ~ T.LookupKey n (Cons b as)
#endif
    )
  =>
    LookupKey' n (Cons b as) a
  where
    lookupKey' p (Cons _ as) = lookupKey' p as


------------------------------------------------------------------------------
lookupKey :: forall n as a f. LookupKey' n as a => Product f as -> f (Pair n a)
lookupKey = lookupKey' (Proxy :: Proxy n)
{-# INLINE lookupKey #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateKey n b as
  =>
#endif
    UpdateKey'
        (n :: KPoly1)
        (b :: KPoly2)
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1, KPoly2)))
    | n b as -> bs
  where
    updateKey' :: f (Pair n b) -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateKey n b as = UpdateKey' n b as (T.UpdateKey n b as)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ as ~ bs =>
    UpdateKey' n b (Cons (Pair n a) as) (Cons (Pair n b) bs)
  where
    updateKey' b (Cons _ as) = Cons b as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( UpdateKey' n c as bs
    , a ~ b
#ifdef ClosedTypeFamilies
    , Cons b bs ~ T.UpdateKey n c (Cons b as)
#endif
    )
  =>
    UpdateKey' n c (Cons a as) (Cons b bs)
  where
    updateKey' b (Cons a as) = Cons a (updateKey' b as)


------------------------------------------------------------------------------
updateKey :: forall n b as bs f. UpdateKey' n b as bs
    => f (Pair n b)
    -> Product f as
    -> Product f bs
updateKey = updateKey'
{-# INLINE updateKey #-}


------------------------------------------------------------------------------
key' :: (LookupKey' n as a, UpdateKey' n b as bs, Functor f)
    => proxy n
    -> (g (Pair n a) -> f (g (Pair n b)))
    -> Product g as
    -> f (Product g bs)
key' p = lens (lookupKey' p) updateKey'
{-# INLINE key' #-}


------------------------------------------------------------------------------
key
    :: forall n as a b bs g f.
        ( LookupKey' n as a
        , UpdateKey' n b as bs
        , Functor f
        )
    => (g (Pair n a) -> f (g (Pair n b)))
    -> Product g as
    -> f (Product g bs)
key = key' (Proxy :: Proxy n)
{-# INLINE key #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    a ~ T.LookupValue n as
  =>
#endif
    LookupValue'
        (n :: KPoly2)
        (as :: KList (KPair (KPoly1, KPoly2)))
        (a :: KPoly1)
    | n as -> a
  where
    lookupValue' :: proxy n -> Product f as -> f (Pair a n)


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupValue n as = LookupValue' n as (T.LookupValue n as)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ LookupValue' n (Cons (Pair a n) as) a where
    lookupValue' _ (Cons a _) = a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( LookupValue' n as a
#ifdef ClosedTypeFamilies
    , a ~ T.LookupValue n (Cons b as)
#endif
    )
  =>
    LookupValue' n (Cons b as) a
  where
    lookupValue' p (Cons _ as) = lookupValue' p as


------------------------------------------------------------------------------
lookupValue :: forall n as a f. LookupValue' n as a
    => Product f as
    -> f (Pair a n)
lookupValue = lookupValue' (Proxy :: Proxy n)
{-# INLINE lookupValue #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateValue n b as
  =>
#endif
    UpdateValue'
        (n :: KPoly2)
        (b :: KPoly1)
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1    , KPoly2)))
    | n b as -> bs
  where
    updateValue' :: f (Pair b n) -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateValue n b as = UpdateValue' n b as (T.UpdateValue n b as)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPING__ as ~ bs =>
    UpdateValue' n b (Cons (Pair a n) as) (Cons (Pair b n) bs)
  where
    updateValue' b (Cons _ as) = Cons b as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( UpdateValue' n c as bs
    , a ~ b
#ifdef ClosedTypeFamilies
    , Cons b bs ~ T.UpdateValue n c (Cons b as)
#endif
    )
  =>
    UpdateValue' n c (Cons a as) (Cons b bs)
  where
    updateValue' b (Cons a as) = Cons a (updateValue' b as)


------------------------------------------------------------------------------
updateValue :: forall n b as bs f. UpdateValue' n b as bs
    => f (Pair b n)
    -> Product f as
    -> Product f bs
updateValue = updateValue'
{-# INLINE updateValue #-}


------------------------------------------------------------------------------
value' :: (LookupValue' n as a, UpdateValue' n b as bs, Functor f)
    => proxy n
    -> (g (Pair a n) -> f (g (Pair b n)))
    -> Product g as
    -> f (Product g bs)
value' p = lens (lookupValue' p) updateValue'
{-# INLINE value' #-}


------------------------------------------------------------------------------
value
    :: forall n as a b bs g f.
        ( LookupValue' n as a
        , UpdateValue' n b as bs
        , Functor f
        )
    => (g (Pair a n) -> f (g (Pair b n)))
    -> Product g as
    -> f (Product g bs)
value = value' (Proxy :: Proxy n)
{-# INLINE value #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.LookupIndices ns as
  =>
#endif
    LookupIndices'
        (ns :: KList (KNatural))
        (as :: KList (KPoly1))
        (bs :: KList (KPoly1))
    | ns as -> bs
  where
    lookupIndices' :: proxy ns -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupIndices ns as = LookupIndices' ns as (T.LookupIndices ns as)


#endif
------------------------------------------------------------------------------
instance LookupIndices' Nil as Nil where
    lookupIndices' _ _ = Nil


------------------------------------------------------------------------------
instance (LookupIndex' n as b, LookupIndices' ns as bs) =>
    LookupIndices' (Cons n ns) as (Cons b bs)
  where
    lookupIndices' _ as = Cons
        (lookupIndex' (Proxy :: Proxy n) as)
        (lookupIndices' (Proxy :: Proxy ns) as)


------------------------------------------------------------------------------
lookupIndices :: forall ns as bs f. LookupIndices' ns as bs
    => Product f as
    -> Product f bs
lookupIndices = lookupIndices' (Proxy :: Proxy ns)
{-# INLINE lookupIndices #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateIndices ns cs as
  =>
#endif
    UpdateIndices'
        (ns :: KList (KNatural))
        (cs :: KList (KPoly1))
        (as :: KList (KPoly1))
        (bs :: KList (KPoly1))
    | ns cs as -> bs
  where
    updateIndices' :: proxy ns -> Product f cs -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateIndices ns cs as =
    UpdateIndices' ns cs as (T.UpdateIndices ns cs as)


#endif
------------------------------------------------------------------------------
instance as ~ bs => UpdateIndices' Nil Nil as bs where
    updateIndices' _ Nil as = as


------------------------------------------------------------------------------
instance (UpdateIndex' n c as bs', UpdateIndices' ns cs bs' bs) =>
    UpdateIndices' (Cons n ns) (Cons c cs) as bs
  where
    updateIndices' _ (Cons c cs) as = updateIndices'
        (Proxy :: Proxy ns)
        cs
        (updateIndex' (Proxy :: Proxy n) c as)


------------------------------------------------------------------------------
updateIndices
    :: forall ns cs as bs f. UpdateIndices' ns cs as bs
    => Product f cs
    -> Product f as
    -> Product f bs
updateIndices = updateIndices' (Proxy :: Proxy ns)
{-# INLINE updateIndices #-}


------------------------------------------------------------------------------
indices' :: (LookupIndices' ns ss as, UpdateIndices' ns bs ss ts, Functor f)
    => proxy ns
    -> (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
indices' p = lens (lookupIndices' p) (updateIndices' p)
{-# INLINE indices' #-}


------------------------------------------------------------------------------
indices
    :: forall ns ss as bs ts g f.
        ( LookupIndices' ns ss as
        , UpdateIndices' ns bs ss ts
        , Functor f
        )
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
indices = indices' (Proxy :: Proxy ns)
{-# INLINE indices #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    ns ~ T.LookupElements ns as
  =>
#endif
    LookupElements' 
        (ns :: KList (KPoly1))
        (as :: KList (KPoly1))
  where
    lookupElements' :: proxy ns -> Product f as -> Product f ns


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupElements n as = LookupElements' n as


#endif
-----------------------------------------------------------------------------
instance LookupElements' Nil as where
    lookupElements' _ _ = Nil


------------------------------------------------------------------------------
instance (LookupElement' n as, LookupElements' ns as) =>
    LookupElements' (Cons n ns) as
  where
    lookupElements' _ as = Cons
        (lookupElement' (Proxy :: Proxy n) as)
        (lookupElements' (Proxy :: Proxy ns) as)


------------------------------------------------------------------------------
lookupElements :: forall ns as f. LookupElements' ns as
    => Product f as
    -> Product f ns
lookupElements = lookupElements' (Proxy :: Proxy ns)
{-# INLINE lookupElements #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    as ~ T.UpdateElements ns as
  =>
#endif
    UpdateElements'
        (ns :: KList (KPoly1))
        (as :: KList (KPoly1))
  where
    updateElements' :: Product f ns -> Product f as -> Product f as


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateElements n as = UpdateElements' n as


#endif
------------------------------------------------------------------------------
instance UpdateElements' Nil as where
    updateElements' _ as = as


------------------------------------------------------------------------------
instance (UpdateElement' n as, UpdateElements' ns as) =>
    UpdateElements' (Cons n ns) as
  where
    updateElements' (Cons n ns) as = updateElements' ns (updateElement' n as)


------------------------------------------------------------------------------
updateElements :: forall ns as f. UpdateElements' ns as
    => Product f ns
    -> Product f as
    -> Product f as
updateElements = updateElements'
{-# INLINE updateElements #-}


------------------------------------------------------------------------------
elements' :: (LookupElements' ns as, UpdateElements' ns as, Functor f)
    => proxy ns
    -> (Product g ns -> f (Product g ns))
    -> Product g as
    -> f (Product g as)
elements' p = lens (lookupElements' p) updateElements'
{-# INLINE elements' #-}


------------------------------------------------------------------------------
elements
    :: forall ns as g f.
        ( LookupElements' ns as
        , UpdateElements' ns as
        , Functor f
        )
    => (Product g ns -> f (Product g ns))
    -> Product g as
    -> f (Product g as)
elements = elements' (Proxy :: Proxy ns)
{-# INLINE elements #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.LookupKeys ns as
  =>
#endif
    LookupKeys'
        (ns :: KList (KPoly1))
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1, KPoly2)))
    | ns as -> bs
  where
    lookupKeys' :: proxy ns -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupKeys ns as = LookupKeys' ns as (T.LookupKeys ns as)


#endif
------------------------------------------------------------------------------
instance LookupKeys' Nil as Nil where
    lookupKeys' _ _ = Nil


------------------------------------------------------------------------------
instance (LookupKey' n as b, LookupKeys' ns as bs) =>
    LookupKeys' (Cons n ns) as (Cons (Pair n b) bs)
  where
    lookupKeys' _ as = Cons
        (lookupKey' (Proxy :: Proxy n) as)
        (lookupKeys' (Proxy :: Proxy ns) as)


------------------------------------------------------------------------------
lookupKeys :: forall ns as bs f. LookupKeys' ns as bs
    => Product f as
    -> Product f bs
lookupKeys = lookupKeys' (Proxy :: Proxy ns)
{-# INLINE lookupKeys #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateKeys ns as
  =>
#endif
    UpdateKeys'
        (ns :: KList (KPair (KPoly1, KPoly2)))
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1, KPoly2)))
    | ns as -> bs
  where
    updateKeys' :: Product f ns -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateKeys ns as = UpdateKeys' ns as (T.UpdateKeys ns as)


#endif
------------------------------------------------------------------------------
instance as ~ bs => UpdateKeys' Nil as bs where
    updateKeys' Nil as = as


------------------------------------------------------------------------------
instance (UpdateKey' n b as bs', UpdateKeys' ns bs' bs) =>
    UpdateKeys' (Cons (Pair n b) ns) as bs
  where
    updateKeys' (Cons n ns) as = updateKeys' ns (updateKey' n as)


------------------------------------------------------------------------------
updateKeys :: forall ns as bs f. UpdateKeys' ns as bs
    => Product f ns
    -> Product f as
    -> Product f bs
updateKeys = updateKeys'
{-# INLINE updateKeys #-}


------------------------------------------------------------------------------
keys' :: (LookupKeys' ns ss as, UpdateKeys' bs ss ts, Functor f)
    => proxy ns
    -> (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
keys' p = lens (lookupKeys' p) updateKeys'
{-# INLINE keys' #-}


------------------------------------------------------------------------------
keys
    :: forall ns ss as bs ts g f.
        ( LookupKeys' ns ss as
        , UpdateKeys' bs ss ts
        , Functor f
        )
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
keys = keys' (Proxy :: Proxy ns)
{-# INLINE keys #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.LookupValues ns as
  =>
#endif
    LookupValues'
        (ns :: KList (KPoly2))
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1, KPoly2)))
    | ns as -> bs
  where
    lookupValues' :: proxy ns -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type LookupValues ns as = LookupValues' ns as (T.LookupValues ns as)


#endif
------------------------------------------------------------------------------
instance LookupValues' Nil as Nil where
    lookupValues' _ _ = Nil


------------------------------------------------------------------------------
instance (LookupValue' n as b, LookupValues' ns as bs) =>
    LookupValues' (Cons n ns) as (Cons (Pair b n) bs)
  where
    lookupValues' _ as = Cons
        (lookupValue' (Proxy :: Proxy n) as)
        (lookupValues' (Proxy :: Proxy ns) as)


------------------------------------------------------------------------------
lookupValues :: forall ns as bs f. LookupValues' ns as bs
    => Product f as
    -> Product f bs
lookupValues = lookupValues' (Proxy :: Proxy ns)
{-# INLINE lookupValues #-}


------------------------------------------------------------------------------
class
#ifdef ClosedTypeFamilies
    bs ~ T.UpdateValues ns as
  =>
#endif
    UpdateValues'
        (ns :: KList (KPair (KPoly1, KPoly2)))
        (as :: KList (KPair (KPoly1, KPoly2)))
        (bs :: KList (KPair (KPoly1, KPoly2)))
    | ns as -> bs
  where
    updateValues' :: Product f ns -> Product f as -> Product f bs


#ifdef ClosedTypeFamilies
------------------------------------------------------------------------------
type UpdateValues ns as = UpdateValues' ns as (T.UpdateValues ns as)


#endif
------------------------------------------------------------------------------
instance as ~ bs => UpdateValues' Nil as bs where
    updateValues' Nil as = as


------------------------------------------------------------------------------
instance (UpdateValue' n b as bs', UpdateValues' ns bs' bs) =>
    UpdateValues' (Cons (Pair b n) ns) as bs
  where
    updateValues' (Cons n ns) as = updateValues' ns (updateValue' n as)


------------------------------------------------------------------------------
updateValues :: forall ns as bs f. UpdateValues' ns as bs
    => Product f ns
    -> Product f as
    -> Product f bs
updateValues = updateValues'
{-# INLINE updateValues #-}


------------------------------------------------------------------------------
values' :: (LookupValues' ns ss as, UpdateValues' bs ss ts, Functor f)
    => proxy ns
    -> (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
values' p = lens (lookupValues' p) updateValues'
{-# INLINE values' #-}


------------------------------------------------------------------------------
values
    :: forall ns ss as bs ts g f.
        ( LookupValues' ns ss as
        , UpdateValues' bs ss ts
        , Functor f
        )
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
values = values' (Proxy :: Proxy ns)
{-# INLINE values #-}


------------------------------------------------------------------------------
lens :: Functor f => (s -> a) -> (b -> s -> t) -> (a -> f b) -> s -> f t
lens get set' f s = fmap (flip set' s) (f (get s))
{-# INLINE lens #-}
