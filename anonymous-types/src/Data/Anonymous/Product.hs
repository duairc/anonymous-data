{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"
#include "incoherent.h"

module Data.Anonymous.Product
    ( Product (Cons, Nil)
    , Field
#if __GLASGOW_HASKELL__ >= 710
    , pattern Field
#endif
    , field
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Control.Monad (msum)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Ix (Ix, inRange, range)
import qualified Data.Ix as I (index)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if __GLASGOW_HASKELL__ >= 800
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.Typeable (Typeable)
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
import           GHC.Generics
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
#if __GLASGOW_HASKELL__ >= 800
                     , FixityI (PrefixI)
                     , Meta (MetaCons, MetaData, MetaSel)
                     , SourceUnpackedness (NoSourceUnpackedness)
                     , SourceStrictness (NoSourceStrictness)
                     , DecidedStrictness (DecidedLazy)
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
import           GHC.TypeLits (KnownSymbol)


-- types ---------------------------------------------------------------------
import qualified Data.Field as F (Field (Field))
import           Data.Uncurry (Uncurry (Uncurry))


------------------------------------------------------------------------------
data Product (f :: u -> *) (as :: [u]) where
    Nil :: Product f '[]
    Cons :: f a -> Product f as -> Product f (a ': as)
  deriving (Typeable)


------------------------------------------------------------------------------
instance Eq (Product g '[]) where
    Nil == Nil = True


------------------------------------------------------------------------------
instance (Eq (g a), Eq (Product g as)) => Eq (Product g (a ': as)) where
    Cons a as == Cons b bs = a == b && as == bs


------------------------------------------------------------------------------
instance Ord (Product g '[]) where
    compare Nil Nil = EQ


------------------------------------------------------------------------------
instance (Ord (g a), Ord (Product g as)) => Ord (Product g (a ': as)) where
    compare (Cons a as) (Cons b bs) = mappend (compare a b) (compare as bs)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ PlainRead g as => Read (Product g as) where
    readsPrec _ = plainReads


------------------------------------------------------------------------------
instance (ReadHelper Identity as, PlainRead Identity as) =>
    Read (Product Identity as)
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
instance (ReadHelper Field as, PlainRead Field as) => Read (Product Field as)
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
instance PlainRead g '[] where
    plainReads s = do
        ("<>", s') <- lex s
        return (Nil, s')


------------------------------------------------------------------------------
instance PlainReadHelper g (a ': as) => PlainRead g (a ': as) where
    plainReads s = do
        ("<", s') <- lex s
        (as, s'') <- plainReadsHelper s'
        (">", s''') <- lex s''
        return (as, s''')


------------------------------------------------------------------------------
class PlainReadHelper g as where
    plainReadsHelper :: ReadS (Product g as)


------------------------------------------------------------------------------
instance PlainReadHelper g '[] where
    plainReadsHelper s = return (Nil, s)


------------------------------------------------------------------------------
instance Read (g a) => PlainReadHelper g (a ': '[]) where
    plainReadsHelper s = do
        (a, s') <- reads s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read (g a), PlainReadHelper g as) =>
    PlainReadHelper g (a ': as)
  where
    plainReadsHelper s = do
        (a, s') <- reads s
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
instance Read a => ReadHelper Identity (a ': '[]) where
    readsHelper s = do
        (a, s') <- reads s
        return (Cons (Identity a) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read a, ReadHelper Identity as) =>
    ReadHelper Identity (a ': as)
  where
    readsHelper s = do
        (a, s') <- reads s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Identity a) as, s''')


------------------------------------------------------------------------------
instance Read b => ReadHelper (Const b) (a ': '[]) where
    readsHelper s = do
        (b, s') <- reads s
        return (Cons (Const b) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read b, ReadHelper (Const b) as) =>
    ReadHelper (Const b) (a ': as)
  where
    readsHelper s = do
        (b, s') <- reads s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Const b) as, s''')


------------------------------------------------------------------------------
instance __INCOHERENT__ ShowHelper g as => Show (Product g as) where
    showsPrec _ as = foldr (.) id $
        [ showString "<"
        , showsHelper as
        , showString ">"
        ]


------------------------------------------------------------------------------
instance ShowHelper Identity as => Show (Product Identity as) where
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
instance ShowHelper Field as => Show (Product Field as) where
    showsPrec _ as = foldr (.) id $
        [ showString "{"
        , showsHelper as
        , showString "}"
        ]


------------------------------------------------------------------------------
class ShowHelper g as where
    showsHelper :: Product g as -> ShowS


------------------------------------------------------------------------------
instance ShowHelper g '[] where
    showsHelper Nil = id


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    (Show (g a), ShowHelper g as)
  =>
    ShowHelper g (a ': as)
  where
    showsHelper (Cons a Nil) = shows a
    showsHelper (Cons a as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show a, ShowHelper Identity as) => ShowHelper Identity (a ': as)
  where
    showsHelper (Cons (Identity a) Nil) = shows a
    showsHelper (Cons (Identity a) as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show b, ShowHelper (Const b) as) => ShowHelper (Const b) (a ': as)
  where
    showsHelper (Cons (Const a) Nil) = shows a
    showsHelper (Cons (Const a) as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance Bounded (Product g '[]) where
    minBound = Nil
    maxBound = Nil


------------------------------------------------------------------------------
instance (Bounded (g a), Bounded (Product g as)) =>
    Bounded (Product g (a ': as))
  where
    minBound = Cons minBound minBound
    maxBound = Cons maxBound maxBound


------------------------------------------------------------------------------
instance Enum (Product g '[]) where
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
    Enum (Product g (a ': as))
  where
    succ (Cons a as)
        | as == maxBound = Cons (succ a) minBound
        | otherwise = Cons a (succ as)
    pred (Cons a as)
        | as == minBound = Cons (pred a) maxBound
        | otherwise = Cons a (pred as)
    fromEnum (Cons a as) =
        (fromEnum a * (fromEnum (maxBound :: Product g as) + 1)) + fromEnum as
    toEnum n = Cons (toEnum a) (toEnum as)
      where
        (a, as) = divMod n (fromEnum (maxBound :: Product g as) + 1)
    enumFrom a = a : enumFrom (succ a)
    enumFromTo a b
        | a == b = [a]
        | otherwise = a : enumFromTo (succ a) b


------------------------------------------------------------------------------
instance (Enum (Product g as), Ord (Product g as)) => Ix (Product g as) where
    range = uncurry enumFromTo
    index (a, b) i
        | inRange (a, b) i = toEnum (fromEnum i - fromEnum a)
        | otherwise = error "Ix{Data.Product}.index: error in array index"
    {-# INLINE index #-}
    inRange (a, b) i = i >= a && i <= b


#if __GLASGOW_HASKELL__ >= 800
------------------------------------------------------------------------------
instance Semigroup (Product g '[]) where
    Nil <> Nil = Nil


------------------------------------------------------------------------------
instance (Semigroup (g a), Semigroup (Product g as)) =>
    Semigroup (Product g (a ': as))
  where
    Cons a as <> Cons b bs = Cons (a <> b) (as <> bs)


#endif
------------------------------------------------------------------------------
instance Monoid (Product g '[]) where
    mempty = Nil
    mappend Nil Nil = Nil


------------------------------------------------------------------------------
instance (Monoid (g a), Monoid (Product g as)) => Monoid (Product g (a ': as))
  where
    mempty = Cons mempty mempty
    mappend (Cons a as) (Cons b bs) = Cons (mappend a b) (mappend as bs)


------------------------------------------------------------------------------
instance Storable (Product g '[]) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return Nil
    poke _ Nil = return ()


------------------------------------------------------------------------------
instance (Storable (g a), Storable (Product g as)) =>
    Storable (Product g (a ': as))
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
#if __GLASGOW_HASKELL__ >= 800
type ProductMetaData
    = 'MetaData "Product" "Data.Anonymous.Product.Product" "anonymous-types"
        'False
type ProductMetaConsNil = 'MetaCons "Nil" 'PrefixI 'False
type ProductMetaConsCons = 'MetaCons "Cons" 'PrefixI 'False
type ProductMetaSelCons0
    = 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy
type ProductMetaSelCons1
    = 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy
#else
data ProductMetaData
data ProductMetaConsNil
data ProductMetaConsCons
type ProductMetaSelCons0 = NoSelector
type ProductMetaSelCons1 = NoSelector


------------------------------------------------------------------------------
instance Datatype ProductMetaData where
    datatypeName _ = "Product"
    moduleName _ = "Data.Product"


------------------------------------------------------------------------------
instance Constructor ProductMetaConsNil where
    conName _ = "Nil"


------------------------------------------------------------------------------
instance Constructor ProductMetaConsCons where
    conName _ = "Cons"


#endif
------------------------------------------------------------------------------
instance Generic (Product g '[]) where
    type Rep (Product g '[]) = D1 ProductMetaData (C1 ProductMetaConsNil U1)
    from Nil = M1 (M1 U1)
    to (M1 (M1 U1)) = Nil


------------------------------------------------------------------------------
instance Generic (Product g (a ': as)) where
    type Rep (Product g (a ': as)) = D1 ProductMetaData (C1 ProductMetaConsCons
        ((:*:)
            (S1 ProductMetaSelCons0 (Rec0 (g a)))
            (S1 ProductMetaSelCons1 (Rec0 (Product g as)))))

    from (Cons a as) = M1 (M1 ((:*:) (M1 (K1 a)) (M1 (K1 as))))
    to (M1 (M1 ((:*:) (M1 (K1 a)) (M1 (K1 as))))) = Cons a as


------------------------------------------------------------------------------
type Field = Uncurry F.Field


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 710
pattern Field :: KnownSymbol s => a -> Field '(s, a)
pattern Field a = Uncurry (F.Field a)


#endif
------------------------------------------------------------------------------
field :: KnownSymbol s => proxy s -> a -> Field '(s, a)
field _ a = Uncurry (F.Field a)
