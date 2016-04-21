{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
#include "incoherent.h"
#include "overlap.h"

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif


module Data.Anonymous.Product
    ( Product (Cons, Nil)
    , Record
    , Tuple
    )
where

-- anonymous-types -----------------------------------------------------------
import           Data.Field (Field)
import           Data.Uncurry (Uncurry)


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Control.Monad (msum)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Ix (Ix, inRange, range)
import qualified Data.Ix as I (index)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
#ifdef PolyTypeable
import           Data.Typeable (Typeable)
#endif
import           Foreign.Ptr (castPtr, plusPtr)
import           Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
#if MIN_VERSION_base(4, 4, 0)
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
#if MIN_VERSION_base(4, 9, 0)
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
#endif


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)


------------------------------------------------------------------------------
data Product (f :: KPoly1 -> *) (as :: KList (KPoly1)) where
    Nil :: Product f Nil
    Cons :: f a -> Product f as -> Product f (Cons a as)
#if defined(PolyTypeable)
  deriving (Typeable)
#endif


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
instance (Ord (g a), Ord (Product g as)) => Ord (Product g (Cons a as)) where
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
instance (ReadHelper (Uncurry Field) as, PlainRead (Uncurry Field) as) =>
    Read (Record as)
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
        (a, s') <- reads s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read (g a), PlainReadHelper g as) =>
    PlainReadHelper g (Cons a as)
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
instance Read a => ReadHelper Identity (Cons a Nil) where
    readsHelper s = do
        (a, s') <- reads s
        return (Cons (Identity a) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read a, ReadHelper Identity as) =>
    ReadHelper Identity (Cons a as)
  where
    readsHelper s = do
        (a, s') <- reads s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Identity a) as, s''')


------------------------------------------------------------------------------
instance Read b => ReadHelper (Const b) (Cons a Nil) where
    readsHelper s = do
        (b, s') <- reads s
        return (Cons (Const b) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Read b, ReadHelper (Const b) as) =>
    ReadHelper (Const b) (Cons a as)
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
instance ShowHelper (Uncurry Field) as => Show (Record as) where
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
    showsHelper (Cons a Nil) = shows a
    showsHelper (Cons a as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show a, ShowHelper Identity as) => ShowHelper Identity (Cons a as)
  where
    showsHelper (Cons (Identity a) Nil) = shows a
    showsHelper (Cons (Identity a) as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (Show b, ShowHelper (Const b) as) => ShowHelper (Const b) (Cons a as)
  where
    showsHelper (Cons (Const a) Nil) = shows a
    showsHelper (Cons (Const a) as) = foldr (.) id $
        [ shows a
        , showString ", "
        , showsHelper as
        ]


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


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
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
instance Generic (Product g Nil) where
    type Rep (Product g Nil) = D1 ProductMetaData (C1 ProductMetaConsNil U1)
    from Nil = M1 (M1 U1)
    to (M1 (M1 U1)) = Nil


------------------------------------------------------------------------------
instance Generic (Product g (Cons a as)) where
    type Rep (Product g (Cons a as)) = D1 ProductMetaData
        (C1 ProductMetaConsCons ((:*:)
            (S1 ProductMetaSelCons0 (Rec0 (g a)))
            (S1 ProductMetaSelCons1 (Rec0 (Product g as)))))

    from (Cons a as) = M1 (M1 ((:*:) (M1 (K1 a)) (M1 (K1 as))))
    to (M1 (M1 ((:*:) (M1 (K1 a)) (M1 (K1 as))))) = Cons a as


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
type Record = Product (Uncurry Field)


------------------------------------------------------------------------------
type Tuple = Product Identity
