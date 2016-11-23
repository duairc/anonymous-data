{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
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
    , (<:->)
#ifdef LanguagePatternSynonyms
    , pattern (:<::>)
    , pattern (:<:>)
    , pattern (:<:.>)
    , pattern (:<:?>)
    , pattern (:<:->)
#endif
    , (:<++>) ((<++>))
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
import           Data.Field (Field (Field))
import qualified Data.Classes as I
#ifdef ClosedTypeFamilies
import qualified Type.List.Fields as T
#endif


-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Control.Monad (msum)
import           Data.Functor.Compose (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Ix
                     ( Ix
                     , inRange
                     , range
#if __GLASGOW_HASKELL__ >= 700
                     , rangeSize
#endif
                     )
import qualified Data.Ix (index)
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
import           GHC.TypeLits.Compat
                     ( (:-)
                     , KnownSymbol
#ifdef DataPolyKinds
                     , Nat
#endif
                     , One
                     , Zero
                     )
import           Type.List (Cons, Nil)
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
instance (I.Eq (g a), Eq (Product g as)) => Eq (Product g (Cons a as)) where
    Cons a as == Cons b bs = a I.== b && as == bs


------------------------------------------------------------------------------
instance Ord (Product g Nil) where
    compare Nil Nil = EQ


------------------------------------------------------------------------------
instance (I.Ord (g a), Ord (Product g as)) => Ord (Product g (Cons a as))
  where
    compare (Cons a as) (Cons b bs) = mappend (I.compare a b) (compare as bs)


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
instance (ReadHelper Field as, PlainRead Field as) =>
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
instance I.Read (g a) => PlainReadHelper g (Cons a Nil) where
    plainReadsHelper s = do
        (a, s') <- I.readsPrec 0 s
        return (Cons a Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (I.Read (g a), PlainReadHelper g as) =>
    PlainReadHelper g (Cons a as)
  where
    plainReadsHelper s = do
        (a, s') <- I.readsPrec 0 s
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
instance I.Read a => ReadHelper Identity (Cons a Nil) where
    readsHelper s = do
        (a, s') <- I.readsPrec 0 s
        return (Cons (Identity a) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (I.Read a, ReadHelper Identity as) =>
    ReadHelper Identity (Cons a as)
  where
    readsHelper s = do
        (a, s') <- I.readsPrec 0 s
        (",", s'') <- lex s'
        (as, s''') <- readsHelper s''
        return (Cons (Identity a) as, s''')


------------------------------------------------------------------------------
instance I.Read b => ReadHelper (Const b) (Cons a Nil) where
    readsHelper s = do
        (b, s') <- I.readsPrec 0 s
        return (Cons (Const b) Nil, s')


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (I.Read b, ReadHelper (Const b) as) =>
    ReadHelper (Const b) (Cons a as)
  where
    readsHelper s = do
        (b, s') <- I.readsPrec 0 s
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
instance ShowHelper Field as => Show (Record as) where
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
    (I.Show (g a), ShowHelper g as)
  =>
    ShowHelper g (Cons a as)
  where
    showsHelper (Cons a Nil) = I.showsPrec 0 a
    showsHelper (Cons a as) = foldr (.) id $
        [ I.showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (I.Show a, ShowHelper Identity as) => ShowHelper Identity (Cons a as)
  where
    showsHelper (Cons (Identity a) Nil) = I.showsPrec 0 a
    showsHelper (Cons (Identity a) as) = foldr (.) id $
        [ I.showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance (I.Show b, ShowHelper (Const b) as) =>
    ShowHelper (Const b) (Cons a as)
  where
    showsHelper (Cons (Const a) Nil) = I.showsPrec 0 a
    showsHelper (Cons (Const a) as) = foldr (.) id $
        [ I.showsPrec 0 a
        , showString ", "
        , showsHelper as
        ]


------------------------------------------------------------------------------
instance Bounded (Product g Nil) where
    minBound = Nil
    maxBound = Nil


------------------------------------------------------------------------------
instance (I.Bounded (g a), Bounded (Product g as)) =>
    Bounded (Product g (Cons a as))
  where
    minBound = Cons I.minBound minBound
    maxBound = Cons I.maxBound maxBound


#if __GLASGOW_HASKELL__ >= 700
------------------------------------------------------------------------------
instance I.Enum (Product g as) => Enum (Product g as) where
    enumFrom = I.enumFrom
    {-# INLINE enumFrom #-}
    enumFromThen = I.enumFromThen
    {-# INLINE enumFromThen #-}
    enumFromThenTo = I.enumFromThenTo
    {-# INLINE enumFromThenTo #-}
    enumFromTo = I.enumFromTo
    {-# INLINE enumFromTo #-}
    fromEnum = fromInteger . I.fromEnum
    {-# INLINE fromEnum #-}
    pred = I.pred
    {-# INLINE pred #-}
    succ = I.succ
    {-# INLINE succ #-}
    toEnum = I.toEnum . toInteger
    {-# INLINE toEnum #-}


#endif
------------------------------------------------------------------------------
instance I.Enum (Product g Nil) where
    fromEnum _ = 0
    toEnum 0 = Nil
    toEnum _ = error "Enum{Data.Product}.toEnum: bad argument"


------------------------------------------------------------------------------
instance
    ( I.Enum (g a)
    , I.Eq (g a)
    , Bounded (Product g as)
    , I.Enum (Product g as)
    , Eq (Product g as)
    )
  =>
    I.Enum (Product g (Cons a as))
  where
    succ (Cons a as)
        | as == maxBound = Cons (I.succ a) minBound
        | otherwise = Cons a (I.succ as)
    pred (Cons a as)
        | as == minBound = Cons (I.pred a) maxBound
        | otherwise = Cons a (I.pred as)
    fromEnum (Cons a as) = I.fromEnum as +
        (I.fromEnum a * (I.fromEnum (maxBound :: Product g as) + 1))
    toEnum n = Cons (I.toEnum a) (I.toEnum as)
      where
        (a, as) = divMod n (I.fromEnum (maxBound :: Product g as) + 1)
    enumFrom a = a : I.enumFrom (succ a)
    enumFromTo a b
        | a == b = [a]
        | otherwise = a : I.enumFromTo (succ a) b


#if __GLASGOW_HASKELL__ >= 700
------------------------------------------------------------------------------
instance (I.Ix (Product g as), Ord (Product g as)) => Ix (Product g as) where
    index = \r -> fromInteger . I.index r
    {-# INLINE index #-}
    inRange = I.inRange
    {-# INLINE inRange #-}
    range = I.range
    {-# INLINE range #-}
    rangeSize = fromInteger . I.rangeSize
    {-# INLINE rangeSize #-}


#endif
------------------------------------------------------------------------------
instance (I.Enum (Product g as), I.Ord (Product g as)) => I.Ix (Product g as)
  where
    range = uncurry I.enumFromTo
    index (a, b) i
        | I.inRange (a, b) i = I.fromEnum i - I.fromEnum a
        | otherwise = error "Ix{Data.Product}.index: error in array index"
    {-# INLINE index #-}
    inRange (a, b) i = i I.>= a && i I.<= b



#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup (Product g Nil) where
    Nil <> Nil = Nil


------------------------------------------------------------------------------
instance (I.Semigroup (g a), Semigroup (Product g as)) =>
    Semigroup (Product g (Cons a as))
  where
    Cons a as <> Cons b bs = Cons (a I.<> b) (as <> bs)


#endif
------------------------------------------------------------------------------
instance Monoid (Product g Nil) where
    mempty = Nil
    mappend Nil Nil = Nil


------------------------------------------------------------------------------
instance (I.Monoid (g a), Monoid (Product g as)) =>
    Monoid (Product g (Cons a as))
  where
    mempty = Cons I.mempty mempty
    mappend (Cons a as) (Cons b bs) = Cons (I.mappend a b) (mappend as bs)


------------------------------------------------------------------------------
instance Storable (Product g Nil) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return Nil
    poke _ Nil = return ()


------------------------------------------------------------------------------
instance (I.Storable (g a), Storable (Product g as)) =>
    Storable (Product g (Cons a as))
  where
    sizeOf _ = roundUpToNearestMultipleOf
        (I.sizeOf (undefined :: g a))
        (alignment (undefined :: Product g as)) +
            sizeOf (undefined :: Product g as)
    alignment _ = I.alignment (undefined :: g a)
    peek ptr = do
        a <- I.peek (castPtr ptr)
        as <- peek (plusPtr ptr
            (roundUpToNearestMultipleOf
                (I.sizeOf a)
                (alignment (undefined :: Product g as))))
        return $ Cons a as
    poke ptr (Cons a as) = do
        I.poke (castPtr ptr) a
        poke
            (plusPtr ptr
                (roundUpToNearestMultipleOf (I.sizeOf a) (alignment as)))
            as


------------------------------------------------------------------------------
roundUpToNearestMultipleOf :: Integral a => a -> a -> a
roundUpToNearestMultipleOf n m = n + m - mod n m


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
type ProductMetaData
    = 'MetaData "Product" "Data.Anonymous.Product.Product" "anonymous-data"
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
type Record = Product Field


------------------------------------------------------------------------------
type Tuple = Product Identity


------------------------------------------------------------------------------
type Options = Product (Compose First Field)


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
    => a
    -> Options as
    -> Options (Cons (Pair s a) as)
(<:?>) = Cons . Compose . First . Just . Field
infixr 5 <:?>


------------------------------------------------------------------------------
(<:->) :: forall s a as. KnownSymbol s
    => Options as
    -> Options (Cons (Pair s a) as)
(<:->) as = Cons (Compose (First Nothing)) as
infixr 5 <:->


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
    => a
    -> Options as
    -> Options (Cons (Pair s a) as)
pattern (:<:?>) a as = Cons (Compose (First (Just (Field a)))) as
infixr 5 :<:?>


------------------------------------------------------------------------------
pattern (:<:->) :: forall s a as. KnownSymbol s
    => Options as
    -> Options (Cons (Pair s a) as)
pattern (:<:->) as = Cons (Compose (First Nothing)) as
infixr 5 :<:->


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
