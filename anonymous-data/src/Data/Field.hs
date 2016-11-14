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

#if MIN_VERSION_base(4, 7, 0) && !MIN_VERSION_base(4, 8, 0)
{-# OPTIONS_GHC -fno-warn-amp #-}
#endif

module Data.Field
    ( Field (Field)
    , field
    -- * Functor
    , (<$)
    , ($>)
    , (<$>)
    , fmap
    , void
    -- * Applicative
    , (<*)
    , (*>)
    , (<*>)
    , liftA2
    , liftA3
    , pure
    -- * Monad
    , (>>=)
    , (=<<)
    , (>=>)
    , (<=<)
    , foldM
    , foldM_
    , forever
    , forM
    , forM_
    , filterM
    , join
    , mapM
    , mapM_
    , mfix
    , munzip
    , mzipWith
    , replicateM
    , replicateM_
    , sequence
    , sequence_
    , when
    , zipWithM
    , zipWithM_
    -- * Foldable
    , all
    , and
    , any
    , asum
    , concat
    , concatMap
    , elem
    , fold
    , foldl
    , foldl'
    , foldlM
    , foldMap
    , foldr
    , foldr'
    , foldrM
    , for_
    , length
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , notElem
    , null
    , or
    , product
    , sequenceA_
    , sum
    , toList
    , traverse_
    -- * Traversable
    , for
    , mapAccumL
    , mapAccumR
    , sequenceA
    , traverse
    -- * Eq1, Ord1, Read1, Show1
    , liftEq
    , liftCompare
    , liftReadsPrec
    , liftReadList
    , liftShowsPrec
    , liftShowList
#if MIN_VERSION_base(4, 4, 0)
    -- * Generic1
    , Rep1
    , from1
    , to1
#endif
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative
                     (
#if !MIN_VERSION_base(4, 8, 0)
                       Applicative
                     ,
#endif
                       (<|>)
                     , empty
                     )
import           Control.Monad (guard)
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
import           Data.Foldable (Foldable)
#endif
import qualified Data.Foldable as T (foldr)
import           Data.Function (fix)
import qualified Data.Functor as T ((<$), fmap)
import           Data.Ix (Ix, range, index, inRange)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Data.String (IsString, fromString)
import qualified Data.Traversable as T ()
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
                     , Par1 (Par1)
                     , Rec0
                     , Rep
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
                     , to
                     )
#endif
import           Prelude hiding
                     (
#if MIN_VERSION_base(4, 8, 0)
                       (<$)
                     , (<*)
                     , (*>)
                     , (<*>)
                     , (<$>)
                     ,
#endif
                       (>>=)
                     , (=<<)
                     , all
                     , and
                     , any
                     , concat
                     , concatMap
                     , elem
                     , fmap
                     , foldl
#if MIN_VERSION_base(4, 8, 0)
                     , foldMap
#endif
                     , foldr
                     , length
                     , mapM
                     , mapM_
                     , maximum
                     , minimum
                     , notElem
                     , null
                     , or
                     , product
#if MIN_VERSION_base(4, 8, 0)
                     , pure
#endif
                     , sequence
                     , sequence_
#if MIN_VERSION_base(4, 8, 0)
                     , sequenceA
#endif
                     , sum
#if MIN_VERSION_base(4, 8, 0)
                     , traverse
#endif
                     )


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData, rnf)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat
                     ( KnownSymbol
#ifdef DataPolyKinds
                     , Symbol
#endif
                     , symbolVal
                     )
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
fmap :: (a -> b) -> Field (Pair s a) -> Field (Pair s b)
fmap f (Field a) = Field (f a)
{-# INLINE fmap #-}


------------------------------------------------------------------------------
(<$) :: a -> Field (Pair s b) -> Field (Pair s a)
(<$) = (<$>) . const
infixl 4 <$
{-# INLINE (<$) #-}


------------------------------------------------------------------------------
($>) :: Field (Pair s a) -> b -> Field (Pair s b)
($>) = flip (<$)
infixl 4 $>


------------------------------------------------------------------------------
(<$>) :: (a -> b) -> Field (Pair s a) -> Field (Pair s b)
(<$>) = fmap
infixl 4 <$>
{-# INLINE (<$>) #-}


------------------------------------------------------------------------------
void :: Field (Pair s b) -> Field (Pair s ())
void = (() <$)


------------------------------------------------------------------------------
pure :: KnownSymbol s => a -> Field (Pair s a)
pure = Field
{-# INLINE pure #-}


------------------------------------------------------------------------------
(<*) :: Field (Pair s a) -> Field (Pair s b) -> Field (Pair s a)
(<*) = liftA2 const
infixl 4 <*


------------------------------------------------------------------------------
(*>) :: Field (Pair s a) -> Field (Pair s b) -> Field (Pair s b)
a *> b = (id <$ a) <*> b
infixl 4 *>


------------------------------------------------------------------------------
(<*>) :: Field (Pair s (a -> b)) -> Field (Pair s a) -> Field (Pair s b)
Field f <*> Field a = Field (f a)
infixl 4 <*>
{-# INLINE (<*>) #-}


------------------------------------------------------------------------------
liftA2
    :: (a -> b -> c)
    -> Field (Pair s a)
    -> Field (Pair s b)
    -> Field (Pair s c)
liftA2 f a b = f <$> a <*> b
{-# INLINABLE liftA2 #-}


------------------------------------------------------------------------------
liftA3
    :: (a -> b -> c -> d)
    -> Field (Pair s a)
    -> Field (Pair s b)
    -> Field (Pair s c)
    -> Field (Pair s d)
liftA3 f a b c = f <$> a <*> b <*> c
{-# INLINABLE liftA3 #-}


------------------------------------------------------------------------------
(>>=) :: Field (Pair s a) -> (a -> Field (Pair s b)) -> Field (Pair s b)
Field a >>= f = f a
infixl 1 >>=
{-# INLINE (>>=) #-}


------------------------------------------------------------------------------
(=<<) :: (a -> Field (Pair s b)) -> Field (Pair s a) -> Field (Pair s b)
(=<<) = flip (>>=)
infixr 1 =<<


------------------------------------------------------------------------------
(>=>)
    :: (a -> Field (Pair s b))
    -> (b -> Field (Pair s c))
    -> (a -> Field (Pair s c))
f >=> g = \x -> f x >>= g
infixr 1 >=>


------------------------------------------------------------------------------
(<=<)
    :: (b -> Field (Pair s c))
    -> (a -> Field (Pair s b))
    -> (a -> Field (Pair s c))
(<=<) = flip (>=>)
infixr 1 <=<


------------------------------------------------------------------------------
foldM :: (Foldable t, KnownSymbol s)
    => (b -> a -> Field (Pair s b))
    -> b
    -> t a
    -> Field (Pair s b)
foldM f = flip (T.foldr (\a m b -> f b a >>= m) pure)
{-# INLINABLE foldM #-}


------------------------------------------------------------------------------
foldM_ :: (Foldable t, KnownSymbol s)
    => (b -> a -> Field (Pair s b))
    -> b
    -> t a
    -> Field (Pair s ())
foldM_ f a xs = foldM f a xs *> pure ()
{-# INLINABLE foldM_ #-}


------------------------------------------------------------------------------
forever :: Field (Pair s a) -> Field (Pair s b)
forever a = let a' = a *> a' in a'
{-# INLINE forever #-}


------------------------------------------------------------------------------
forM :: (KnownSymbol s, Functor t)
    => t a
    -> (a -> Field (Pair s b))
    -> Field (Pair s (t b))
forM = flip mapM
{-# INLINE forM #-}


------------------------------------------------------------------------------
forM_ :: KnownSymbol s => t a -> (a -> Field (Pair s b)) -> Field (Pair s ())
forM_ = flip mapM_
{-# INLINE forM_ #-}


------------------------------------------------------------------------------
filterM :: KnownSymbol s
    => (a -> Field (Pair s Bool))
    -> [a]
    -> Field (Pair s [a])
filterM p =
    T.foldr (\a -> liftA2 (\t -> if t then (a :) else id) (p a)) (pure [])
{-# INLINE filterM #-}


------------------------------------------------------------------------------
join :: Field (Pair s (Field (Pair s a))) -> Field (Pair s a)
join (Field a) = a
{-# INLINE join #-}


------------------------------------------------------------------------------
mapM :: (KnownSymbol s, Functor t)
    => (a -> Field (Pair s b))
    -> t a
    -> Field (Pair s (t b))
mapM f = Field . T.fmap (go . f)
  where
    go :: Field (Pair s a) -> a
    go (Field a) = a
{-# INLINABLE mapM #-}


------------------------------------------------------------------------------
mapM_ :: KnownSymbol s => (a -> Field (Pair s b)) -> t a -> Field (Pair s ())
mapM_ = const (const (Field ()))
{-# INLINE mapM_ #-}


------------------------------------------------------------------------------
replicateM :: Int -> Field (Pair s a) -> Field (Pair s [a])
replicateM n (Field a) = Field (replicate n a)


------------------------------------------------------------------------------
replicateM_ :: Int -> Field (Pair s a) -> Field (Pair s ())
replicateM_ _ (Field _) = Field ()


------------------------------------------------------------------------------
sequence :: (Functor t, KnownSymbol s)
    => t (Field (Pair s a))
    -> Field (Pair s (t a))
sequence = mapM id
{-# INLINE sequence #-}


------------------------------------------------------------------------------
sequence_ :: KnownSymbol s
    => t (Field (Pair s a))
    -> Field (Pair s ())
sequence_ = mapM_ id
{-# INLINE sequence_ #-}


------------------------------------------------------------------------------
when :: KnownSymbol s => Bool -> Field (Pair s ()) -> Field (Pair s ())
when p s = if p then s else pure ()
{-# INLINABLE when #-}


------------------------------------------------------------------------------
zipWithM :: KnownSymbol s
    => (a -> b -> Field (Pair s c))
    -> ([a] -> [b] -> Field (Pair s [c]))
zipWithM f as bs = sequence (zipWith f as bs)
{-# INLINE zipWithM #-}


------------------------------------------------------------------------------
zipWithM_ :: KnownSymbol s
    => (a -> b -> Field (Pair s c))
    -> ([a] -> [b] -> Field (Pair s ()))
zipWithM_ f as bs = sequence_ (zipWith f as bs)
{-# INLINE zipWithM_ #-}


------------------------------------------------------------------------------
mfix :: forall s a. KnownSymbol s
    => (a -> Field (Pair s a))
    -> Field (Pair s a)
mfix f = Field (fix go)
  where
    go :: a -> a
    go a = case f a of
        Field r -> r
{-# INLINABLE mfix #-}


------------------------------------------------------------------------------
mzipWith
    :: (a -> b -> c)
    -> Field (Pair s a)
    -> Field (Pair s b)
    -> Field (Pair s c)
mzipWith f (Field a) (Field b) = Field (f a b)
{-# INLINE mzipWith #-}


------------------------------------------------------------------------------
munzip :: Field (Pair s (a, b)) -> (Field (Pair s a), Field (Pair s b))
munzip (Field (a, b)) = (Field a, Field b)
{-# INLINE munzip #-}


------------------------------------------------------------------------------
all :: (a -> Bool) -> Field (Pair s a) -> Bool
all f (Field a) = f a
{-# INLINE all #-}


------------------------------------------------------------------------------
and :: Field (Pair s Bool) -> Bool
and (Field a) = a
{-# INLINE and #-}


------------------------------------------------------------------------------
any :: (a -> Bool) -> Field (Pair s a) -> Bool
any f (Field a) = f a
{-# INLINE any #-}


------------------------------------------------------------------------------
asum :: Field (Pair s (f a)) -> f a
asum (Field a) = a
{-# INLINE asum #-}


------------------------------------------------------------------------------
concat :: Field (Pair s [a]) -> [a]
concat (Field a) = a
{-# INLINE concat #-}


------------------------------------------------------------------------------
concatMap :: (a -> [b]) -> Field (Pair s a) -> [b]
concatMap f (Field a) = f a
{-# INLINE concatMap #-}


------------------------------------------------------------------------------
elem :: Eq a => a -> Field (Pair s a) -> Bool
elem a (Field b) = a == b
{-# INLINE elem #-}


------------------------------------------------------------------------------
fold :: Field (Pair s a) -> a
fold (Field a) = a
{-# INLINE fold #-}


------------------------------------------------------------------------------
foldl :: (b -> a -> b) -> b -> Field (Pair s a) -> b
foldl f b (Field a) = f b a
{-# INLINE foldl #-}


------------------------------------------------------------------------------
foldl' :: (b -> a -> b) -> b -> Field (Pair s a) -> b
foldl' f b (Field a) = id $! f b a


------------------------------------------------------------------------------
foldlM :: (b -> a -> m b) -> b -> Field (Pair s a) -> m b
foldlM f b (Field a) = f b a
{-# INLINE foldlM #-}


------------------------------------------------------------------------------
foldMap :: (a -> m) -> Field (Pair s a) -> m
foldMap f (Field a) = f a
{-# INLINE foldMap #-}


------------------------------------------------------------------------------
foldr :: (a -> b -> b) -> b -> Field (Pair s a) -> b
foldr f b (Field a) = f a b
{-# INLINE foldr #-}


------------------------------------------------------------------------------
foldr' :: (a -> b -> b) -> b -> Field (Pair s a) -> b
foldr' f b (Field a) = id $! f a b


------------------------------------------------------------------------------
foldrM :: (a -> b -> m b) -> b -> Field (Pair s a) -> m b
foldrM f b (Field a) = f a b
{-# INLINE foldrM #-}


------------------------------------------------------------------------------
for_ :: Functor f => Field (Pair s a) -> (a -> f b) -> f ()
for_ = flip traverse_
{-# INLINE for_ #-}


------------------------------------------------------------------------------
length :: Field (Pair s a) -> Int
length = const 1
{-# INLINE length #-}


------------------------------------------------------------------------------
maximum :: Field (Pair s a) -> a
maximum (Field a) = a
{-# INLINE maximum #-}


------------------------------------------------------------------------------
maximumBy :: (a -> a -> Ordering) -> Field (Pair s a) -> a
maximumBy _ (Field a) = a
{-# INLINE maximumBy #-}


------------------------------------------------------------------------------
minimum :: Field (Pair s a) -> a
minimum (Field a) = a



------------------------------------------------------------------------------
minimumBy :: (a -> a -> Ordering) -> Field (Pair s a) -> a
minimumBy _ (Field a) = a
{-# INLINE minimumBy #-}


------------------------------------------------------------------------------
notElem :: Eq a => a -> Field (Pair s a) -> Bool
notElem a (Field b) = a /= b
infix 4 `notElem`
{-# INLINE notElem #-}


------------------------------------------------------------------------------
null :: Field (Pair s a) -> Bool
null = const False
{-# INLINE null #-}


------------------------------------------------------------------------------
or :: Field (Pair s Bool) -> Bool
or (Field a) = a
{-# INLINE or #-}


------------------------------------------------------------------------------
product :: Field (Pair s a) -> a
product (Field a) = a
{-# INLINE product #-}


------------------------------------------------------------------------------
sequenceA_ :: Functor f => Field (Pair s (f a)) -> f ()
sequenceA_ = traverse_ id
{-# INLINE sequenceA_ #-}


------------------------------------------------------------------------------
sum :: Field (Pair s a) -> a
sum (Field a) = a
{-# INLINE sum #-}


------------------------------------------------------------------------------
toList :: Field (Pair s a) -> [a]
toList (Field a) = [a]
{-# INLINE toList #-}


------------------------------------------------------------------------------
traverse_ :: Functor f => (a -> f b) -> Field (Pair s a) -> f ()
traverse_ f (Field a) = () T.<$ f a
{-# INLINABLE traverse_ #-}


------------------------------------------------------------------------------
for :: Applicative f => Field (Pair s a) -> (a -> f b) -> f (Field (Pair s b))
for = flip traverse
{-# INLINE for #-}


------------------------------------------------------------------------------
mapAccumL :: (a -> b -> (a, c)) -> a -> Field (Pair s b) -> (a, Field (Pair s c))
mapAccumL f a (Field b) = T.fmap Field $ f a b
{-# INLINE mapAccumL #-}


------------------------------------------------------------------------------
mapAccumR :: (a -> b -> (a, c)) -> a -> Field (Pair s b) -> (a, Field (Pair s c))
mapAccumR f a (Field b) = T.fmap Field $ f a b
{-# INLINE mapAccumR #-}


------------------------------------------------------------------------------
sequenceA :: Applicative f => Field (Pair s (f a)) -> f (Field (Pair s a))
sequenceA = traverse id
{-# INLINE sequenceA #-}


------------------------------------------------------------------------------
traverse :: Functor f
    => (a -> f b)
    -> Field (Pair s a)
    -> f (Field (Pair s b))
traverse f (Field a) = T.fmap Field $ f a
{-# INLINE traverse #-}


------------------------------------------------------------------------------
liftEq :: (a -> b -> Bool) -> Field (Pair s a) -> Field (Pair s b) -> Bool
liftEq eq (Field a) (Field b) = eq a b
{-# INLINE liftEq #-}


------------------------------------------------------------------------------
liftCompare
    :: (a -> b -> Ordering)
    -> Field (Pair s a)
    -> Field (Pair s b)
    -> Ordering
liftCompare compare_ (Field a) (Field b) = compare_ a b
{-# INLINE liftCompare #-}


------------------------------------------------------------------------------
liftReadList :: forall s a. KnownSymbol s
    => (Int -> ReadS a)
    -> ReadS [a]
    -> ReadS [Field (Pair s a)]
liftReadList rp rl s = do
    ("[", s') <- lex s
    go id s' <|> do
        ("]", s'') <- lex s'
        return ([], s'')
  where
    go
        :: ([Field (Pair s a)] -> [Field (Pair s a)])
        -> ReadS [Field (Pair s a)]
    go dlist s' = do
        (a, s'') <- liftReadsPrec rp rl 0 s'
        (c, s''') <- lex s''
        case c of
            "]" -> return (dlist [a], s''')
            "," -> go (dlist . (a :)) s'''
            _ -> empty


------------------------------------------------------------------------------
liftReadsPrec :: forall s a. KnownSymbol s
    => (Int -> ReadS a)
    -> ReadS [a]
    -> Int
    -> ReadS (Field (Pair s a))
liftReadsPrec readsPrec_ _ p = readParen (p > 11) $ \s -> do
    (label, s') <- lex s
    guard $ label == symbolVal (Proxy :: Proxy s)
    ("=", s'') <- lex s'
    (value, s''') <- readsPrec_ 0 s''
    return $ (field (Proxy :: Proxy s) value, s''')


------------------------------------------------------------------------------
liftShowList
    :: (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> [Field (Pair s a)]
    -> ShowS
liftShowList _ _ [] s = "[]" ++ s
liftShowList sp sl (a : as) s = '[' : liftShowsPrec sp sl 0 a (go as)
  where
    go [] = ']' : s
    go (a' : as') = ',' : liftShowsPrec sp sl 0 a' (go as')


------------------------------------------------------------------------------
liftShowsPrec :: forall s a.
       (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> Field (Pair s a)
    -> ShowS
liftShowsPrec showsPrec_ _ p (Field a) = showParen (p > 10) $ T.foldr (.) id
    [ showString (symbolVal (Proxy :: Proxy s))
    , showString " = "
    , showsPrec_ 11 a
    ]


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
type Rep1 = D1 FieldMetaData (C1 FieldMetaCons (S1 FieldMetaSel Par1))


------------------------------------------------------------------------------
from1 :: Field (Pair s a) -> Rep1 a
from1 (Field a) = M1 (M1 (M1 (Par1 a)))


------------------------------------------------------------------------------
to1 :: KnownSymbol s => Rep1 a -> Field (Pair s a)
to1 (M1 (M1 (M1 (Par1 a)))) = Field a


#endif
------------------------------------------------------------------------------
instance Eq a => Eq (Field (Pair s a)) where
    Field a == Field b = a == b


------------------------------------------------------------------------------
instance Ord a => Ord (Field (Pair s a)) where
    compare (Field a) (Field b) = compare a b


------------------------------------------------------------------------------
instance (KnownSymbol s, Read a) => Read (Field (Pair s a)) where
    readsPrec p = readParen (p > 11) $ \s -> do
        (label, s') <- lex s
        guard $ label == symbolVal (Proxy :: Proxy s)
        ("=", s'') <- lex s'
        (value, s''') <- readsPrec 0 s''
        return $ (field (Proxy :: Proxy s) value, s''')


------------------------------------------------------------------------------
instance Show a => Show (Field (Pair s a)) where
    showsPrec p (Field a) = showParen (p > 10) $ T.foldr (.) id
        [ showString (symbolVal (Proxy :: Proxy s))
        , showString " = "
        , shows a
        ]


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
    range (Field a, Field b) = T.fmap Field $ range (a, b)
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
    peek = T.fmap Field . peek . castPtr
    poke ptr (Field a) = poke (castPtr ptr) a


------------------------------------------------------------------------------
instance (KnownSymbol s, Num a) => Num (Field (Pair s a)) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Field . fromInteger


------------------------------------------------------------------------------
instance (KnownSymbol s, Real a) => Real (Field (Pair s a)) where
    toRational (Field a) = toRational a


------------------------------------------------------------------------------
instance (KnownSymbol s, Integral a) => Integral (Field (Pair s a)) where
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
instance (KnownSymbol s, Fractional a) => Fractional (Field (Pair s a)) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = Field . fromRational


------------------------------------------------------------------------------
instance (KnownSymbol s, Floating a) => Floating (Field (Pair s a)) where
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
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Field a) = isNaN a
    isInfinite (Field a) = isInfinite a
    isDenormalized (Field a) = isDenormalized a
    isNegativeZero (Field a) = isNegativeZero a
    isIEEE (Field a) = isIEEE a
    atan2 = liftA2 atan2


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


#if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 9, 0)
type FieldMetaData = 'MetaData "Field" "Data.Field" "anonymous-data" 'False
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
instance KnownSymbol s => Generic (Field (Pair s a)) where
    type Rep (Field (Pair s a)) = D1 FieldMetaData
        (C1 FieldMetaCons (S1 FieldMetaSel (Rec0 a)))
    from (Field a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Field a


#endif
------------------------------------------------------------------------------
instance NFData a => NFData (Field (Pair s a)) where
    rnf (Field a) = rnf a
