{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

module Data.Anonymous.Product.Lens
    ( Index (index')
    , index
    , Element (element')
    , element
    , Key (key')
    , key
    , Value (value')
    , value
    , Indices (indices')
    , indices
    , Elements (elements')
    , elements
    , Keys (keys')
    , keys
    , Values (values')
    , values
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const), getConst)
#if __GLASGOW_HASKELL__ < 710
import           Data.Functor ((<$>))
#endif
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.Proxy (Proxy (Proxy))
import           GHC.TypeLits (type (-))


-- anonymous-types -----------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil))
import           Type.List.Fields
                     ( FindKey
                     , FindKeys
                     , FindIndex
                     , FindIndices
                     , LookupKey
                     , LookupKeys
                     , LookupIndex
                     , LookupIndices
                     , UpdateKey
                     , UpdateKeys
                     , UpdateIndex
                     , UpdateIndices
                     )


------------------------------------------------------------------------------
class (a ~ LookupIndex n as, bs ~ UpdateIndex n as b) => Index n as bs a b
    | n as -> a
    , n bs -> b
    , n as b -> bs
    , n bs a -> as
  where
    index' :: Functor f
        => proxy n
        -> (g a -> f (g b))
        -> Product g as
        -> f (Product g bs)


------------------------------------------------------------------------------
instance __OVERLAPPING__ Index 0 (a ': as) (a ': as) a a where
    index' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance Index 0 (a ': as) (b ': as) a b where
    index' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Index (n - 1) as as a a
    , LookupIndex n (c ': as) ~ a
    , UpdateIndex n (c ': as) a ~ (c ': as)
    )
  =>
    Index n (c ': as) (c ': as) a a
  where
    index' _ f (Cons a as) = Cons a <$> index' (Proxy :: Proxy (n - 1)) f as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Index (n - 1) as bs a b
    , LookupIndex n (c ': as) ~ a
    , UpdateIndex n (c ': as) b ~ (c ': bs)
    )
  =>
    Index n (c ': as) (c ': bs) a b
  where
    index' _ f (Cons a as) = Cons a <$> index' (Proxy :: Proxy (n - 1)) f as


------------------------------------------------------------------------------
index
    :: forall n as bs a b g f. (Index n as bs a b, Functor f)
    => (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
index = index' (Proxy :: Proxy n)


------------------------------------------------------------------------------
class (n ~ FindIndex a as, bs ~ UpdateIndex n as b) => Element n as bs a b
    | n as -> a
    , n bs -> b
    , n as b -> bs
    , n bs a -> as
  where
    element' :: Functor f
        => proxy a
        -> (g a -> f (g b))
        -> Product g as
        -> f (Product g bs)


------------------------------------------------------------------------------
instance __OVERLAPPING__ Element 0 (a ': as) (a ': as) a a where
    element' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance Element 0 (a ': as) (b ': as) a b where
    element' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Element (n - 1) as as a a
    , FindIndex a (c ': as) ~ n
    , UpdateIndex n (c ': as) a ~ (c ': as)
    )
  =>
    Element n (c ': as) (c ': as) a a
  where
    element' e f (Cons a as) = Cons a <$> element' e f as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Element (n - 1) as bs a b
    , FindIndex a (c ': as) ~ n
    , UpdateIndex n (c ': as) b ~ (c ': bs)
    )
  =>
    Element n (c ': as) (c ': bs) a b
  where
    element' e f (Cons a as) = Cons a <$> element' e f as


------------------------------------------------------------------------------
element
    :: forall a n as bs b g f. (Element n as bs a b, Functor f)
    => (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
element = element' (Proxy :: Proxy a)


------------------------------------------------------------------------------
class (a ~ LookupKey n as, bs ~ UpdateKey n as b) => Key n as bs a b
    | n as -> a
    , n bs -> b
    , n as b -> as
    , n bs a -> bs
  where
    key' :: Functor f
        => proxy n
        -> (g '(n, a) -> f (g '(n, b)))
        -> Product g as
        -> f (Product g bs)


------------------------------------------------------------------------------
instance __OVERLAPPING__ Key n ('(n, a) ': as) ('(n, a) ': as) a a where
    key' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance Key n ('(n, a) ': as) ('(n, b) ': as) a b where
    key' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Key n as as a a
    , LookupKey n (c ': as) ~ a
    , UpdateKey n (c ': as) a ~ (c ': as)
    )
  =>
    Key n (c ': as) (c ': as) a a
  where
    key' p f (Cons a as) = Cons a <$> key' p f as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Key n as bs a b
    , LookupKey n (c ': as) ~ a
    , UpdateKey n (c ': as) b ~ (c ': bs)
    )
  =>
    Key n (c ': as) (c ': bs) a b
  where
    key' p f (Cons a as) = Cons a <$> key' p f as


------------------------------------------------------------------------------
key
    :: forall n as bs a b g f. (Key n as bs a b, Functor f)
    => (g '(n, a) -> f (g '(n, b)))
    -> Product g as
    -> f (Product g bs)
key = key' (Proxy :: Proxy n)


------------------------------------------------------------------------------
class (n ~ FindKey a as, bs ~ UpdateKey n as b) => Value n as bs a b
    | n as -> a
    , n bs -> b
    , n as b -> bs
    , n bs a -> as
  where
    value' :: Functor f
        => proxy a
        -> (g '(n, a) -> f (g '(n, b)))
        -> Product g as
        -> f (Product g bs)


------------------------------------------------------------------------------
instance __OVERLAPPING__ Value n ('(n, a) ': as) ('(n, a) ': as) a a where
    value' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance Value n ('(n, a) ': as) ('(n, b) ': as) a b where
    value' _ f (Cons a as) = fmap (\a' -> Cons a' as) (f a)


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Value n as as a a
    , FindKey a (c ': as) ~ n
    , UpdateKey n (c ': as) a ~ (c ': as)
    )
  =>
    Value n (c ': as) (c ': as) a a
  where
    value' p f (Cons a as) = Cons a <$> value' p f as


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Value n as bs a b
    , FindKey a (c ': as) ~ n
    , UpdateKey n (c ': as) b ~ (c ': bs)
    )
  =>
    Value n (c ': as) (c ': bs) a b
  where
    value' p f (Cons a as) = Cons a <$> value' p f as


------------------------------------------------------------------------------
value
    :: forall a n as bs b g f. (Value n as bs a b, Functor f)
    => (g '(n, a) -> f (g '(n, b)))
    -> Product g as
    -> f (Product g bs)
value = value' (Proxy :: Proxy a)


------------------------------------------------------------------------------
class (as ~ LookupIndices ns ss, ts ~ UpdateIndices ns ss bs) =>
    Indices ns ss ts as bs
        | ns ss -> as
        , ns ts -> bs
        , ns ss bs -> ts
        , ns ts as -> ss
  where
    indices' :: Functor f
        => proxy ns
        -> (Product g as -> f (Product g bs))
        -> Product g ss
        -> f (Product g ts)


------------------------------------------------------------------------------
instance Indices '[] ss ss '[] '[] where
    indices' _ f s = const s <$> f Nil


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Index n ss ss a a
    , UpdateIndex n ss a ~ ss
    , Indices ns ss ss as as
    , UpdateIndices ns ss as ~ ss
    )
  =>
    Indices (n ': ns) ss ss (a ': as) (a ': as)
  where
    indices' _ = lens get set'
      where
        get :: Product g ss -> Product g (a ': as)
        get ss = Cons
            (view (index' (Proxy :: Proxy n)) ss)
            (view (indices' (Proxy :: Proxy ns)) ss)
        set' :: Product g (a ': as) -> Product g ss -> Product g ss
        set' (Cons a as) = set (indices' (Proxy :: Proxy ns)) as
            . set (index' (Proxy :: Proxy n)) a


------------------------------------------------------------------------------
instance
    ( Index n ss ss a a
    , Index n ss ss' a b
    , Index n ss' ss b a
    , Index n ss' ss' b b
    , UpdateIndex n ss a ~ ss
    , UpdateIndex n ss b ~ ss'
    , UpdateIndex n ss' a ~ ss
    , UpdateIndex n ss' b ~ ss'
    , Indices ns ss ss as as
    , Indices ns ss ss' as as
    , Indices ns ss' ss as as
    , Indices ns ss' ss' as as
    , Indices ns ss' ts as bs
    , Indices ns ts ss' bs as
    , Indices ns ts ts bs bs
    , UpdateIndices ns ss as ~ ss
    , UpdateIndices ns ss' as ~ ss'
    , UpdateIndices ns ss' bs ~ ts
    , UpdateIndices ns ts as ~ ss'
    , UpdateIndices ns ts bs ~ ts
    )
  =>
    Indices (n ': ns) ss ts (a ': as) (b ': bs)
  where
    indices' _ = lens get set'
      where
        get :: Product g ss -> Product g (a ': as)
        get ss = Cons
            (view (index' (Proxy :: Proxy n)) ss)
            (view (indices' (Proxy :: Proxy ns)) ss)
        set' :: Product g (b ': bs) -> Product g ss -> Product g ts
        set' (Cons b bs) = set (indices' (Proxy :: Proxy ns)) bs
            . set (index' (Proxy :: Proxy n)) b


------------------------------------------------------------------------------
indices :: forall ns ss ts as bs g f. (Indices ns ss ts as bs, Functor f)
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
indices = indices' (Proxy :: Proxy ns)


------------------------------------------------------------------------------
class (ns ~ FindIndices as ss, ts ~ UpdateIndices ns ss bs) =>
    Elements ns ss ts as bs
        | ns ss -> as
        , ns ts -> bs
        , ns ss bs -> ts
        , ns ts as -> ss
  where
    elements' :: Functor f
        => proxy as
        -> (Product g as -> f (Product g bs))
        -> Product g ss
        -> f (Product g ts)


------------------------------------------------------------------------------
instance Elements '[] ss ss '[] '[] where
    elements' _ f s = const s <$> f Nil


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Element n ss ss a a
    , UpdateIndex n ss a ~ ss
    , Elements ns ss ss as as
    , UpdateIndices ns ss as ~ ss
    )
  =>
    Elements (n ': ns) ss ss (a ': as) (a ': as)
  where
    elements' _ = lens get set'
      where
        get :: Product g ss -> Product g (a ': as)
        get ss = Cons (view (element' Proxy) ss) (view (elements' Proxy) ss)
        set' :: Product g (a ': as) -> Product g ss -> Product g ss
        set' (Cons a as) = set (elements' (Proxy :: Proxy as)) as
            . set (element' (Proxy :: Proxy a)) a


------------------------------------------------------------------------------
instance
    ( Element n ss ss a a
    , Element n ss ss' a b
    , Element n ss' ss b a
    , Element n ss' ss' b b
    , UpdateIndex n ss a ~ ss
    , UpdateIndex n ss b ~ ss'
    , UpdateIndex n ss' a ~ ss
    , UpdateIndex n ss' b ~ ss'
    , Elements ns ss ss as as
    , Elements ns ss ss' as as
    , Elements ns ss' ss as as
    , Elements ns ss' ss' as as
    , Elements ns ss' ts as bs
    , Elements ns ts ss' bs as
    , Elements ns ts ts bs bs
    , UpdateIndices ns ss as ~ ss
    , UpdateIndices ns ss' as ~ ss'
    , UpdateIndices ns ss' bs ~ ts
    , UpdateIndices ns ts as ~ ss'
    , UpdateIndices ns ts bs ~ ts
    )
  =>
    Elements (n ': ns) ss ts (a ': as) (b ': bs)
  where
    elements' _ = lens get set'
      where
        get :: Product g ss -> Product g (a ': as)
        get ss = Cons (view (element' Proxy) ss) (view (elements' Proxy) ss)
        set' :: Product g (b ': bs) -> Product g ss -> Product g ts
        set' (Cons b bs) = set (elements' (Proxy :: Proxy as)) bs
            . set (element' (Proxy :: Proxy a)) b


------------------------------------------------------------------------------
elements :: forall as ns ss ts bs g f. (Elements ns ss ts as bs, Functor f)
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
elements = elements' (Proxy :: Proxy as)


------------------------------------------------------------------------------
class (LookupKeys (MapFst as) ss ~ MapSnd as, UpdateKeys ss bs ~ ts) =>
    Keys ss ts as bs
        | ss bs -> ts
        , ts as -> ss
        , ss ts as -> bs
        , ss ts bs -> as
  where
    keys' :: Functor f
        => proxy (MapFst as)
        -> (Product g as -> f (Product g bs))
        -> Product g ss
        -> f (Product g ts)


------------------------------------------------------------------------------
instance Keys ss ss '[] '[] where
    keys' _ f s = const s <$> f Nil


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Key n ss ss a a
    , UpdateKey n ss a ~ ss
    , Keys ss ss as as
    , UpdateKeys ss as ~ ss
    , UpdateKeys ss ('(n, a) ': as) ~ ss
    )
  =>
    Keys ss ss ('(n, a) ': as) ('(n, a) ': as)
  where
    keys' _ = lens get set'
      where
        get :: Product g ss -> Product g ('(n, a) ': as)
        get ss = Cons
            (view (key' (Proxy :: Proxy n)) ss)
            (view (keys' (Proxy :: Proxy (MapFst as))) ss)
        set' :: Product g ('(n, a) ': as) -> Product g ss -> Product g ss
        set' (Cons a as) = set (keys' (Proxy :: Proxy (MapFst as))) as
            . set (key' (Proxy :: Proxy n)) a


------------------------------------------------------------------------------
instance
    ( Key n ss ss a a
    , Key n ss ss' a b
    , Key n ss' ss b a
    , Key n ss' ss' b b
    , UpdateKey n ss a ~ ss
    , UpdateKey n ss b ~ ss'
    , UpdateKey n ss' a ~ ss
    , UpdateKey n ss' b ~ ss'
    , Keys ss ss as as
    , Keys ss ss' as as
    , Keys ss' ss as as
    , Keys ss' ss' as as
    , Keys ss' ts as bs
    , Keys ts ss' bs as
    , Keys ts ts bs bs
    , UpdateKeys ss as ~ ss
    , UpdateKeys ss' as ~ ss'
    , UpdateKeys ss' bs ~ ts
    , UpdateKeys ts as ~ ss'
    , UpdateKeys ts bs ~ ts
    , UpdateKeys ss ('(n, b) ': bs) ~ ts
    )
  =>
    Keys ss ts ('(n, a) ': as) ('(n, b) ': bs)
  where
    keys' _ = lens get set'
      where
        get :: Product g ss -> Product g ('(n, a) ': as)
        get ss = Cons
            (view (key' (Proxy :: Proxy n)) ss)
            (view (keys' (Proxy :: Proxy (MapFst as))) ss)
        set' :: Product g ('(n, b) ': bs) -> Product g ss -> Product g ts
        set' (Cons b bs) = set (keys' (Proxy :: Proxy (MapFst as))) bs
            . set (key' (Proxy :: Proxy n)) b


------------------------------------------------------------------------------
keys
    :: forall ns ss ts as bs f g.
        ( Keys ss ts as bs
        , Functor f
        , ns ~ MapFst as
        )
    =>  (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
keys = keys' (Proxy :: Proxy ns)


------------------------------------------------------------------------------
class (FindKeys (MapSnd as) ss ~ MapFst as, UpdateKeys ss bs ~ ts) =>
    Values ss ts as bs
        | ss bs -> ts
        , ts as -> ss
        , ss ts as -> bs
        , ss ts bs -> as
  where
    values' :: Functor f
        => proxy (MapSnd as)
        -> (Product g as -> f (Product g bs))
        -> Product g ss
        -> f (Product g ts)


------------------------------------------------------------------------------
instance Values ss ss '[] '[] where
    values' _ f s = const s <$> f Nil


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Value n ss ss a a
    , UpdateKey n ss a ~ ss
    , Values ss ss as as
    , UpdateKeys ss as ~ ss
    , UpdateKeys ss ('(n, a) ': as) ~ ss
    )
  =>
    Values ss ss ('(n, a) ': as) ('(n, a) ': as)
  where
    values' _ = lens get set'
      where
        get :: Product g ss -> Product g ('(n, a) ': as)
        get ss = Cons
            (view (value' (Proxy :: Proxy a)) ss)
            (view (values' (Proxy :: Proxy (MapSnd as))) ss)
        set' :: Product g ('(n, a) ': as) -> Product g ss -> Product g ss
        set' (Cons a as) = set (values' (Proxy :: Proxy (MapSnd as))) as
            . set (value' (Proxy :: Proxy a)) a


------------------------------------------------------------------------------
instance
    ( Value n ss ss a a
    , Value n ss ss' a b
    , Value n ss' ss b a
    , Value n ss' ss' b b
    , UpdateKey n ss a ~ ss
    , UpdateKey n ss b ~ ss'
    , UpdateKey n ss' a ~ ss
    , UpdateKey n ss' b ~ ss'
    , Values ss ss as as
    , Values ss ss' as as
    , Values ss' ss as as
    , Values ss' ss' as as
    , Values ss' ts as bs
    , Values ts ss' bs as
    , Values ts ts bs bs
    , UpdateKeys ss as ~ ss
    , UpdateKeys ss' as ~ ss'
    , UpdateKeys ss' bs ~ ts
    , UpdateKeys ts as ~ ss'
    , UpdateKeys ts bs ~ ts
    , UpdateKeys ss ('(n, b) ': bs) ~ ts
    )
  =>
    Values ss ts ('(n, a) ': as) ('(n, b) ': bs)
  where
    values' _ = lens get set'
      where
        get :: Product g ss -> Product g ('(n, a) ': as)
        get ss = Cons
            (view (value' (Proxy :: Proxy a)) ss)
            (view (values' (Proxy :: Proxy (MapSnd as))) ss)
        set' :: Product g ('(n, b) ': bs) -> Product g ss -> Product g ts
        set' (Cons b bs) = set (values' (Proxy :: Proxy (MapSnd as))) bs
            . set (value' (Proxy :: Proxy a)) b


------------------------------------------------------------------------------
values
    :: forall ns ss ts as bs f g.
        ( Values ss ts as bs
        , Functor f
        , ns ~ MapSnd as
        )
    =>  (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
values = values' (Proxy :: Proxy ns)


------------------------------------------------------------------------------
type family MapFst (as :: [(k, v)]) :: [k]
  where
    MapFst '[] = '[]
    MapFst ('(k, v) ': as) = k ': MapFst as


------------------------------------------------------------------------------
type family MapSnd (as :: [(k, v)]) :: [v]
  where
    MapSnd '[] = '[]
    MapSnd ('(k, v) ': as) = v ': MapSnd as


------------------------------------------------------------------------------
lens :: Functor f => (s -> a) -> (b -> s -> t) -> (a -> f b) -> s -> f t
lens get set' f s = flip set' s <$> f (get s)
{-# INLINE lens #-}


------------------------------------------------------------------------------
view :: ((s -> Const s s) -> a -> Const s a) -> a -> s
view l = getConst . l Const
{-# INLINE view #-}


------------------------------------------------------------------------------
set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)
{-# INLINE set #-}


{-
------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
instance (Functor f, Index (n - 1) as bs a b) => Has n (->) f
    (Product Identity as)
    (Product Identity bs)
    a
    b
#else
instance (Functor f, Index (n - 1) as as a a) => Has n (->) f
    (Product Identity as)
    (Product Identity as)
    a
    a
#endif
  where
    has _ f = index' (Proxy :: Proxy (n - 1)) (\(Identity a) -> Identity <$> f a)


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
instance (Functor f, Key n as bs a b) => Has n (->) f
    (Product Field as)
    (Product Field bs)
    a
    b
#else
instance (Functor f, Key n as as a a) => Has n (->) f
    (Product Field as)
    (Product Field as)
    a
    a
#endif
  where
    has p f = key' p (\(Uncurry (F.Field a)) -> Uncurry . F.Field <$> f a)
-}