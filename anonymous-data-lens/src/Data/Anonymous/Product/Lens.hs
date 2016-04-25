{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( Index
    , index'
    , index
    , Element
    , element'
    , element
    , Key
    , key'
    , key
    , Value
    , value'
    , value
    , Indices
    , indices'
    , indices
    , Elements
    , elements'
    , elements
    , Keys
    , keys'
    , keys
    , Values
    , values'
    , values
    , MapFst
    , MapSnd
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


-- anonymous-data ------------------------------------------------------------
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
class Index (FindIndex e as) as bs a b => Element e as bs a b


------------------------------------------------------------------------------
instance Index (FindIndex e as) as bs a b => Element e as bs a b


------------------------------------------------------------------------------
element'
    :: forall e as bs a b g f proxy. (Element e as bs a b, Functor f)
    => proxy e
    -> (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
element' _ = index' (Proxy :: Proxy (FindIndex e as))


------------------------------------------------------------------------------
element
    :: forall e as bs a b g f. (Element e as bs a b, Functor f)
    => (g a -> f (g b))
    -> Product g as
    -> f (Product g bs)
element = element' (Proxy :: Proxy e)


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
class Key (FindKey v as) as bs a b => Value v as bs a b


------------------------------------------------------------------------------
instance Key (FindKey v as) as bs a b => Value v as bs a b


------------------------------------------------------------------------------
value'
    :: forall v as bs a b g f proxy. (Value v as bs a b, Functor f)
    => proxy v
    -> (g '((FindKey v as), a) -> f (g '((FindKey v as), b)))
    -> Product g as
    -> f (Product g bs)
value' _ = key' (Proxy :: Proxy (FindKey v as))


------------------------------------------------------------------------------
value
    :: forall v as bs a b g f. (Value v as bs a b, Functor f)
    => (g '((FindKey v as), a) -> f (g '((FindKey v as), b)))
    -> Product g as
    -> f (Product g bs)
value = value' (Proxy :: Proxy v)


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
class Indices (FindIndices es ss) ss ts as bs => Elements es ss ts as bs


------------------------------------------------------------------------------
instance Indices (FindIndices es ss) ss ts as bs => Elements es ss ts as bs


------------------------------------------------------------------------------
elements'
    :: forall es ss ts as bs g f proxy. (Elements es ss ts as bs, Functor f)
    => proxy es
    -> (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
elements' _ = indices' (Proxy :: Proxy (FindIndices es ss))


------------------------------------------------------------------------------
elements
    :: forall es ss ts as bs g f.
        ( Elements es ss ts as bs
        , Functor f
        )
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
elements = elements' (Proxy :: Proxy es)


------------------------------------------------------------------------------
class (LookupKeys ns ss ~ MapSnd as, ns ~ MapFst as, UpdateKeys ns ss bs ~ ts)
  =>
    Keys ns ss ts as bs
        | ns ss -> as
        , ns ts -> bs
        , ss bs -> ts
        , ts as -> ss
  where
    keys' :: Functor f
        => proxy ns
        -> (Product g as -> f (Product g bs))
        -> Product g ss
        -> f (Product g ts)


------------------------------------------------------------------------------
instance Keys '[] ss ss '[] '[] where
    keys' _ f s = const s <$> f Nil


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
    , Keys ns ss ss as as
    , Keys ns ss ss' as as
    , Keys ns ss' ss as as
    , Keys ns ss' ss' as as
    , Keys ns ss' ts as bs
    , Keys ns ts ss' bs as
    , Keys ns ts ts bs bs
    , UpdateKeys ns ss as ~ ss
    , UpdateKeys ns ss' as ~ ss'
    , UpdateKeys ns ss' bs ~ ts
    , UpdateKeys ns ts as ~ ss'
    , UpdateKeys ns ts bs ~ ts
    , UpdateKeys (n ': ns) ss ('(n, b) ': bs) ~ ts
    )
  =>
    Keys (n ': ns) ss ts ('(n, a) ': as) ('(n, b) ': bs)
  where
    keys' _ = lens get set'
      where
        get :: Product g ss -> Product g ('(n, a) ': as)
        get ss = Cons
            (view (key' (Proxy :: Proxy n)) ss)
            (view (keys' (Proxy :: Proxy ns)) ss)
        set' :: Product g ('(n, b) ': bs) -> Product g ss -> Product g ts
        set' (Cons b bs) = set (keys' (Proxy :: Proxy ns)) bs
            . set (key' (Proxy :: Proxy n)) b


------------------------------------------------------------------------------
keys
    :: forall ns ss ts as bs f g. (Keys ns ss ts as bs, Functor f)
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
keys = keys' (Proxy :: Proxy ns)


------------------------------------------------------------------------------
class Keys (FindKeys vs ss) ss ts as bs => Values vs ss ts as bs


------------------------------------------------------------------------------
instance Keys (FindKeys vs ss) ss ts as bs => Values vs ss ts as bs


------------------------------------------------------------------------------
values'
    :: forall vs ss ts as bs g f proxy.
        ( Values vs ss ts as bs
        , Functor f
        )
    => proxy vs
    -> (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
values' _ = keys' (Proxy :: Proxy (FindKeys vs ss))


------------------------------------------------------------------------------
values
    :: forall vs ss ts as bs g f.
        ( Values vs ss ts as bs
        , Functor f
        )
    => (Product g as -> f (Product g bs))
    -> Product g ss
    -> f (Product g ts)
values = values' (Proxy :: Proxy vs)


------------------------------------------------------------------------------
type family MapFst (as :: [(k, v)]) :: [k]
  where
    MapFst '[] = '[]
    MapFst (a ': as) = Fst a ': MapFst as


------------------------------------------------------------------------------
type family MapSnd (as :: [(k, v)]) :: [v]
  where
    MapSnd '[] = '[]
    MapSnd (a ': as) = Snd a ': MapSnd as


------------------------------------------------------------------------------
type family Fst (p :: (a, b)) :: a where
    Fst '(a, b) = a


------------------------------------------------------------------------------
type family Snd (p :: (a, b)) :: b where
    Snd '(a, b) = b


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
