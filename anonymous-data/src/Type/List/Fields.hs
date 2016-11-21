{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4, 8, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module Type.List.Fields
    ( LookupIndex
    , UpdateIndex
    , LookupElement
    , UpdateElement
    , LookupKey
    , UpdateKey
    , LookupValue
    , UpdateValue
    , LookupIndices
    , UpdateIndices
    , LookupElements
    , UpdateElements
    , LookupKeys
    , UpdateKeys
    , LookupValues
    , UpdateValues
    )
where

-- base ----------------------------------------------------------------------
import           GHC.TypeLits (Nat, type (-))


------------------------------------------------------------------------------
type family LookupIndex (n :: Nat) (as :: [v]) :: v where
    LookupIndex 0 (a ': as) = a
    LookupIndex n (a ': as) = LookupIndex (n - 1) as


------------------------------------------------------------------------------
type family UpdateIndex (n :: Nat) (b :: v) (as :: [v]) :: [v] where
    UpdateIndex 0 b (a ': as) = b ': as
    UpdateIndex n b (a ': as) = a ': (UpdateIndex (n - 1) b as)


------------------------------------------------------------------------------
type family LookupElement (n :: v) (as :: [v]) :: v where
    LookupElement v (v ': as) = v
    LookupElement v (a ': as) = LookupElement v as


------------------------------------------------------------------------------
type family UpdateElement (a :: v) (as :: [v]) :: [v] where
    UpdateElement a (a ': as) = a ': as
    UpdateElement a (b ': as) = b ': UpdateElement a as


------------------------------------------------------------------------------
type family LookupKey (n :: k) (as :: [(k, v)]) :: v where
    LookupKey k ('(k, v) ': as) = v
    LookupKey k (a ': as) = LookupKey k as


------------------------------------------------------------------------------
type family UpdateKey (n :: k) (b :: v) (as :: [(k, v)]) :: [(k, v)] where
    UpdateKey k v' ('(k, v) ': as) = '(k, v') ': as
    UpdateKey k v' (a ': as) = a ': UpdateKey k v' as


------------------------------------------------------------------------------
type family LookupValue (n :: v) (as :: [(k, v)]) :: k where
    LookupValue v ('(k, v) ': as) = k
    LookupValue v (a ': as) = LookupValue v as


------------------------------------------------------------------------------
type family UpdateValue (n :: v) (b :: k) (as :: [(k, v)]) :: [(k, v)] where
    UpdateValue v k' ('(k, v) ': as) = '(k', v) ': as
    UpdateValue v k' (a ': as) = a ': UpdateValue k' v as


------------------------------------------------------------------------------
type family LookupIndices (ns :: [Nat]) (as :: [v]) :: [v] where
    LookupIndices '[] as = '[]
    LookupIndices (n ': ns) as = LookupIndex n as ': LookupIndices ns as


------------------------------------------------------------------------------
type family UpdateIndices (ns :: [Nat]) (bs :: [k]) (as :: [k]) :: [k] where
    UpdateIndices '[] '[] as = as
    UpdateIndices (n ': ns) (b ': bs) as =
        UpdateIndices ns bs (UpdateIndex n b as)


------------------------------------------------------------------------------
type family LookupElements (n :: [v]) (as :: [v]) :: [v] where
    LookupElements '[] as = '[]
    LookupElements (v ': vs) as = LookupElement v as ': LookupElements vs as


------------------------------------------------------------------------------
type family UpdateElements (bs :: [v]) (as :: [b]) :: [v] where
    UpdateElements '[] as = as
    UpdateElements (b ': bs) as = UpdateElements bs (UpdateElement b as)


------------------------------------------------------------------------------
type family LookupKeys (ns :: [k]) (as :: [(k, v)]) :: [(k, v)] where
    LookupKeys '[] as = '[]
    LookupKeys (k ': ks) as = '(k, LookupKey k as) ': LookupKeys ks as


------------------------------------------------------------------------------
type family UpdateKeys (bs :: [(k, v)]) (as :: [(k, v)]) :: [(k, v)]
  where
    UpdateKeys '[] as = as
    UpdateKeys ( '(n, b) ': bs) as = UpdateKeys bs (UpdateKey n b as)


------------------------------------------------------------------------------
type family LookupValues (ns :: [v]) (as :: [(k, v)]) :: [(k, v)] where
    LookupValues '[] as = '[]
    LookupValues (v ': vs) as = '(LookupValue v as, v) ': LookupValues vs as


------------------------------------------------------------------------------
type family UpdateValues (bs :: [(k, v)]) (as :: [(k, v)]) :: [(k, v)] where
    UpdateValues '[] as = as
    UpdateValues ( '(b, n) ': bs) as = UpdateValues bs (UpdateValue n b as)
