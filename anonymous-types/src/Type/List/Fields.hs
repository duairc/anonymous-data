{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

module Type.List.Fields
    ( LookupIndex
    , FindIndex
    , UpdateIndex
    , LookupKey
    , FindKey
    , UpdateKey
    , LookupIndices
    , FindIndices
    , UpdateIndices
    , LookupKeys
    , FindKeys
    , UpdateKeys
    )
where

-- base ----------------------------------------------------------------------
import           GHC.TypeLits (Nat, type (+), type (-))


------------------------------------------------------------------------------
type family LookupIndex (n :: Nat) (as :: [v]) :: v where
    LookupIndex 0 (a ': as) = a
    LookupIndex n (a ': as) = LookupIndex (n - 1) as


------------------------------------------------------------------------------
type family FindIndex (n :: v) (as :: [v]) :: Nat where
    FindIndex v (v ': as) = 0
    FindIndex v (a ': as) = 1 + FindIndex v as


------------------------------------------------------------------------------
type family UpdateIndex (n :: Nat) (as :: [v]) (b :: v) :: [v] where
    UpdateIndex 0 (a ': as) b = b ': as
    UpdateIndex n (a ': as) b = a ': (UpdateIndex (n - 1) as b)


------------------------------------------------------------------------------
type family LookupIndices (ns :: [Nat]) (as :: [v]) :: [v] where
    LookupIndices '[] as = '[]
    LookupIndices (n ': ns) as = LookupIndex n as ': LookupIndices ns as


------------------------------------------------------------------------------
type family FindIndices (ns :: [v]) (as :: [v]) :: [Nat] where
    FindIndices '[] as = '[]
    FindIndices (v ': vs) as = FindIndex v as ': FindIndices vs as


------------------------------------------------------------------------------
type family UpdateIndices (ns :: [Nat]) (as :: [k]) (bs :: [k]) :: [k] where
    UpdateIndices '[] as '[] = as
    UpdateIndices '[] as bs = as
    UpdateIndices ns as '[] = as
    UpdateIndices (n ': ns) as (b ': bs) =
        UpdateIndices ns (UpdateIndex n as b) bs


------------------------------------------------------------------------------
type family LookupKey (n :: k) (as :: [(k, v)]) :: v where
    LookupKey k (p k v ': as) = v
    LookupKey k (a ': as) = LookupKey k as


------------------------------------------------------------------------------
type family FindKey (n :: v) (as :: [(k, v)]) :: k where
    FindKey v (p k v ': as) = k
    FindKey v (a ': as) = FindKey v as


------------------------------------------------------------------------------
type family UpdateKey (n :: k) (as :: [(k, v)]) (b :: v) :: [(k, v)] where
    UpdateKey k (p k v ': as) v' = p k v' ': as
    UpdateKey k (a ': as) v' = a ': UpdateKey k as v'


------------------------------------------------------------------------------
type family LookupKeys (ns :: [k]) (as :: [(k, v)]) :: [v] where
    LookupKeys '[] as = '[]
    LookupKeys (k ': ks) as = LookupKey k as ': LookupKeys ks as


------------------------------------------------------------------------------
type family FindKeys (ns :: [v]) (as :: [(k, v)]) :: [k] where
    FindKeys '[] as = '[]
    FindKeys (v ': vs) as = FindKey v as ': FindKeys vs as


------------------------------------------------------------------------------
type family UpdateKeys (as :: [(k, v)]) (bs :: [(k, v)]) :: [(k, v)]
  where
    UpdateKeys '[] bs = '[]
    UpdateKeys as '[] = as
    UpdateKeys as ('(n, b) ': bs) = UpdateKeys (UpdateKey n as b) bs
