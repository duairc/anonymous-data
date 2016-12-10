{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Data.Anonymous.Hashable
    ()
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil))
import           Data.Field (Field (Field), Option (Option))


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable, hashWithSalt)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
instance Hashable a => Hashable (Field (Pair s a)) where
    hashWithSalt s (Field a) = hashWithSalt s a


------------------------------------------------------------------------------
instance Hashable a => Hashable (Option (Pair s a)) where
    hashWithSalt s (Option a) = hashWithSalt s a


------------------------------------------------------------------------------
instance Hashable (Product g Nil) where
    hashWithSalt s Nil = hashWithSalt s ()


------------------------------------------------------------------------------
instance (Hashable (g a), Hashable (Product g as)) =>
    Hashable (Product g (Cons a as))
  where
    hashWithSalt s (Cons a as) = s `hashWithSalt` a `hashWithSalt` as
