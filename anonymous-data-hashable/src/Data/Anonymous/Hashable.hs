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
import           Data.Labeled (Labeled (Labeled), Labeled1 (Labeled1))


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable, hashWithSalt)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
instance Hashable (f a) => Hashable (Labeled f (Pair s a)) where
    hashWithSalt s (Labeled a) = hashWithSalt s a


------------------------------------------------------------------------------
instance Hashable (f a) => Hashable (Labeled1 f s a) where
    hashWithSalt s (Labeled1 a) = hashWithSalt s a


------------------------------------------------------------------------------
instance Hashable (Product g Nil) where
    hashWithSalt s Nil = hashWithSalt s ()


------------------------------------------------------------------------------
instance (Hashable (g a), Hashable (Product g as)) =>
    Hashable (Product g (Cons a as))
  where
    hashWithSalt s (Cons a as) = s `hashWithSalt` a `hashWithSalt` as
