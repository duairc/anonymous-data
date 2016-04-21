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

-- anonymous-types -----------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil))
import           Data.Uncurry (Uncurry (Uncurry))
import           Data.Field (Field (Field))


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable, hashWithSalt)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
instance Hashable a => Hashable (Field s a) where
    hashWithSalt s (Field a) = hashWithSalt s a


------------------------------------------------------------------------------
instance Hashable (f a b) => Hashable (Uncurry f (Pair a b)) where
    hashWithSalt s (Uncurry f) = hashWithSalt s f


------------------------------------------------------------------------------
instance Hashable (Product g Nil) where
    hashWithSalt s Nil = hashWithSalt s ()


------------------------------------------------------------------------------
instance (Hashable (g a), Hashable (Product g as)) =>
    Hashable (Product g (Cons a as))
  where
    hashWithSalt s (Cons a as) = s `hashWithSalt` a `hashWithSalt` as
