{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Data.Anonymous.Profunctor
    ( ProductAdaptor
    , pProduct
    , pRecord
    , pTuple
    )
where

-- anonymous-types -----------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Record, Tuple)
import           Data.Uncurry (Uncurry (Uncurry))
import           Data.Field (Field (Field))


-- base ----------------------------------------------------------------------
import           Data.Functor.Identity (Identity (Identity))


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default (def))


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat (KnownSymbol)
import           Type.List (Cons, Nil)
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
class ProductProfunctor p => ProductAdaptor p g abs as bs
    | g p as bs -> abs
    , g abs -> as
    , g abs -> bs
  where
    pProduct :: Product g abs -> p (Product g as) (Product g bs)


------------------------------------------------------------------------------
instance ProductProfunctor p => ProductAdaptor p g Nil Nil Nil where
    pProduct Nil = dimap (\Nil -> ()) (\() -> Nil) empty


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , ProductAdaptor p Identity abs as bs
    )
  =>
    ProductAdaptor p Identity (Cons (p a b) abs) (Cons a as) (Cons b bs)
  where
    pProduct (Cons (Identity a) as) = dimap
        (\(Cons (Identity a_) as_) -> (a_, as_))
        (\(a_, as_) -> Cons (Identity a_) as_)
        (a ***! pProduct as)


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , ProductAdaptor p (Uncurry Field) abs as bs
    , KnownSymbol s
    )
  =>
    ProductAdaptor p (Uncurry Field)
        (Cons (Pair s (p a b)) abs)
        (Cons (Pair s a) as)
        (Cons (Pair s b) bs)
  where
    pProduct (Cons (Uncurry (Field a)) as) = dimap
        (\(Cons (Uncurry (Field a_)) as_) -> (a_, as_))
        (\(a_, as_) -> Cons (Uncurry (Field a_)) as_)
        (a ***! pProduct as)


------------------------------------------------------------------------------
pRecord
    :: ProductAdaptor p (Uncurry Field) abs as bs
    => Record abs -> p (Record as) (Record bs)
pRecord = pProduct


------------------------------------------------------------------------------
pTuple
    :: ProductAdaptor p Identity abs as bs
    => Tuple abs -> p (Tuple as) (Tuple bs)
pTuple = pProduct


------------------------------------------------------------------------------
instance ProductProfunctor p => Default p (Product f Nil) (Product g Nil)
  where
    def = dimap (\Nil -> ()) (\() -> Nil) empty


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , Default p (f a) (g b)
    , Default p (Product f as) (Product g bs)
    )
  =>
    Default p (Product f (Cons a as)) (Product g (Cons b bs))
  where
    def = dimap (\(Cons a as) -> (a, as)) (uncurry Cons)
        ((***!)
            (def :: p (f a) (g b))
            (def :: p (Product f as) (Product g bs)))


------------------------------------------------------------------------------
instance (Profunctor p, Default p a b, KnownSymbol s) =>
    Default p (Field s a) (Field s b)
  where
    def = dimap (\(Field a) -> a) Field def


------------------------------------------------------------------------------
instance (Profunctor p, Default p (f a b) (g c d)) =>
    Default p (Uncurry f (Pair a b)) (Uncurry g (Pair c d))
  where
    def = dimap (\(Uncurry f) -> f) Uncurry (def :: p (f a b) (g c d))
