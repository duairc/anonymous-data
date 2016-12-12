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
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Data.Anonymous.Profunctor
    ( ProductAdaptor
    , pProduct
    , pTuple
    , pRecord
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Tuple, Record)
import           Data.Labeled (Field, Labeled (Labeled), Labeled1 (Labeled1))


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
class ProductProfunctor p => ProductAdaptor p f abs as bs
    | f p as bs -> abs
    , f abs -> as
    , f abs -> bs
  where
    pProduct :: Product f abs -> p (Product f as) (Product f bs)


------------------------------------------------------------------------------
instance ProductProfunctor p => ProductAdaptor p f Nil Nil Nil where
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
    , ProductAdaptor p Field abs as bs
    , KnownSymbol s
    )
  =>
    ProductAdaptor p Field
        (Cons (Pair s (p a b)) abs)
        (Cons (Pair s a) as)
        (Cons (Pair s b) bs)
  where
    pProduct (Cons (Labeled (Identity a)) as) = dimap
        (\(Cons (Labeled (Identity a_)) as_) -> (a_, as_))
        (\(a_, as_) -> Cons (Labeled (Identity a_)) as_)
        (a ***! pProduct as)


------------------------------------------------------------------------------
pTuple
    :: ProductAdaptor p Identity abs as bs
    => Tuple abs -> p (Tuple as) (Tuple bs)
pTuple = pProduct


------------------------------------------------------------------------------
pRecord
    :: ProductAdaptor p Field abs as bs
    => Record abs -> p (Record as) (Record bs)
pRecord = pProduct


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
instance (Profunctor p, Default p (f a) (f b), KnownSymbol s) =>
    Default p (Labeled f (Pair s a)) (Labeled f (Pair s b))
  where
    def = dimap (\(Labeled a) -> a) Labeled (def :: p (f a) (f b))


------------------------------------------------------------------------------
instance (Profunctor p, Default p (f a) (f b), KnownSymbol s) =>
    Default p (Labeled1 f s a) (Labeled1 f s b)
  where
    def = dimap (\(Labeled1 (Labeled a)) -> a) (Labeled1 . Labeled)
        (def :: p (f a) (f b))
