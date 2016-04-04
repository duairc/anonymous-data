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
{-# LANGUAGE Safe #-}

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
import           Control.Applicative (Const (Const))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Proxy (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, symbolVal)


-- product-profunctors -------------------------------------------------------
import           Data.Profunctor.Product (ProductProfunctor, (***!), empty)
import           Data.Profunctor.Product.Default (Default (def))


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


------------------------------------------------------------------------------
class ProductProfunctor p => ProductAdaptor p (g :: k -> *)
        (abs :: [k])
        (as :: [k])
        (bs :: [k])
            | g p as bs -> abs
            , g abs -> as
            , g abs -> bs
  where
    pProduct :: Product g abs -> p (Product g as) (Product g bs)


------------------------------------------------------------------------------
instance ProductProfunctor p => ProductAdaptor p g '[] '[] '[] where
    pProduct Nil = dimap (\Nil -> ()) (\() -> Nil) empty


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , ProductAdaptor p Identity abs as bs
    )
  =>
    ProductAdaptor p Identity (p a b ': abs) (a ': as) (b ': bs)
  where
    pProduct (Cons (Identity a) as) =
        dimap
            (\(Cons (Identity a_) as_) -> (a_, as_))
            (\(a_, as_) -> Cons (Identity a_) as_)
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


{-
------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , ProductAdaptor p (Const c) abs as bs
    )
  =>
    ProductAdaptor p (Const c) (p a b ': abs) (a ': as) (b ': bs)
  where
    pProduct (Cons (Const a) as) = dimap
            (\(Cons (Const a_) as_) -> (a_, as_))
            (\(a_, as_) -> Cons (Const a_) as_)
            (def ***! pProduct as)
-}


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , ProductAdaptor p (Uncurry Field) abs as bs
    , KnownSymbol s
    )
  =>
    ProductAdaptor p (Uncurry Field)
        ('(s, p a b) ': abs)
        ('(s, a) ': as)
        ('(s, b) ': bs)
  where
    pProduct (Cons (Uncurry (Field a)) as) =
        dimap
            (\(Cons (Uncurry (Field a_)) as_) -> (a_, as_))
            (\(a_, as_) -> Cons (Uncurry (Field a_)) as_)
            (a ***! pProduct as)


------------------------------------------------------------------------------
instance ProductProfunctor p => Default p (Product f '[]) (Product g '[])
  where
    def = dimap (\Nil -> ()) (\() -> Nil) empty


------------------------------------------------------------------------------
instance
    ( ProductProfunctor p
    , Default p (f a) (g b)
    , Default p (Product f as) (Product g bs)
    )
  =>
    Default p (Product f (a ': as)) (Product g (b ': bs))
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
    Default p (Uncurry f '(a, b)) (Uncurry g '(c, d))
  where
    def = dimap (\(Uncurry f) -> f) Uncurry (def :: p (f a b) (g c d))
