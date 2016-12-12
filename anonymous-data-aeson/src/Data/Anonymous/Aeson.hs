{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#include "overlap.h"

module Data.Anonymous.Aeson
    ()
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON
                     , ToJSON
                     , Value (Array, Object)
                     , (.:)
                     , (.=)
                     , object
                     , parseJSON
                     , toJSON
                     )


-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Tuple)
import           Data.Labeled (Labeled (Labeled), Labeled1 (Labeled1))


-- base ----------------------------------------------------------------------
import           Control.Applicative
                     (
#if !MIN_VERSION_base(4, 8, 0)
                       (<$>)
                     , (<*>)
                     ,
#endif
                       empty
                     , (<|>)
                     , pure
                     )
import           Data.Functor.Identity (Identity (Identity))


-- text ----------------------------------------------------------------------
import           Data.Text (pack)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat (KnownSymbol, symbolVal)
import           Type.List (Cons, Nil)
import           Type.Meta (Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


-- unordered-containers ------------------------------------------------------
import qualified Data.HashMap.Lazy as H (insert)


-- vector --------------------------------------------------------------------
import qualified Data.Vector as V (cons, empty, head, null, tail)


------------------------------------------------------------------------------
instance (KnownSymbol s, FromJSON (f a)) => FromJSON (Labeled f (Pair s a))
  where
    parseJSON (Object h) =
        Labeled <$> h .: pack (symbolVal (Proxy :: Proxy s))
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (KnownSymbol s, FromJSON (f a)) => FromJSON (Labeled1 f s a) where
    parseJSON = fmap Labeled1 . parseJSON


------------------------------------------------------------------------------
instance FromJSON (Product f Nil) where
    parseJSON (Array v) | V.null v = pure Nil
    parseJSON (Object _) = pure Nil
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (FromJSON (f a), FromJSON (Product f as)) =>
    FromJSON (Product f (Cons a as))
  where
    parseJSON (Array v) | not (V.null v) =
        Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
    parseJSON _ = empty


------------------------------------------------------------------------------
instance __OVERLAPPING__ FromJSON a => FromJSON (Tuple (Cons a Nil)) where
    parseJSON a = Cons . Identity <$> parseJSON a <*> pure Nil <|> case a of
        Array v | not (V.null v) -> do
            Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
        _ -> empty


------------------------------------------------------------------------------
instance __OVERLAPPING__
    (KnownSymbol s, FromJSON (f a), FromJSON (Product (Labeled f) as))
  =>
    FromJSON (Product (Labeled f) (Cons (Pair s a) as))
  where
    parseJSON (Array v) | not (V.null v) =
        Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
    parseJSON j@(Object h) = do
        Cons <$> fmap Labeled (h .: pack (symbolVal (Proxy :: Proxy s)))
            <*> parseJSON j
    parseJSON _ = empty


------------------------------------------------------------------------------
instance ToJSON (f a) => ToJSON (Labeled f (Pair s a)) where
    toJSON (Labeled a) = object [pack (symbolVal (Proxy :: Proxy s)) .= a]


------------------------------------------------------------------------------
instance ToJSON (f a) => ToJSON (Labeled1 f s a) where
    toJSON (Labeled1 a) = toJSON a


------------------------------------------------------------------------------
instance __INCOHERENT__ ToJSON (Product f Nil) where
    toJSON Nil = toJSON ()


------------------------------------------------------------------------------
instance ToJSON (Product (Labeled f) Nil) where
    toJSON Nil = object []


------------------------------------------------------------------------------
instance __INCOHERENT__ (ToJSON (f a), ToJSON (Product f as)) =>
    ToJSON (Product f (Cons a as))
  where
    toJSON (Cons a as) = case toJSON as of
        Array v -> Array (V.cons (toJSON a) v)
        x -> x


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Tuple (Cons a Nil)) where
    toJSON (Cons (Identity a) Nil) = toJSON a


------------------------------------------------------------------------------
instance (ToJSON a, ToJSON b) => ToJSON (Tuple (Cons a (Cons b Nil))) where
    toJSON (Cons (Identity a) (Cons (Identity b) Nil)) =
        Array (V.cons (toJSON a) (V.cons (toJSON b) V.empty))


------------------------------------------------------------------------------
instance (KnownSymbol s, ToJSON (f a), ToJSON (Product (Labeled f) as)) =>
    ToJSON (Product (Labeled f) (Cons (Pair s a) as))
  where
    toJSON (Cons (Labeled a) as) = case toJSON as of
        Array v -> Array (V.cons (toJSON a) v)
        Object h -> Object $
            H.insert (pack (symbolVal (Proxy :: Proxy s))) (toJSON a) h
        x -> x
