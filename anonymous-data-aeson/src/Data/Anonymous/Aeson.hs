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
#include "incoherent.h"

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
import           Data.Anonymous.Product
                     ( Product (Cons, Nil)
                     , Tuple
                     , Record
                     , Options
                     )
import           Data.Field (Field (Field), Option (Option))


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
instance (KnownSymbol s, FromJSON a) => FromJSON (Field (Pair s a)) where
    parseJSON (Object h) = Field <$> h .: pack (symbolVal (Proxy :: Proxy s))
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (KnownSymbol s, FromJSON a) => FromJSON (Option (Pair s a)) where
    parseJSON (Object h) = Option <$> h .: pack (symbolVal (Proxy :: Proxy s))
    parseJSON _ = empty


------------------------------------------------------------------------------
instance FromJSON (Product g Nil) where
    parseJSON (Array v) | V.null v = pure Nil
    parseJSON (Object _) = pure Nil
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (FromJSON (g a), FromJSON (Product g as)) =>
    FromJSON (Product g (Cons a as))
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
instance __OVERLAPPING__ (KnownSymbol s, FromJSON a, FromJSON (Record as)) =>
    FromJSON (Record (Cons (Pair s a) as))
  where
    parseJSON (Array v) | not (V.null v) =
        Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
    parseJSON j@(Object h) = do
        Cons <$> fmap Field (h .: pack (symbolVal (Proxy :: Proxy s)))
            <*> parseJSON j
    parseJSON _ = empty


------------------------------------------------------------------------------
instance __OVERLAPPING__ (KnownSymbol s, FromJSON a, FromJSON (Options as)) =>
    FromJSON (Options (Cons (Pair s a) as))
  where
    parseJSON (Array v) | not (V.null v) =
        Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
    parseJSON j@(Object h) = do
        Cons <$> fmap Option (h .: pack (symbolVal (Proxy :: Proxy s)))
            <*> parseJSON j
    parseJSON _ = empty


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Field (Pair s a)) where
    toJSON (Field a) = object [pack (symbolVal (Proxy :: Proxy s)) .= a]


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Option (Pair s a)) where
    toJSON (Option a) = object [pack (symbolVal (Proxy :: Proxy s)) .= a]


------------------------------------------------------------------------------
instance __INCOHERENT__ ToJSON (Product g Nil) where
    toJSON Nil = toJSON ()


------------------------------------------------------------------------------
instance ToJSON (Record Nil) where
    toJSON Nil = object []


------------------------------------------------------------------------------
instance __INCOHERENT__ (ToJSON (g a), ToJSON (Product g as)) =>
    ToJSON (Product g (Cons a as))
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
instance ToJSON (Options Nil) where
    toJSON Nil = object []


------------------------------------------------------------------------------
instance (KnownSymbol s, ToJSON a, ToJSON (Record as)) =>
    ToJSON (Record (Cons (Pair s a) as))
  where
    toJSON (Cons (Field a) as) = case toJSON as of
        Array v -> Array (V.cons (toJSON a) v)
        Object h -> Object $
            H.insert (pack (symbolVal (Proxy :: Proxy s))) (toJSON a) h
        x -> x


------------------------------------------------------------------------------
instance (KnownSymbol s, ToJSON a, ToJSON (Options as)) =>
    ToJSON (Options (Cons (Pair s a) as))
  where
    toJSON (Cons (Option a) as) = case toJSON as of
        Array v -> Array (V.cons (toJSON a) v)
        Object h -> Object $
            H.insert (pack (symbolVal (Proxy :: Proxy s))) (toJSON a) h
        x -> x
