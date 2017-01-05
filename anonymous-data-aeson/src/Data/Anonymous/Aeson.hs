{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
                     , pure
                     ,
#endif
                       empty
                     , (<|>)
                     )
import           Data.Functor.Identity (Identity (Identity))


-- text ----------------------------------------------------------------------
import           Data.Text (Text, pack)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)
import           Type.Meta (Known, Val, val, Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


-- unordered-containers ------------------------------------------------------
import qualified Data.HashMap.Lazy as H (insert)


-- vector --------------------------------------------------------------------
import qualified Data.Vector as V (cons, empty, head, null, tail)


#if __GLASGOW_HASKELL__ < 700
#define Key String ~ 
key :: (Known s, Val s ~ String) => Proxy s -> Text
key = pack . val
#else
------------------------------------------------------------------------------
class Key a where
    key :: (Known s, Val s ~ a) => Proxy s -> Text


------------------------------------------------------------------------------
instance Key String where
    key = pack . val


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ Show a => Key a where
    key = pack . show . val


#endif
------------------------------------------------------------------------------
instance (Known s, Key (Val s), FromJSON (f a)) =>
    FromJSON (Labeled f (Pair s a))
  where
    parseJSON (Object h) =
        Labeled <$> h .: key (Proxy :: Proxy s)
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (Known s, Key (Val s), FromJSON (f a)) => FromJSON (Labeled1 f s a)
  where
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
    (Known s, Key (Val s), FromJSON (f a), FromJSON (Product (Labeled f) as))
  =>
    FromJSON (Product (Labeled f) (Cons (Pair s a) as))
  where
    parseJSON (Array v) | not (V.null v) =
        Cons <$> parseJSON (V.head v) <*> parseJSON (Array (V.tail v))
    parseJSON j@(Object h) = do
        Cons <$> fmap Labeled (h .: key (Proxy :: Proxy s)) <*> parseJSON j
    parseJSON _ = empty


------------------------------------------------------------------------------
instance (ToJSON (f a), Key (Val s)) => ToJSON (Labeled f (Pair s a)) where
    toJSON (Labeled a) = object [key (Proxy :: Proxy s) .= a]


------------------------------------------------------------------------------
instance (ToJSON (f a), Key (Val s)) => ToJSON (Labeled1 f s a) where
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
#if __GLASGOW_HASKELL__ < 800
    toJSON _ = undefined
#endif


------------------------------------------------------------------------------
instance (ToJSON a, ToJSON b) => ToJSON (Tuple (Cons a (Cons b Nil))) where
    toJSON (Cons (Identity a) (Cons (Identity b) Nil)) =
        Array (V.cons (toJSON a) (V.cons (toJSON b) V.empty))
#if __GLASGOW_HASKELL__ < 800
    toJSON _ = undefined
#endif


------------------------------------------------------------------------------
instance (Known s, Key (Val s), ToJSON (f a), ToJSON (Product (Labeled f) as))
  =>
    ToJSON (Product (Labeled f) (Cons (Pair s a) as))
  where
    toJSON (Cons (Labeled a) as) = case toJSON as of
        Array v -> Array (V.cons (toJSON a) v)
        Object h -> Object $
            H.insert (key (Proxy :: Proxy s)) (toJSON a) h
        x -> x
