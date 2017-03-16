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
#if MIN_VERSION_aeson(1, 0, 0)
import           Data.Aeson.Types
                     ( FromJSONKey
                     , FromJSONKeyFunction
                        ( FromJSONKeyCoerce
                        , FromJSONKeyText
                        , FromJSONKeyTextParser
                        , FromJSONKeyValue
                        )
                     , Parser
                     , ToJSONKey
                     , ToJSONKeyFunction
                        ( ToJSONKeyText
                        , ToJSONKeyValue
                        )
                     , Value (String)
                     , contramapToJSONKeyFunction
                     , fromJSONKey
                     , fromJSONKeyList
                     , listParser
                     , listValue
                     , toEncoding
                     , toJSONKey
                     , toJSONKeyList
                     )
#endif


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
#if MIN_VERSION_aeson(1, 0, 0)
import           Unsafe.Coerce (unsafeCoerce)
#endif


-- text ----------------------------------------------------------------------
import           Data.Text (Text, pack)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)
import           Type.Meta (Known, Val, val, Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


-- unordered-containers ------------------------------------------------------
import qualified Data.HashMap.Lazy as H


-- vector --------------------------------------------------------------------
import qualified Data.Vector as V


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
#if MIN_VERSION_aeson(1, 0, 0)


------------------------------------------------------------------------------
instance (Known s, Key (Val s), FromJSONKey (f a)) =>
    FromJSONKey (Labeled f (Pair s a))
  where
    fromJSONKey = fmap Labeled fromJSONKey
    fromJSONKeyList = fmap (map Labeled) fromJSONKeyList


------------------------------------------------------------------------------
instance (Known s, Key (Val s), FromJSONKey (f a)) =>
    FromJSONKey (Labeled1 f s a)
  where
    fromJSONKey = fmap (Labeled1 . Labeled) fromJSONKey
    fromJSONKeyList = fmap (map (Labeled1 . Labeled)) fromJSONKeyList


------------------------------------------------------------------------------
instance FromJSONKey (Product f Nil) where
    fromJSONKey = FromJSONKeyValue parseJSON
    fromJSONKeyList = FromJSONKeyValue parseJSON


------------------------------------------------------------------------------
instance
    ( FromJSONKey (f a)
    , FromJSONKey (Product f as)
    )
  =>
    FromJSONKey (Product f (Cons a as))
  where
    fromJSONKey = FromJSONKeyValue parseJSONKeyTuple
    fromJSONKeyList = FromJSONKeyValue (listParser parseJSONKeyTuple)


------------------------------------------------------------------------------
instance __OVERLAPPING__ FromJSONKey a => FromJSONKey (Tuple (Cons a Nil))
  where
    fromJSONKey = fmap (\a -> Cons (Identity a) Nil) fromJSONKey
    fromJSONKeyList = fmap (map (\a -> Cons (Identity a) Nil)) fromJSONKeyList


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Known s
    , Key (Val s)
    , FromJSONKey (f a)
    , FromJSONKey (Product (Labeled f) as)
    )
  =>
    FromJSONKey (Product (Labeled f) (Cons (Pair s a) as))
  where
    fromJSONKey = FromJSONKeyValue parseJSONKeyRecord
    fromJSONKeyList = FromJSONKeyValue (listParser parseJSONKeyRecord)


------------------------------------------------------------------------------
parseJSONKey :: FromJSONKey a => Value -> Parser a
parseJSONKey = case fromJSONKey of
    FromJSONKeyCoerce _ -> \x -> case x of
        String t -> pure (unsafeCoerce t)
        _ -> empty
    FromJSONKeyText f -> \x -> case x of
        String t -> pure (f t)
        _ -> empty
    FromJSONKeyTextParser f -> \x -> case x of
        String t -> f t
        _ -> empty
    FromJSONKeyValue f -> f


------------------------------------------------------------------------------
parseJSONKeyTuple :: (FromJSONKey (f a), FromJSONKey (Product f as))
    => Value -> Parser (Product f (Cons a as))
parseJSONKeyTuple (Array v) | not (V.null v) =
    Cons <$> parseJSONKey (V.head v) <*> parseJSONKey (Array (V.tail v))
parseJSONKeyTuple _ = empty


------------------------------------------------------------------------------
parseJSONKeyRecord
    :: forall s f a as.
        ( Known s
        , Key (Val s)
        , FromJSONKey (f a)
        , FromJSONKey (Product (Labeled f) as)
        )
    => Value -> Parser (Product (Labeled f) (Cons (Pair s a) as))
parseJSONKeyRecord (Array v) | not (V.null v) =
    Cons <$> parseJSONKey (V.head v) <*> parseJSONKey (Array (V.tail v))
parseJSONKeyRecord j@(Object h) = Cons
    <$> (result >>= parseJSONKey >>= pure . Labeled)
    <*> parseJSONKey j
  where
    result :: Parser Value
    result = maybe empty pure (H.lookup (key (Proxy :: Proxy s)) h)
parseJSONKeyRecord _ = empty
#endif


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
#if MIN_VERSION_aeson(1, 0, 0)


------------------------------------------------------------------------------
instance (Known s, Key (Val s), ToJSONKey (f a)) =>
    ToJSONKey (Labeled f (Pair s a))
  where
    toJSONKey = contramapToJSONKeyFunction unlabel toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map unlabel) toJSONKeyList


------------------------------------------------------------------------------
instance (Known s, Key (Val s), ToJSONKey (f a)) =>
    ToJSONKey (Labeled1 f s a)
  where
    toJSONKey = contramapToJSONKeyFunction unlabel1 toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map unlabel1) toJSONKeyList


------------------------------------------------------------------------------
instance __INCOHERENT__ ToJSONKey (Product f Nil) where
    toJSONKey = ToJSONKeyValue toJSON toEncoding
    toJSONKeyList = ToJSONKeyValue toJSON toEncoding


------------------------------------------------------------------------------
instance ToJSONKey (Product (Labeled f) Nil) where
    toJSONKey = ToJSONKeyValue toJSON toEncoding
    toJSONKeyList = ToJSONKeyValue toJSON toEncoding


------------------------------------------------------------------------------
instance (ToJSONKey (f a), ToJSONKey (Product f as)) =>
    ToJSONKey (Product f (Cons a as))
  where
    toJSONKey = toJSONKeyFunction toJSONKeyTuple
    toJSONKeyList = toJSONKeyFunction (listValue toJSONKeyTuple)


------------------------------------------------------------------------------
instance __OVERLAPPING__ ToJSONKey a => ToJSONKey (Tuple (Cons a Nil)) where
    toJSONKey = contramapToJSONKeyFunction unsingleton toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (map unsingleton) toJSONKeyList


------------------------------------------------------------------------------
instance (ToJSONKey a, ToJSONKey b) => ToJSONKey (Tuple (Cons a (Cons b Nil)))
  where
    toJSONKey = toJSONKeyFunction toJSONKeyPair
    toJSONKeyList = toJSONKeyFunction (listValue toJSONKeyPair)


------------------------------------------------------------------------------
instance __OVERLAPPING__
    ( Known s
    , Key (Val s)
    , ToJSON (f a)
    , ToJSON (Product (Labeled f) as)
    , ToJSONKey (f a)
    , ToJSONKey (Product (Labeled f) as)
    )
  =>
    ToJSONKey (Product (Labeled f) (Cons (Pair s a) as))
  where
    toJSONKey = toJSONKeyFunction toJSONKeyRecord
    toJSONKeyList = toJSONKeyFunction (listValue toJSONKeyRecord)


------------------------------------------------------------------------------
unlabel :: Labeled f (Pair s a) -> f a
unlabel (Labeled a) = a


------------------------------------------------------------------------------
unlabel1 :: Labeled1 f s a -> f a
unlabel1 (Labeled1 (Labeled a)) = a


------------------------------------------------------------------------------
unsingleton :: Tuple (Cons a Nil) -> a
unsingleton (Cons (Identity a) Nil) = a


------------------------------------------------------------------------------
toJSONKey_ :: ToJSONKey a => a -> Value
toJSONKey_ = case toJSONKey of 
    ToJSONKeyText a _ -> String . a
    ToJSONKeyValue a _ -> a


------------------------------------------------------------------------------
toJSONKeyFunction :: (a -> Value) -> ToJSONKeyFunction a
toJSONKeyFunction f = ToJSONKeyValue f (toEncoding . f)


------------------------------------------------------------------------------
toJSONKeyPair :: (ToJSONKey a, ToJSONKey b)
    => Tuple (Cons a (Cons b Nil)) -> Value
toJSONKeyPair  (Cons (Identity a) (Cons (Identity b) Nil)) =
    Array (V.cons (toJSONKey_ a) (V.cons (toJSONKey_ b) V.empty))
#if __GLASGOW_HASKELL__ < 800
toJSONKeyPair _ = undefined
#endif


------------------------------------------------------------------------------
toJSONKeyTuple :: (ToJSONKey (f a), ToJSONKey (Product f as))
    => Product f (Cons a as) -> Value
toJSONKeyTuple (Cons a as) = case toJSONKey_ as of
    Array v -> Array (V.cons (toJSONKey_ a) v)
    x -> x


------------------------------------------------------------------------------
toJSONKeyRecord
    :: forall s f a as.
        ( Known s
        , Key (Val s)
        , ToJSONKey (f a)
        , ToJSONKey (Product (Labeled f) as)
        )
    => Product (Labeled f) (Cons (Pair s a) as) -> Value
toJSONKeyRecord (Cons (Labeled a) as) = case toJSONKey_ as of
        Array v -> Array (V.cons (toJSONKey_ a) v)
        Object h -> Object $
            H.insert (key (Proxy :: Proxy s)) (toJSONKey_ a) h
        x -> x
#endif
