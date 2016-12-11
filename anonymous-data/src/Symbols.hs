{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Symbols
    ( Empty_
    , AnonymousData
    , DataAnonymousProduct
    , DataAnonymousSum
    , DataLabeled
    , Labeled
    , Labeled1
    , Product
    , Sum
    )
where

#ifdef UseTypeLits
------------------------------------------------------------------------------
type Empty_ = ""
type AnonymousData = "anonymous-data"
type DataAnonymousProduct = "Data.Anonymous.Product"
type DataAnonymousSum = "Data.Anonymous.Sum"
type DataLabeled = "Data.Labeled"
type Labeled = "Labeled"
type Labeled1 = "Labeled1"
type Product = "Product"
type Sum = "Sum"
#else
-- types ---------------------------------------------------------------------
import           Type.Bool (True, False)
import           Type.Char (Char)
import           Type.List (Nil, Cons)
import           Type.String (String)


------------------------------------------------------------------------------
type I = True
type O = False
type a :+ as = Cons a as
infixr 5 :+


------------------------------------------------------------------------------
type Empty_ = String Nil
type AnonymousData = String (-- $("anonymous-data")
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataAnonymousProduct = String (-- $("Data.Anonymous.Product")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataAnonymousSum = String (-- $("Data.Anonymous.Sum")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type DataLabeled = String (-- $("Data.Labeled")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Labeled = String (-- $("Labeled")
    Char O O I I O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Labeled1 = String (-- $("Labeled1")
    Char O O I I O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Product = String (-- $("Product")
    Char O O O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
type Sum = String (-- $("Sum")
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Nil)
#endif
