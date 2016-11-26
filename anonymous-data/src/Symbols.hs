{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

module Symbols
    ( AnonymousData
    , Cons
    , DataAnonymousProduct
    , DataAnonymousSum
    , DataField
    , Field
    , Field1
    , Here
    , Nil
    , Product
    , Sum
    , There
    )
where

#ifdef UseTypeLits
------------------------------------------------------------------------------
type AnonymousData = "anonymous-data"
type Cons = "Cons"
type DataAnonymousProduct = "Data.Anonymous.Product"
type DataAnonymousSum = "Data.Anonymous.Sum"
type DataField = "Data.Field"
type Field = "Field"
type Field1 = "Field1"
type Here = "Here"
type Nil = "Nil"
type Product = "Product"
type Sum = "Sum"
type There = "There"
#else
-- types ---------------------------------------------------------------------
import           Type.Bool (True, False)
import           Type.Char (Char)
import qualified Type.List as L (Nil, Cons)
import           Type.String (String)


------------------------------------------------------------------------------
type I = True
type O = False
type a :+ as = L.Cons a as
infixr 5 :+


------------------------------------------------------------------------------
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
    L.Nil)
type Cons = String (-- $("Cons")
    Char I I O O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
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
    L.Nil)
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
    L.Nil)
type DataField = String (-- $("Data.Field")
    Char O O I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I I O I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Field = String (-- $("Field")
    Char O I I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Field1 = String (-- $("Field1")
    Char O I I O O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Here = String (-- $("Here")
    Char O O O I O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Nil = String (-- $("Nil")
    Char O I I I O O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Product = String (-- $("Product")
    Char O O O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I I O O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type Sum = String (-- $("Sum")
    Char I I O O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
type There = String (-- $("There")
    Char O O I O I O I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O O O I O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char O I O O I I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    Char I O I O O I I O O O O O O O O O O O O O O O O O O O O O O O O O :+
    L.Nil)
#endif
