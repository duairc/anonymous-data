{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

#include "overlap.h"

module Data.Anonymous.Product.Lens
    ( index'
    , index
    , element'
    , element
    , key'
    , key
    , value'
    , value
    , indices'
    , indices
    , elements'
    , elements
    , keys'
    , keys
    , values'
    , values
    )
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product
                     ( index'
                     , index
                     , element'
                     , element
                     , key'
                     , key
                     , value'
                     , value
                     , indices'
                     , indices
                     , elements'
                     , elements
                     , keys'
                     , keys
                     , values'
                     , values
                     )
