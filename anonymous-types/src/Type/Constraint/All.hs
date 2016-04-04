{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Type.Constraint.All
    ( All
    , AllF
    )
where

-- base ----------------------------------------------------------------------
import           GHC.Exts (Constraint)


------------------------------------------------------------------------------
type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
    All c '[] = ()
    All c (a ': as) = (c a, All c as)


------------------------------------------------------------------------------
type family AllF (c :: j -> Constraint) (f :: k -> j) (as :: [k])
    :: Constraint
  where
    AllF c f '[] = ()
    AllF c f (a ': as) = (c (f a), All c as)
