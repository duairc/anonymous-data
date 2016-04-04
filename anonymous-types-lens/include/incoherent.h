#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE IncoherentInstances #-}
#define __INCOHERENT__
#else
#define __INCOHERENT__ {-# INCOHERENT #-}
#endif
