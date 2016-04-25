#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#define __OVERLAPPABLE__
#define __OVERLAPPING__
#define __OVERLAPS__
#else
#define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
#define __OVERLAPPING__ {-# OVERLAPPING #-}
#define __OVERLAPS__ {-# OVERLAPS #-}
#endif
