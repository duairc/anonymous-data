{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#if MIN_VERSION_base(4, 7, 0)
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif

module Data.Classes.Internal
    ( Bits
        ( (.&.)
        , (.|.)
        , bit
        , bitSize
#if MIN_VERSION_base(4, 7, 0)
        , bitSizeMaybe
#endif
        , clearBit
        , complement
        , complementBit
        , isSigned
#if MIN_VERSION_base(4, 5, 0)
        , popCount
#endif
        , rotate
        , rotateL
        , rotateR
        , setBit
        , shift
        , shiftL
        , shiftR
        , testBit
#if MIN_VERSION_base(4, 5, 0)
        , unsafeShiftL
        , unsafeShiftR
#endif
        , xor
#if MIN_VERSION_base(4, 7, 0)
        , zeroBits
#endif
        )
    , Bounded (maxBound, minBound)
    , Enum
        ( enumFrom
        , enumFromThen
        , enumFromThenTo
        , enumFromTo
        , fromEnum
        , pred
        , succ
        , toEnum
        )
    , Eq ((==), (/=))
#if MIN_VERSION_base(4, 7, 0)
    , FiniteBits
        ( finiteBitSize
#if MIN_VERSION_base(4, 8, 0)
        , countLeadingZeros
        , countTrailingZeros
#endif
        )
#endif
    , Floating
        ( (**)
        , acos
        , acosh
        , asin
        , asinh
        , atan
        , atanh
        , cos
        , cosh
        , exp
        , log
        , logBase
        , pi
        , sin
        , sinh
        , sqrt
        , tan
        , tanh
        )
    , Fractional ((/), fromRational, recip)
    , Integral (div, divMod, mod, quot, quotRem, rem, toInteger)
    , Ix (index, inRange, range, rangeSize)
    , Monoid (mappend, mconcat, mempty)
    , NFData (rnf)
    , Num ((+), (-), (*), abs, fromInteger, negate, signum)
    , Ord
        ( (<)
        , (<=)
        , (>)
        , (>=)
        , compare
        , max
        , min
        )
    , Read (readList, readsPrec)
    , Real (toRational)
    , RealFrac (ceiling, floor, properFraction, round, truncate)
    , RealFloat
        ( atan2
        , decodeFloat
        , encodeFloat
        , exponent
        , floatDigits
        , floatRadix
        , floatRange
        , isDenormalized
        , isIEEE
        , isInfinite
        , isNaN
        , isNegativeZero
        , scaleFloat
        , significand
        )
#if MIN_VERSION_base(4, 9, 0)
    , Semigroup ((<>), sconcat, stimes)
#endif
    , Show (show, showList, showsPrec)
    , Storable
         ( alignment
         , peek
         , peekByteOff
         , peekElemOff
         , poke
         , pokeByteOff
         , pokeElemOff
         , sizeOf
         )
    )
where

#if __GLASGOW_HASKELL__ < 700
-- base ----------------------------------------------------------------------
import           Data.Bits (Bits (..))
import           Data.Ix (Ix (..))
import           Data.Monoid (Monoid (..))
import           Foreign.Storable (Storable (..))
import           Prelude


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData (..))
#else
-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>), empty)
import           Control.Category ((.), id)
import           Control.Monad
                     (
#if __GLASGOW_HASKELL__ < 700
                       (>>)
                     , (>>=)
                     , fail
                     ,
#endif
                       return
                     )
import           Data.Bool (Bool (False, True), (&&), (||), not, otherwise)
import qualified Data.Eq as P ((==))
import           Data.Foldable (foldr)
import           Data.Function (($))
import           Data.Int (Int)
#if MIN_VERSION_base(4, 9, 0)
import           Data.List.NonEmpty (NonEmpty ((:|)))
#endif
#if MIN_VERSION_base(4, 7, 0)
import           Data.Maybe (Maybe)
#endif
import qualified Data.Monoid as P (mappend)
import           Data.Ord (Ordering (EQ, GT, LT))
import qualified Data.Ord as P
                     ( (<)
                     , (>)
#if MIN_VERSION_base(4, 8, 0)
                     , (>=)
#endif
                     , max
                     , min
                     )
import           Data.Tuple (fst, snd)
import           Foreign.Ptr (Ptr, plusPtr)
import           Prelude
                     ( Integer
                     , IO
                     , Rational
                     , ReadS
                     , ShowS
                     , String
#if (MIN_VERSION_base(4, 5, 0) && !MIN_VERSION_base(4, 7, 0)) \
    || MIN_VERSION_base(4, 9, 0)
                     , error
#endif
                     , lex
                     , map
                     , subtract
                     , undefined
                     )
import qualified Prelude as P
                     ( (-)
                     , (+)
#if MIN_VERSION_base(4, 5, 0) && !MIN_VERSION_base(4, 7, 0)
                     , (/=)
#endif
                     , (*)
                     , enumFrom
                     , enumFromThen
                     , enumFromThenTo
                     , enumFromTo
                     , fromInteger
                     , negate
                     )


------------------------------------------------------------------------------
#if MIN_VERSION_base(4, 7, 0)
{-# DEPRECATED bitSize "Use 'bitSizeMaybe' or 'finiteBitSize' instead" #-}
#endif
class
#if MIN_VERSION_base(4, 6, 0)
    Eq a
#elif MIN_VERSION_base(4, 5, 0)
    (Eq a, Num a)
#else
    Num a
#endif
  =>
    Bits a
  where
#ifdef MinimalPragma
    {-# MINIMAL
          (.&.)
        , (.|.)
        , bit
        , bitSize
        , bitSizeMaybe
        , complement
        , isSigned
        , popCount
        , (rotate | (rotateL, rotateR))
        , (shift | (shiftL, shiftR))
        , testBit
        , xor
        #-}
#endif
    (.&.) :: a -> a -> a
    infixl 7 .&.

    (.|.) :: a -> a -> a
    infixl 5 .|.

    bit :: Int -> a
#if !MIN_VERSION_base(4, 6, 0)
    bit = shiftL (fromInteger 1)
    {-# INLINE bit #-}
#endif
    bitSize :: a -> Int
#if MIN_VERSION_base(4, 7, 0)
    bitSizeMaybe :: a -> Maybe Int
#endif
    clearBit :: a -> Int -> a
    clearBit a i = a .&. complement (bit i)
    {-# INLINE clearBit #-}

    complement :: a -> a

    complementBit :: a -> Int -> a
    complementBit a i = xor a (bit i)
    {-# INLINE complementBit #-}

    isSigned :: a -> Bool
#if MIN_VERSION_base(4, 5, 0)
    popCount :: a -> Int
#if !MIN_VERSION_base(4, 6, 0)
    popCount = go (P.fromInteger 0)
      where
        go c w
            | w == fromInteger 0 = c
            | otherwise =
                go (c P.+ P.fromInteger 1) (w .&. w - fromInteger 1)
#endif
#endif
    rotate :: a -> Int -> a
    rotate a i
        | i P.< P.fromInteger 0 = rotateR a (P.negate i)
        | i P.> P.fromInteger 0 = rotateL a i
        | otherwise = a
    infixl 8 `rotate`

    rotateL :: a -> Int -> a
    rotateL a i = rotate a i
    infixl 8 `rotateL`
    {-# INLINE rotateL #-}

    rotateR :: a -> Int -> a
    rotateR a i = rotate a (P.negate i)
    infixl 8 `rotateR`
    {-# INLINE rotateR #-}

    setBit :: a -> Int -> a
    setBit a i = a .|. bit i
    {-# INLINE setBit #-}

    shift :: a -> Int -> a
    shift a i
        | i P.< P.fromInteger 0 = shiftR a (P.negate i)
        | i P.> P.fromInteger 0 = shiftL a i
        | otherwise = a
    infixl 8 `shift`

    shiftL :: a -> Int -> a
    shiftL a i = shift a i
    infixl 8 `shiftL`
    {-# INLINE shiftL #-}

    shiftR :: a -> Int -> a
    shiftR a i = shift a (P.negate i)
    infixl 8 `shiftR`
    {-# INLINE shiftR #-}

    testBit :: a -> Int -> Bool
#if !MIN_VERSION_base(4, 6, 0)
    testBit a i = (a .&. bit i) /= fromInteger 0
    {-# INLINE testBit #-}

#endif
#if MIN_VERSION_base(4, 5, 0)
    unsafeShiftL :: a -> Int -> a
    unsafeShiftL = shiftL
    {-# INLINE unsafeShiftL #-}

    unsafeShiftR :: a -> Int -> a
    unsafeShiftR = shiftR
    {-# INLINE unsafeShiftR #-}

#endif
    xor :: a -> a -> a
    infixl 6 `xor`
#if MIN_VERSION_base(4, 7, 0)

    zeroBits :: a
    zeroBits = clearBit (bit (P.fromInteger 0)) (P.fromInteger 0)
#endif


------------------------------------------------------------------------------
class Bounded a where
    maxBound :: a
    minBound :: a


------------------------------------------------------------------------------
class Enum a where
    enumFrom :: a -> [a]
    enumFrom a = map toEnum $ P.enumFrom (fromEnum a)

    enumFromThen :: a -> a -> [a]
    enumFromThen a b = map toEnum $ P.enumFromThen (fromEnum a) (fromEnum b)

    enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo a b c = map toEnum $
        P.enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c)

    enumFromTo :: a -> a -> [a]
    enumFromTo a b = map toEnum $ P.enumFromTo (fromEnum a) (fromEnum b)

    fromEnum :: a -> Integer

    pred :: a -> a
    pred = toEnum . (subtract 1) . fromEnum

    succ :: a -> a
    succ = toEnum . (P.+ 1)  . fromEnum

    toEnum :: Integer -> a


------------------------------------------------------------------------------
class Eq a where
#ifdef MinimalPragma
    {-# MINIMAL (==) | (/=) #-}
#endif
    (==) :: a -> a -> Bool
    a == b = not (a /= b)
    infix 4 ==
    {-# INLINE (==) #-}

    (/=) :: a -> a -> Bool
    a /= b = not (a == b)
    infix 4 /=
    {-# INLINE (/=) #-}


#if MIN_VERSION_base(4, 7, 0)
------------------------------------------------------------------------------
class Bits a => FiniteBits a where
    finiteBitSize :: a -> Int
#if MIN_VERSION_base(4, 8, 0)

    countLeadingZeros :: a -> Int
    countLeadingZeros x =
        (w P.- P.fromInteger 1) P.- go (w P.- P.fromInteger 1)
      where
        go i
            | i P.< P.fromInteger 0 = i
            | testBit x i = i
            | otherwise = go (i P.- P.fromInteger 1)
        w = finiteBitSize x

    countTrailingZeros :: a -> Int
    countTrailingZeros x = go (P.fromInteger 0)
      where
        go i
            | i P.>= w = i
            | testBit x i = i
            | otherwise = go (i P.+ P.fromInteger 1)
        w = finiteBitSize x
#endif


#endif


------------------------------------------------------------------------------
class Fractional a => Floating a where
    (**) :: a -> a -> a
    a ** b = exp (log a * b)
    infixr 8 **
    {-# INLINE (**) #-}

    acos :: a -> a
    acosh :: a -> a
    asin :: a -> a
    asinh :: a -> a
    atan :: a -> a
    atanh :: a -> a
    cos :: a -> a
    cosh :: a -> a
    exp :: a -> a
    log :: a -> a

    logBase :: a -> a -> a
    logBase a b = log b / log a
    {-# INLINE logBase #-}

    pi :: a

    sin :: a -> a
    sinh :: a -> a

    sqrt :: a -> a
    sqrt = (** fromRational 0.5)
    {-# INLINE sqrt #-}

    tan :: a -> a
    tan a = sin a / cos a
    {-# INLINE tan #-}

    tanh :: a -> a
    tanh a = sinh a / cosh a
    {-# INLINE tanh #-}


------------------------------------------------------------------------------
class Num a => Fractional a where
#ifdef MinimalPragma
    {-# MINIMAL ((/) | recip), fromRational #-}
#endif
    (/) :: a -> a -> a
    x / y = x * recip y
    infixl 7 /
    {-# INLINE (/) #-}

    fromRational :: Rational -> a

    recip :: a -> a
    recip x = fromInteger 1 / x
    {-# INLINE recip #-}


------------------------------------------------------------------------------
class (Real a, Enum a) => Integral a where
    div :: a -> a -> a
    div a b = fst $ divMod a b
    infixl 7 `div`
    {-# INLINE div #-}

    divMod :: a -> a -> (a, a)
    divMod a b = if signum a == negate (signum b)
        then (q - fromInteger 1, r + b)
        else qr
      where
        qr@(q, r) = quotRem a b

    mod :: a -> a -> a
    mod a b = snd $ divMod a b
    infixl 7 `mod`
    {-# INLINE mod #-}

    quot :: a -> a -> a
    quot a b = fst $ quotRem a b
    infixl 7 `quot`
    {-# INLINE quot #-}

    quotRem :: a -> a -> (a, a)

    rem :: a -> a -> a
    rem a b = snd $ quotRem a b
    infixl 7 `rem`
    {-# INLINE rem #-}

    toInteger :: a -> Integer


------------------------------------------------------------------------------
class Ord a => Ix a where
#ifdef MinimalPragma
    {-# MINIMAL index, inRange, range #-}
#endif
    index :: (a, a) -> a -> Integer
    inRange :: (a, a) -> a -> Bool
    range :: (a, a) -> [a]

    rangeSize :: (a, a) -> Integer
    rangeSize b@(_, h)
        | inRange b h = index b h P.+ 1
        | otherwise = 0


------------------------------------------------------------------------------
class Monoid a where
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
    mappend :: a -> a -> a
    mempty :: a


------------------------------------------------------------------------------
class NFData a where
    rnf :: a -> ()


------------------------------------------------------------------------------
class
#if !MIN_VERSION_base(4, 5, 0)
    (Eq a, Show a)
  =>
#endif
    Num a
  where
#ifdef MinimalPragma
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
#endif
    (+) :: a -> a -> a
    infixl 6 +
    (-) :: a -> a -> a
    a - b = a + negate b
    infixl 6 -
    {-# INLINE (-) #-}
    (*) :: a -> a -> a
    infixl 7 *
    abs :: a -> a
    fromInteger :: Integer -> a
    negate :: a -> a
    negate a = fromInteger 0 - a
    {-# INLINE negate #-}
    signum :: a -> a


------------------------------------------------------------------------------
class Eq a => Ord a where
#ifdef MinimalPragma
    {-# MINIMAL (<=) | compare #-}
#endif
    (<) :: a -> a -> Bool
    a < b = case compare a b of
        LT -> True
        _ -> False

    (<=) :: a -> a -> Bool
    a <= b = case compare a b of
        GT -> False
        _ -> True

    (>) :: a -> a -> Bool
    a >  b = case compare a b of
        GT -> True
        _ -> False

    (>=) :: a -> a -> Bool
    a >= b = case compare a b of
        LT -> False
        _ -> True

    compare :: a -> a -> Ordering
    compare a b = if a == b
        then EQ
        else if a <= b
            then LT
            else GT

    max :: a -> a -> a
    max a b = if a <= b then b else a

    min :: a -> a -> a
    min a b = if a <= b then a else b


------------------------------------------------------------------------------
class Read a where
#ifdef MinimalPragma
    {-# MINIMAL readsPrec #-}
#endif
    readsPrec :: Int -> ReadS a
    readList :: ReadS [a]
    readList s = do
        ("[", s') <- lex s
        go id s' <|> do
            ("]", s'') <- lex s'
            return ([], s'')
      where
        go dlist s' = do
            (a, s'') <- readsPrec (P.fromInteger 0) s'
            (c, s''') <- lex s''
            case c of
                "]" -> return (dlist [a], s''')
                "," -> go (dlist . (a :)) s'''
                _ -> empty


------------------------------------------------------------------------------
class (Num a, Ord a) => Real a where
    toRational :: a -> Rational


------------------------------------------------------------------------------
class (RealFrac a, Floating a) => RealFloat a where
    atan2 :: a -> a -> a
    atan2 b a
        | a > fromInteger 0 = atan (b / a)
        | a == fromInteger 0 && b > fromInteger 0 = pi / fromInteger 2
        | a < fromInteger 0 && b > fromInteger 0 = pi + atan (b / a)
        | a <= fromInteger 0 && b < fromInteger 0
            || a < fromInteger 0 && isNegativeZero b
            || isNegativeZero a && isNegativeZero b
          = negate (atan2 (negate b) a)
        | b == fromInteger 0 && (a < fromInteger 0 || isNegativeZero a)
            = pi
        | a == fromInteger 0 && b == fromInteger 0 = b
        | otherwise = a + b

    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a

    exponent :: a -> Int
    exponent a = if m P.== P.fromInteger 0
        then P.fromInteger 0
        else n P.+ floatDigits a
      where
        (m, n) = decodeFloat a

    floatDigits :: a -> Int
    floatRadix :: a -> Integer
    floatRange :: a -> (Int, Int)
    isDenormalized :: a -> Bool
    isIEEE :: a -> Bool
    isInfinite :: a -> Bool
    isNaN :: a -> Bool
    isNegativeZero :: a -> Bool

    scaleFloat :: Int -> a -> a
    scaleFloat k a = encodeFloat m (n P.+ clamp b k)
      where
        (m, n) = decodeFloat a
        (l, h) = floatRange a
        d = floatDigits a
        b = h P.- l P.+ P.fromInteger 4 P.* d
        clamp bd j = P.max (P.negate bd) (P.min bd j)

    significand :: a -> a
    significand a =
        encodeFloat (fst (decodeFloat a)) (P.negate (floatDigits a))


------------------------------------------------------------------------------
class (Real a, Fractional a) => RealFrac a where
    ceiling :: Integral b => a -> b
    ceiling a = if r > fromInteger 0
        then n + fromInteger 1
        else n
      where
        (n, r) = properFraction a

    floor :: Integral b => a -> b
    floor a = if r < fromInteger 0
        then n - fromInteger 1
        else n
      where
        (n, r) = properFraction a

    properFraction :: Integral b => a -> (b, a)

    round :: Integral b => a -> b
    round a = do
        let (n, r) = properFraction a
        let m = if r < fromInteger 0
             then n - fromInteger 1
             else n + fromInteger 1
        case compare (signum (abs r - fromRational 0.5)) (fromInteger 0) of
            LT -> n
            EQ
                | rem n (fromInteger 2) == fromInteger 0 -> n
                | otherwise -> m
            GT -> m

    truncate :: Integral b => a -> b
    truncate = fst . properFraction
    {-# INLINE truncate #-}


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
class Semigroup a where
    (<>) :: a -> a -> a
    infixr 6 <>

    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as
      where
        go b (c : cs) = b <> go c cs
        go b [] = b

    stimes :: Integral b => b -> a -> a
    stimes y0 x0
        | y0 <= fromInteger 0 =
            error "stimes: positive multiplier expected"
        | otherwise = f x0 y0
      where
        f x y
            | rem y (fromInteger 2) == fromInteger 0
                = f (x <> x) (quot y (fromInteger 2))
            | y == fromInteger 1 = x
            | otherwise = g (x <> x) (quot (pred y) (fromInteger 2)) x
        g x y z
            | rem y (fromInteger 2) == fromInteger 0 =
                g (x <> x) (quot y (fromInteger 2)) z
            | y == fromInteger 1 = x <> z
            | otherwise =
                g (x <> x) (quot (pred y) (fromInteger 2)) (x <> z)


#endif
------------------------------------------------------------------------------
class Show a where
#ifdef MinimalPragma
    {-# MINIMAL show | showsPrec #-}
#endif
    show :: a -> String
    show a = showsPrec (P.fromInteger 0) a ""

    showList :: [a] -> ShowS
    showList [] s = P.mappend "[]" s
    showList (a : as) s = '[' : showsPrec (P.fromInteger 0) a (go as)
      where
        go [] = ']' : s
        go (a' : as') = ',' : showsPrec (P.fromInteger 0) a' (go as')

    showsPrec :: Int -> a -> ShowS
    showsPrec _ a s = P.mappend (show a) s


------------------------------------------------------------------------------
class Storable a where
#ifdef MinimalPragma
    {-# MINIMAL
          alignment
        , (peek | peekByteOff | peekElemOff)
        , (poke | pokeByteOff | pokeElemOff)
        , sizeOf
        #-}
#endif
    alignment :: a -> Int

    peek :: Ptr a -> IO a
    peek p = peekElemOff p (P.fromInteger 0)

    peekByteOff :: Ptr b -> Int -> IO a
    peekByteOff p i = peek (plusPtr p i)

    peekElemOff :: Ptr a -> Int -> IO a
    peekElemOff p i = peekByteOff p (i P.* sizeOf (undefined :: a))

    poke :: Ptr a -> a -> IO ()
    poke p = pokeElemOff p (P.fromInteger 0)

    pokeByteOff :: Ptr b -> Int -> a -> IO ()
    pokeByteOff p i = poke (plusPtr p i)

    pokeElemOff :: Ptr a -> Int -> a -> IO ()
    pokeElemOff p i a = pokeByteOff p (i P.* sizeOf a) a

    sizeOf :: a -> Int
#endif
