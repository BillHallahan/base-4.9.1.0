{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  GHC.Num
-- -- Copyright   :  (c) The University of Glasgow 1994-2002
-- -- License     :  see libraries/base/LICENSE
-- --
-- -- Maintainer  :  cvs-ghc@haskell.org
-- -- Stability   :  internal
-- -- Portability :  non-portable (GHC Extensions)
-- --
-- -- The 'Num' class and the 'Integer' type.
-- --
-- -----------------------------------------------------------------------------
-- 
-- module GHC.Num (module GHC.Num, module GHC.Integer) where
module GHC.Num (module GHC.Num, module GHC.Integer2) where
-- 
import GHC.Base
-- import GHC.Integer
import GHC.Integer2
import GHC.Prim2
-- 
infixl 7  *
infixl 6  +, -
-- 
default ()              -- Double isn't available yet,
--                         -- and we shouldn't be using defaults anyway
-- 
-- -- | Basic numeric class.
class  Num a  where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-- 
    (+), (-), (*)       :: a -> a -> a
--     -- | Unary negation.
    negate              :: a -> a
--     -- | Absolute value.
    abs                 :: a -> a
--     -- | Sign of a number.
--     -- The functions 'abs' and 'signum' should satisfy the law:
--     --
--     -- > abs x * signum x == x
--     --
--     -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
--     -- or @1@ (positive).
    signum              :: a -> a
--     -- | Conversion from an 'Integer'.
--     -- An integer literal represents the application of the function
--     -- 'fromInteger' to the appropriate value of type 'Integer',
--     -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a
-- 
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y               = x + negate y
    negate x            = (fromInteger zeroInteger) - x
-- 
-- -- | the same as @'flip' ('-')@.
-- --
-- -- Because @-@ is treated specially in the Haskell grammar,
-- -- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- -- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x
-- 
instance  Num Int  where
--     I# x + I# y = I# (x +# y)
    (+) = plusInt
--     I# x - I# y = I# (x -# y)
    (-) = minusInt
--     negate (I# x) = I# (negateInt# x)
    negate = negateInt
--     I# x * I# y = I# (x *# y)
    (*) = timesInt
--     abs n  = if n `geInt` 0 then n else negate n
    abs = absInt
-- 
--     signum n | n `ltInt` 0 = negate 1
--              | n `eqInt` 0 = 0
--              | otherwise   = 1
    signum = signumInt
-- 
--     {-# INLINE fromInteger #-}   -- Just to be sure!
--     fromInteger i = I# (integerToInt i)
    fromInteger = fromIntegerInt

instance Num Word where
    (W# x#) + (W# y#)      = W# (x# `plusWord#` y#)
    (W# x#) - (W# y#)      = W# (x# `minusWord#` y#)
    (W# x#) * (W# y#)      = W# (x# `timesWord#` y#)
    negate (W# x#)         = W# (int2Word# (negateInt# (word2Int# x#)))
    abs x                  = x
    signum x | x == fromInteger (Z# 0#) = fromInteger (Z# 0#)
    signum _               = fromInteger (Z# 1#)
    fromInteger i          = W# (integerToWord i)

instance  Num Integer  where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    negate         = negateInteger
--     fromInteger x  =  x
    fromInteger    = fromIntegerInteger
-- 
    abs = absInteger
    signum = signumInteger

{-# NOINLINE plusInt #-}
plusInt :: Int -> Int -> Int
plusInt (I# x) (I# y) = I# (x +# y)

{-# NOINLINE minusInt #-}
minusInt :: Int -> Int -> Int
minusInt (I# x) (I# y) = I# (x -# y)

{-# NOINLINE timesInt #-}
timesInt :: Int -> Int -> Int
timesInt (I# x) (I# y) = I# (x *# y)

{-# NOINLINE negateInt #-}
negateInt :: Int -> Int
negateInt (I# x) = I# (negateInt# x)

{-# NOINLINE absInt #-}
absInt :: Int -> Int
absInt n = if n `geInt` (fromInteger zeroInteger) then n else negate n

{-# NOINLINE signumInt #-}
signumInt :: Int -> Int
signumInt n | n `ltInt` (fromInteger zeroInteger) = negate (fromInteger oneInteger)
            | n `eqInt` (fromInteger zeroInteger) = fromInteger zeroInteger
signumInt _ = fromInteger oneInteger

{-# NOINLINE fromIntegerInt #-}
fromIntegerInt :: Integer -> Int
fromIntegerInt i = I# (integerToInt i)

{-# NOINLINE fromIntegerInteger #-}
fromIntegerInteger :: Integer -> Integer
fromIntegerInteger x = x

