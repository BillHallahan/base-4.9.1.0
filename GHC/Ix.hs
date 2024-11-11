{-# LANGUAGE MagicHash, Trustworthy #-}
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  Data.Ix
-- -- Copyright   :  (c) The University of Glasgow 2001
-- -- License     :  BSD-style (see the file libraries/base/LICENSE)
-- -- 
-- -- Maintainer  :  libraries@haskell.org
-- -- Stability   :  stable
-- -- Portability :  portable
-- --
-- -- The 'Ix' class is used to map a contiguous subrange of values in
-- -- type onto integers.  It is used primarily for array indexing
-- -- (see the array package).  'Ix' uses row-major order.
-- -- 
-- -----------------------------------------------------------------------------
-- 
module GHC.Ix
    (
    -- * The 'Ix' class
        Ix
          ( range
          , index
          , inRange
          , rangeSize
          )
    -- Ix instances:
    --
    --  Ix Char
    --  Ix Int
    --  Ix Integer
    --  Ix Bool
    --  Ix Ordering
    --  Ix ()
    --  (Ix a, Ix b) => Ix (a, b)
    --  ...

    -- * Deriving Instances of 'Ix'
    -- | Derived instance declarations for the class 'Ix' are only possible
    -- for enumerations (i.e. datatypes having only nullary constructors)
    -- and single-constructor datatypes, including arbitrarily large tuples,
    -- whose constituent types are instances of 'Ix'. 
    -- 
    -- * For an enumeration, the nullary constructors are assumed to be
    -- numbered left-to-right with the indices being 0 to n-1 inclusive. This
    -- is the same numbering defined by the 'Enum' class. For example, given
    -- the datatype: 
    -- 
    -- >        data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    -- 
    -- we would have: 
    -- 
    -- >        range   (Yellow,Blue)        ==  [Yellow,Green,Blue]
    -- >        index   (Yellow,Blue) Green  ==  1
    -- >        inRange (Yellow,Blue) Red    ==  False
    -- 
    -- * For single-constructor datatypes, the derived instance declarations
    -- are as shown for tuples in chapter 19, section 2 of the Haskell 2010 report:
    -- <https://www.haskell.org/onlinereport/haskell2010/haskellch19.html>.

    ) where

import GHC.Enum
import GHC.Num
-- import GHC.ST
import GHC.Base
-- import GHC.List
-- import GHC.Real( fromIntegral )
import GHC.Show
-- 

-- 
-- -- | The 'Ix' class is used to map a contiguous subrange of values in
-- -- a type onto integers.  It is used primarily for array indexing
-- -- (see the array package).
-- --
-- -- The first argument @(l,u)@ of each of these operations is a pair
-- -- specifying the lower and upper bounds of a contiguous subrange of values.
-- --
-- -- An implementation is entitled to assume the following laws about these
-- -- operations:
-- --
-- -- * @'inRange' (l,u) i == 'elem' i ('range' (l,u))@ @ @
-- --
-- -- * @'range' (l,u) '!!' 'index' (l,u) i == i@, when @'inRange' (l,u) i@
-- --
-- -- * @'map' ('index' (l,u)) ('range' (l,u))) == [0..'rangeSize' (l,u)-1]@ @ @
-- --
-- -- * @'rangeSize' (l,u) == 'length' ('range' (l,u))@ @ @
-- --
class (Ord a) => Ix a where
    {-# MINIMAL range, (index | unsafeIndex), inRange #-}

    -- | The list of values in the subrange defined by a bounding pair.
    range               :: (a,a) -> [a]
    -- | The position of a subscript in the subrange.
    index               :: (a,a) -> a -> Int
    -- | Like 'index', but without checking that the value is in range.
    unsafeIndex         :: (a,a) -> a -> Int
    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange             :: (a,a) -> a -> Bool
    -- | The size of the subrange defined by a bounding pair.
    rangeSize           :: (a,a) -> Int
    -- | like 'rangeSize', but without checking that the upper bound is
    -- in range.
    unsafeRangeSize     :: (a,a) -> Int

        -- Must specify one of index, unsafeIndex

        -- 'index' is typically over-ridden in instances, with essentially
        -- the same code, but using indexError instead of hopelessIndexError
        -- Reason: we have 'Show' at the instances
    {-# INLINE index #-}  -- See Note [Inlining index]
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = hopelessIndexError

    unsafeIndex b i = index b i

    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
                       | otherwise   = 0        -- This case is only here to
                                                -- check for an empty range
        -- NB: replacing (inRange b h) by (l <= h) fails for
        --     tuples.  E.g.  (1,2) <= (2,1) but the range is empty

    unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1
-- 
-- {-
-- Note that the following is NOT right
--         rangeSize (l,h) | l <= h    = index b h + 1
--                         | otherwise = 0
-- 
-- Because it might be the case that l<h, but the range
-- is nevertheless empty.  Consider
--         ((1,2),(2,1))
-- Here l<h, but the second index ranges from 2..1 and
-- hence is empty

----------------------------------------------------------------------
instance  Ix Char  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = fromEnum i - fromEnum m

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Char"

    inRange (m,n) i     =  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Int  where
    {-# INLINE range #-}
        -- The INLINE stops the build in the RHS from getting inlined,
        -- so that callers can fuse with the result of range
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = i - m

    {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
                          -- and Note [Inlining index]
    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Int"

    {-# INLINE inRange #-}
    inRange (I# m,I# n) (I# i) =  isTrue# (m <=# i) && isTrue# (i <=# n)
-- 
-- instance Ix Word where
--     range (m,n)         = [m..n]
--     unsafeIndex (m,_) i = fromIntegral (i - m)
--     inRange (m,n) i     = m <= i && i <= n
-- 
-- ----------------------------------------------------------------------
-- instance  Ix Integer  where
--     {-# INLINE range #-}
--     range (m,n) = [m..n]
-- 
--     {-# INLINE unsafeIndex #-}
--     unsafeIndex (m,_n) i   = fromInteger (i - m)
-- 
--     {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
--                           -- and Note [Inlining index]
--     index b i | inRange b i =  unsafeIndex b i
--               | otherwise   =  indexError b i "Integer"
-- 
--     inRange (m,n) i     =  m <= i && i <= n
-- 
----------------------------------------------------------------------
-- instance Ix Bool where -- as derived
--     {-# INLINE range #-}
--     range (m,n) = [m..n]

--     {-# INLINE unsafeIndex #-}
--     unsafeIndex (l,_) i = fromEnum i - fromEnum l

--     {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
--                           -- and Note [Inlining index]
--     index b i | inRange b i =  unsafeIndex b i
--               | otherwise   =  indexError b i "Bool"

--     inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

-- ----------------------------------------------------------------------
-- instance Ix Ordering where -- as derived
--     {-# INLINE range #-}
--     range (m,n) = [m..n]
-- 
--     {-# INLINE unsafeIndex #-}
--     unsafeIndex (l,_) i = fromEnum i - fromEnum l
-- 
--     {-# INLINE index #-}  -- See Note [Out-of-bounds error messages]
--                           -- and Note [Inlining index]
--     index b i | inRange b i =  unsafeIndex b i
--               | otherwise   =  indexError b i "Ordering"
-- 
--     inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u
-- 
-- ----------------------------------------------------------------------
-- instance Ix () where
--     {-# INLINE range #-}
--     range   ((), ())    = [()]
--     {-# INLINE unsafeIndex #-}
--     unsafeIndex   ((), ()) () = 0
--     {-# INLINE inRange #-}
--     inRange ((), ()) () = True
-- 
--     {-# INLINE index #-}  -- See Note [Inlining index]
--     index b i = unsafeIndex b i
-- 
----------------------------------------------------------------------
instance (Ix a, Ix b) => Ix (a, b) where -- as derived
    {-# SPECIALISE instance Ix (Int,Int) #-}

    {-# INLINE range #-}
    range ((l1,l2),(u1,u2)) =
      [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {-# INLINE unsafeIndex #-}
    unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) =
      unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

    {-# INLINE inRange #-}
    inRange ((l1,l2),(u1,u2)) (i1,i2) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2

    -- Default method for index

-- ----------------------------------------------------------------------
-- instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
--     {-# SPECIALISE instance Ix (Int,Int,Int) #-}
-- 
--     range ((l1,l2,l3),(u1,u2,u3)) =
--         [(i1,i2,i3) | i1 <- range (l1,u1),
--                       i2 <- range (l2,u2),
--                       i3 <- range (l3,u3)]
-- 
--     unsafeIndex ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
--       unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
--       unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
--       unsafeIndex (l1,u1) i1))
-- 
--     inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
--       inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
--       inRange (l3,u3) i3
-- 
--     -- Default method for index
-- 
-- ----------------------------------------------------------------------
-- instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
--     range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
--       [(i1,i2,i3,i4) | i1 <- range (l1,u1),
--                        i2 <- range (l2,u2),
--                        i3 <- range (l3,u3),
--                        i4 <- range (l4,u4)]
-- 
--     unsafeIndex ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
--       unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
--       unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
--       unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
--       unsafeIndex (l1,u1) i1)))
-- 
--     inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
--       inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
--       inRange (l3,u3) i3 && inRange (l4,u4) i4
-- 
--     -- Default method for index
-- 
-- instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1,a2,a3,a4,a5)  where
--     range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
--       [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
--                           i2 <- range (l2,u2),
--                           i3 <- range (l3,u3),
--                           i4 <- range (l4,u4),
--                           i5 <- range (l5,u5)]
-- 
--     unsafeIndex ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
--       unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
--       unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
--       unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
--       unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
--       unsafeIndex (l1,u1) i1))))
-- 
--     inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
--       inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
--       inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
--       inRange (l5,u5) i5
-- 
--     -- Default method for index

-- Note [Out-of-bounds error messages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The default method for 'index' generates hoplelessIndexError, because
-- Ix doesn't have Show as a superclass.  For particular base types we
-- can do better, so we override the default method for index.
-- -}
-- 
-- -- Abstract these errors from the relevant index functions so that
-- -- the guts of the function will be small enough to inline.
-- 
{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp = error "indexError"
--   = errorWithoutStackTrace (showString "Ix{" . showString tp . showString "}.index: Index " .
--            showParen True (showsPrec 0 i) .
--            showString " out of range " $
--            showParen True (showsPrec 0 rng) "")
-- 
hopelessIndexError :: Int -- Try to use 'indexError' instead!
hopelessIndexError = error "hopelessIndexError" -- errorWithoutStackTrace "Error in array index"
