{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Real (Integral (..)) where

-- For why this file exists
-- See Note [Semigroup stimes cycle] in GHC.Internal.Base

import GHC.Classes2 (Ord)
import GHC.Integer2 (Integer)

import {-# SOURCE #-} GHC.Num (Num)
import {-# SOURCE #-} GHC.Enum (Enum)

import GHC.Types2

data Ratio (a :: Type)
type Rational = Ratio Integer

class (Num a, Ord a) => Real a where
    toRational          :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot                :: a -> a -> a
    rem                 :: a -> a -> a
    div                 :: a -> a -> a
    mod                 :: a -> a -> a
    quotRem             :: a -> a -> (a,a)
    divMod              :: a -> a -> (a,a)
    toInteger           :: a -> Integer

    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d
