{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex
-- Copyright   :  (c) The University of Glasgow 2(fromInteger zeroInteger)(fromInteger zeroInteger)1
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Complex numbers.
--
-----------------------------------------------------------------------------

module Data.Complex
        (
--         -- * Rectangular form
          Complex((:+))
-- 
        , realPart
        , imagPart
        -- * Polar form
        , mkPolar
        , cis
        -- , polar
        , magnitude
        -- , phase
        -- * Conjugate
        , conjugate

        )  where

-- import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import GHC.Integer2
-- import Data.Data (Data)
-- import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
--                 alignment)

infix  6  :+

-----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        -- deriving (Eq, Show, Read, Data, Generic, Generic1
        --         , Functor, Foldable, Traversable)

-----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (negate y)

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
-- {-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
-- polar            :: (RealFloat a) => Complex a -> (a,a)
-- polar z          =  (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
-- {-# SPECIALISE phase :: Complex Double -> Double #-}
-- phase :: (RealFloat a) => Complex a -> a
-- phase ((fromInteger zeroInteger) :+ (fromInteger zeroInteger))   = (fromInteger zeroInteger)            -- SLPJ July 97 from John Peterson
-- phase (x:+y)     = atan2 y x

-- 
-- -- -----------------------------------------------------------------------------
-- -- Instances of Complex
-- 
instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  magnitude z :+ (fromInteger zeroInteger)
    signum (x :+ y)
        | x == fromInteger zeroInteger
        , y == fromInteger zeroInteger =  (fromInteger zeroInteger)
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ (fromInteger zeroInteger)
-- 
-- instance  (RealFloat a) => Fractional (Complex a)  where
--     {-# SPECIALISE instance Fractional (Complex Float) #-}
--     {-# SPECIALISE instance Fractional (Complex Double) #-}
--     (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
--                            where x'' = scaleFloat k x'
--                                  y'' = scaleFloat k y'
--                                  k   = - max (exponent x') (exponent y')
--                                  d   = x'*x'' + y'*y''
-- 
--     fromRational a      =  fromRational a :+ (fromInteger zeroInteger)
-- 
-- instance  (RealFloat a) => Floating (Complex a) where
--     {-# SPECIALISE instance Floating (Complex Float) #-}
--     {-# SPECIALISE instance Floating (Complex Double) #-}
--     pi             =  pi :+ (fromInteger zeroInteger)
--     exp (x:+y)     =  expx * cos y :+ expx * sin y
--                       where expx = exp x
--     log z          =  log (magnitude z) :+ phase z
-- 
--     x ** y = case (x,y) of
--       (_ , ((fromInteger zeroInteger):+(fromInteger zeroInteger)))  -> 1 :+ (fromInteger zeroInteger)
--       (((fromInteger zeroInteger):+(fromInteger zeroInteger)), (exp_re:+_)) -> case compare exp_re (fromInteger zeroInteger) of
--                  GT -> (fromInteger zeroInteger) :+ (fromInteger zeroInteger)
--                  LT -> inf :+ (fromInteger zeroInteger)
--                  EQ -> nan :+ nan
--       ((re:+im), (exp_re:+_))
--         | (isInfinite re || isInfinite im) -> case compare exp_re (fromInteger zeroInteger) of
--                  GT -> inf :+ (fromInteger zeroInteger)
--                  LT -> (fromInteger zeroInteger) :+ (fromInteger zeroInteger)
--                  EQ -> nan :+ nan
--         | otherwise -> exp (log x * y)
--       where
--         inf = 1/(fromInteger zeroInteger)
--         nan = (fromInteger zeroInteger)/(fromInteger zeroInteger)
-- 
--     sqrt ((fromInteger zeroInteger):+(fromInteger zeroInteger))    =  (fromInteger zeroInteger)
--     sqrt z@(x:+y)  =  u :+ (if y < (fromInteger zeroInteger) then -v else v)
--                       where (u,v) = if x < (fromInteger zeroInteger) then (v',u') else (u',v')
--                             v'    = abs y / (u'*2)
--                             u'    = sqrt ((magnitude z + abs x) / 2)
-- 
--     sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
--     cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
--     tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
--                       where sinx  = sin x
--                             cosx  = cos x
--                             sinhy = sinh y
--                             coshy = cosh y
-- 
--     sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
--     cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
--     tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
--                       where siny  = sin y
--                             cosy  = cos y
--                             sinhx = sinh x
--                             coshx = cosh x
-- 
--     asin z@(x:+y)  =  y':+(-x')
--                       where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
--     acos z         =  y'':+(-x'')
--                       where (x'':+y'') = log (z + ((-y'):+x'))
--                             (x':+y')   = sqrt (1 - z*z)
--     atan z@(x:+y)  =  y':+(-x')
--                       where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))
-- 
--     asinh z        =  log (z + sqrt (1+z*z))
--     acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
--     atanh z        =  (fromInteger zeroInteger).5 * log ((1.(fromInteger zeroInteger)+z) / (1.(fromInteger zeroInteger)-z))
-- 
--     log1p x@(a :+ b)
--       | abs a < (fromInteger zeroInteger).5 && abs b < (fromInteger zeroInteger).5
--       , u <- 2*a + a*a + b*b = log1p (u/(1 + sqrt(u+1))) :+ atan2 (1 + a) b
--       | otherwise = log (1 + x)
--     {-# INLINE log1p #-}
-- 
--     expm1 x@(a :+ b)
--       | a*a + b*b < 1
--       , u <- expm1 a
--       , v <- sin (b/2)
--       , w <- -2*v*v = (u*w + u + w) :+ (u+1)*sin b
--       | otherwise = exp x - 1
--     {-# INLINE expm1 #-}
-- 
-- instance Storable a => Storable (Complex a) where
--     sizeOf a       = 2 * sizeOf (realPart a)
--     alignment a    = alignment (realPart a)
--     peek p           = do
--                         q <- return $ castPtr p
--                         r <- peek q
--                         i <- peekElemOff q 1
--                         return (r :+ i)
--     poke p (r :+ i)  = do
--                         q <-return $  (castPtr p)
--                         poke q r
--                         pokeElemOff q 1 i
-- 
-- instance Applicative Complex where
--   pure a = a :+ a
--   f :+ g <*> a :+ b = f a :+ g b
-- 
-- instance Monad Complex where
--   a :+ b >>= f = realPart (f a) :+ imagPart (f b)
