{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}


module GHC.Prim2
  ( module GHC.Prim2
  , module GHC.PrimSMT
--   , module GHC.Prim
  , Int#, Double#, Char#, Float#, Word#, TYPE
  , coerce
  ) where

import GHC.Prim
  ( Int#, Double#, Char#, Float#, Word#, TYPE -- , Addr#
  , coerce)

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import GHC.Types
  (Bool (..), Char, Levity, RuntimeRep (..))
#else
import GHC.Types
  (Bool (..), Char, RuntimeRep (..))
#endif

import GHC.PrimSMT

-- import GHC.Prim hiding
--     ((+#), (-#), (*#), negateInt#,
--      (==#), (/=#), (>#), (>=#), (<#), (<=#),
-- 
--      (==##), (/=##), (>##), (>=##), (<##), (<=##),
-- 
--      eqFloat#, gtFloat#, geFloat#, ltFloat#, leFloat#,
--      negateFloat#,
--      plusFloat#, minusFloat#, timesFloat#, divideFloat#,
--      powerFloat#, sqrtFloat#, expFloat#, logFloat#,
--      sinFloat#, cosFloat#, tanFloat#,
--      asinFloat#, acosFloat#, atanFloat#,
--      sinhFloat#, coshFloat#, tanhFloat#,
-- 
--      eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
--      eqWord#, neWord#, gtWord#, geWord#, ltWord#, leWord#,
--      word2Int#
--     )

-- Int# operators

-- Returns 0 to indicate an unrecognized type, or positive Ints for special recognized types.
typeIndex# :: forall a . a -> Int#
typeIndex# = typeIndex#

(+#) :: Int# -> Int# -> Int#
(+#) = (+#)

(-#) :: Int# -> Int# -> Int#
(-#) = (-#)

(*#) :: Int# -> Int# -> Int#
(*#) = (*#)

negateInt# :: Int# -> Int#
negateInt# = negateInt#

-- Int#

(==#) :: Int# -> Int# -> Int#
x ==# y = case x $==# y of
            True -> 1#
            False -> 0#

(/=#) :: Int# -> Int# -> Int#
x /=# y = case x $/=# y of
            True -> 1#
            False -> 0#

(>#) :: Int# -> Int# -> Int#
x ># y = case x $># y of
            True -> 1#
            False -> 0#

(>=#) :: Int# -> Int# -> Int#
x >=# y = case x $>=# y of
            True -> 1#
            False -> 0#

(<#) :: Int# -> Int# -> Int#
x <# y = case x $<# y of
            True -> 1#
            False -> 0#

(<=#) :: Int# -> Int# -> Int#
x <=# y = case x $<=# y of
            True -> 1#
            False -> 0#

quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
quotRemInt# x y = (# quotInt# x y, remInt# x y #)

quotInt# :: Int# -> Int# -> Int#
quotInt# = quotInt#

remInt# :: Int# -> Int# -> Int#
remInt# = remInt#

-- Double#

(+##) :: Double# -> Double# -> Double#
(+##) = (+##)

(-##) :: Double# -> Double# -> Double#
(-##) = (-##)

(*##) :: Double# -> Double# -> Double#
(*##) = (*##)

(/##) :: Double# -> Double# -> Double#
(/##) = (/##)

(==##) :: Double# -> Double# -> Int#
x ==## y = case x $==## y of
            True -> 1#
            False -> 0#

(/=##) :: Double# -> Double# -> Int#
x /=## y = case x $/=## y of
            True -> 1#
            False -> 0#

(>##) :: Double# -> Double# -> Int#
x >## y = case x $>## y of
            True -> 1#
            False -> 0#

(>=##) :: Double# -> Double# -> Int#
x >=## y = case x $>=## y of
            True -> 1#
            False -> 0#

(<##) :: Double# -> Double# -> Int#
x <## y = case x $<## y of
            True -> 1#
            False -> 0#

(<=##) :: Double# -> Double# -> Int#
x <=## y = case x $<=## y of
            True -> 1#
            False -> 0#

negateDouble# :: Double# -> Double#
negateDouble# = negateDouble#

decodeDouble# :: Double# -> (# Int#, Int# #)
decodeDouble# = decodeDouble#

encodeDoubleInteger# :: Int# -> Int# -> Double#
encodeDoubleInteger# = encodeDoubleInteger#

isDoubleNegativeZero# :: Double# -> Bool
isDoubleNegativeZero# = isDoubleNegativeZero#

isDoubleDenormalized# :: Double# -> Bool
isDoubleDenormalized# = isDoubleDenormalized#

isDoubleNaN# :: Double# -> Bool
isDoubleNaN# = isDoubleNaN#

isDoubleInfinite# :: Double# -> Bool
isDoubleInfinite# = isDoubleInfinite#

expDouble#, logDouble#, sqrtDouble# :: Double# -> Double#
expDouble# = expDouble#
logDouble# = logDouble#
sqrtDouble# = sqrtDouble#

truncZeroDouble# :: Double# -> Int#
truncZeroDouble# = truncZeroDouble#

decPartDouble# :: Double# -> Double#
decPartDouble# = decPartDouble#

-- Float#

eqFloat# :: Float# -> Float# -> Int#
eqFloat# x y = case x `smtEqFloat#` y of
            True -> 1#
            False -> 0#

neFloat# :: Float# -> Float# -> Int#
neFloat# x y = case x `smtNeFloat#` y of
            True -> 1#
            False -> 0#

gtFloat# :: Float# -> Float# -> Int#
gtFloat# x y = case x `smtGtFloat#` y of
            True -> 1#
            False -> 0#

geFloat# :: Float# -> Float# -> Int#
geFloat# x y = case x `smtGeFloat#` y of
            True -> 1#
            False -> 0#

ltFloat# :: Float# -> Float# -> Int#
ltFloat# x y = case x `smtLtFloat#` y of
            True -> 1#
            False -> 0#

leFloat# :: Float# -> Float# -> Int#
leFloat# x y = case x `smtLeFloat#` y of
            True -> 1#
            False -> 0#

negateFloat# :: Float# -> Float#
negateFloat# = negateFloat#

plusFloat# :: Float# -> Float# -> Float#
plusFloat# = plusFloat#

minusFloat# :: Float# -> Float# -> Float#
minusFloat# = minusFloat#

timesFloat# :: Float# -> Float# -> Float#
timesFloat# = timesFloat#

divideFloat# :: Float# -> Float# -> Float#
divideFloat# = divideFloat#

-- powerFloat# :: Float# -> Float# -> Float#
-- powerFloat# = powerFloat#

sqrtFloat# :: Float# -> Float#
sqrtFloat# = sqrtFloat#

decodeFloat# :: Float# -> (# Int#, Int# #)
decodeFloat# = decodeFloat#

encodeFloatInteger# :: Int# -> Int# -> Float#
encodeFloatInteger# = encodeFloatInteger#

isFloatNegativeZero# :: Float# -> Bool
isFloatNegativeZero# = isFloatNegativeZero#

isFloatDenormalized# :: Float# -> Bool
isFloatDenormalized# = isFloatDenormalized#

isFloatNaN# :: Float# -> Bool
isFloatNaN# = isFloatNaN#

isFloatInfinite# :: Float# -> Bool
isFloatInfinite# = isFloatInfinite#

truncZeroFloat# :: Float# -> Int#
truncZeroFloat# = truncZeroFloat#

decPartFloat# :: Float# -> Float#
decPartFloat# = decPartFloat#

-- expFloat# :: Float# -> Float#
-- expFloat# = expFloat#
-- 
-- logFloat# :: Float# -> Float#
-- logFloat# = logFloat#
-- 
-- sinFloat# :: Float# -> Float#
-- sinFloat# = sinFloat#
-- 
-- cosFloat# :: Float# -> Float#
-- cosFloat# = cosFloat#
-- 
-- tanFloat# :: Float# -> Float#
-- tanFloat# = tanFloat#
-- 
-- asinFloat# :: Float# -> Float#
-- asinFloat# = asinFloat#
-- 
-- acosFloat# :: Float# -> Float#
-- acosFloat# = acosFloat#
-- 
-- atanFloat# :: Float# -> Float#
-- atanFloat# = atanFloat#
-- 
-- sinhFloat# :: Float# -> Float#
-- sinhFloat# = sinhFloat#
-- 
-- coshFloat# :: Float# -> Float#
-- coshFloat# = coshFloat#
-- 
-- tanhFloat# :: Float# -> Float#
-- tanhFloat# = tanhFloat#

-- Char#

eqChar# :: Char# -> Char# -> Int#
eqChar# x y = case x `smtEqChar#` y of
            True -> 1#
            False -> 0#

neChar# :: Char# -> Char# -> Int#
neChar# x y = case x `smtNeChar#` y of
            True -> 1#
            False -> 0#

gtChar# :: Char# -> Char# -> Bool
gtChar# = gtChar#

geChar# :: Char# -> Char# -> Bool
geChar# = geChar#

ltChar# :: Char# -> Char# -> Bool
ltChar# = ltChar#

leChar# :: Char# -> Char# -> Bool
leChar# = leChar#

chr# :: Int# -> Char#
chr# = chr#

ord# :: Char# -> Int#
ord# = ord#

-- Word#

plusWord# :: Word# -> Word# -> Word#
plusWord# = plusWord#

minusWord# :: Word# -> Word# -> Word#
minusWord# = minusWord#

timesWord# :: Word# -> Word# -> Word#
timesWord# = timesWord#

eqWord# :: Word# -> Word# -> Bool
eqWord# = eqWord#

neWord# :: Word# -> Word# -> Bool
neWord# = neWord#

gtWord# :: Word# -> Word# -> Bool
gtWord# = gtWord#

geWord# :: Word# -> Word# -> Bool
geWord# = geWord#

ltWord# :: Word# -> Word# -> Bool
ltWord# = ltWord#

leWord# :: Word# -> Word# -> Bool
leWord# = leWord#

word2Int# :: Word# -> Int#
word2Int# = word2Int#

int2Word# :: Int# -> Word#
int2Word# = int2Word#

-- MutVar#

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
newMutVar# :: forall {l :: Levity} a d. a -> State# d -> (# State# d, MutVar# d a #)
readMutVar# :: forall {l :: Levity} d a. MutVar# d a -> State# d -> (# State# d, a #)
writeMutVar# :: forall {l :: Levity} d a. MutVar# d a -> a -> State# d -> State# d
#else
newMutVar# :: forall a d. a -> State# d -> (# State# d, MutVar# d a #)
readMutVar# :: forall d a. MutVar# d a -> State# d -> (# State# d, a #)
writeMutVar# :: forall d a. MutVar# d a -> a -> State# d -> State# d
#endif

newMutVar# x !s = let y = newMutVar## x s in y `nsmv` (# s, y #)

readMutVar# m !s = let !y = readMutVar## m s in m `nsmv` (# s, y #)

{-# NOINLINE nsmv #-}
nsmv :: b -> (# State# d, a #) -> (# State# d, a #)
nsmv !x y = y

writeMutVar# m x s = s `ns` (m `ns` writeMutVar## m x s)

{-# NOINLINE ns #-}
ns :: a -> State# d -> State# d
ns !x y = y

-- Others

seq :: a -> b -> b
seq !_ b = b

-- Misc add-ons

float2Double# :: Float# -> Double#
float2Double# = float2Double#

double2Float# :: Double# -> Float#
double2Float# = double2Float#

fromIntToFloat :: Int# -> Float#
fromIntToFloat = fromIntToFloat

fromIntToDouble :: Int# -> Double#
fromIntToDouble = fromIntToDouble

data RealWorld = RealWorld

realWorld# :: State# RealWorld
realWorld# = State# RealWorld

data Void# = Void#

void# :: Void#
void# = Void#

data Addr# = Addr# Int#

nullAddr# :: Addr#
nullAddr# = Addr# 0#

plusAddr# :: Addr# -> Int# -> Addr# 
plusAddr# (Addr# x) y= Addr# (x +# y)

minusAddr# :: Addr# -> Addr# -> Int# 
minusAddr# (Addr# x) (Addr# y) = (x -# y)

dataToTag# :: a -> Int#
dataToTag# !x = dataToTag## x

{-# NOINLINE dataToTag## #-}
dataToTag## :: a -> Int#
dataToTag## _ = 0#

{-# NOINLINE tagToEnum# #-}
tagToEnum# :: Int# -> a
tagToEnum# _ = let x = x in x

data (~#) a b (x :: a) (y :: b) = Co

-- String primitives

{-# NOINLINE strLen# #-}
strLen# :: [a] -> Int#
strLen# = strLen#

{-# NOINLINE strAppend# #-}
strAppend# :: [a] -> [a] -> [a]
strAppend# = strAppend#

{-# NOINLINE strAt# #-}
strAt# :: [a] -> Int# -> [a]
strAt# = strAt#

{-# NOINLINE strSubstr# #-}
strSubstr# :: [a] -> Int# -> Int# -> [a]
strSubstr# = strSubstr#

{-# NOINLINE strEq# #-}
strEq# :: [a] -> [a] -> Bool
strEq# = strEq#

{-# NOINLINE strLt# #-}
strLt# :: [a] -> [a] -> Bool
strLt# = strLt#

{-# NOINLINE strLe# #-}
strLe# :: [a] -> [a] -> Bool
strLe# = strLe#

{-# NOINLINE strGt# #-}
strGt# :: [a] -> [a] -> Bool
strGt# = strGt#

{-# NOINLINE strGe# #-}
strGe# :: [a] -> [a] -> Bool
strGe# = strGe#

{-# NOINLINE ite #-}
ite :: Bool -> a -> a -> a
ite = ite

{-# NOINLINE strIndexOf# #-}
strIndexOf# :: [a] -> [a] -> Int# -> Int#
strIndexOf# = strIndexOf#

{-# NOINLINE strPrefixOf# #-}
strPrefixOf# :: [a] -> [a] -> Bool
strPrefixOf# = strPrefixOf#

{-# NOINLINE strSuffixOf# #-}
strSuffixOf# :: [a] -> [a] -> Bool
strSuffixOf# = strSuffixOf#

{-# NOINLINE strReplace# #-}
strReplace# :: [a] -> [a] -> [a] -> [a]
strReplace# = strReplace#

infixl 5 `adjStr`

-- Note [adjStr]
-- go walks over a String to make sure that it is representable in the SMT solver.
-- Suppose we have:
--
-- > xs ++ (f ys)
--
-- (where xs is a symbolic variable of type String.)
-- We must evaluate `f ys` to get a string that can be sent to the SMT solver.
-- However, we want to avoid concretizing the symbolic variable xs.
-- Thus, adjStr first checks if it is passed a symbolic variable (or other string that is
-- already representable in the SMT solver, i.e. a literal or primtive application), and
-- if so does nothing. Only if passed a non-SMT representable value does adjStr walk over the value,
-- forcing evaluation.
--
-- Note that we:
-- (1) first check if the input to go is SMT representable
-- (2) if it is not, force evaluation of the input, then again check if the value is SMT representable.
-- This is needed because of the following scenario: suppose we have:
-- > xs Data.List.++ (ys Data.List.++ zs)
-- This should result in no concretization- however, recognizing that `ys Data.List.++ zs` requires
-- no evaluation requires evaluating it to reach the primitive StrAppend operator.
-- However, we cannot ALWAYS force evaluation, because evaluating xs will result in case splitting.
-- Thus, we first check if the value is ALREADY SMT representable, and only if it is not,
-- force evaluation.
-- (If the value is not SMT representable at all, even after evaluation, we are no worse off, since
-- such values must just be concretized.)

-- Force evaluation of Strings so that they can be sent to the SMT solver.
-- Intended to be used with typeIndex#- forces evaluation only if typeIndex#
-- returns 1# (indicating a String.)
{-# NOINLINE adjStr #-}
adjStr :: forall a . Int# -> [a] -> Int#
adjStr x xs = case x of 1# -> go xs; _ -> x
  where
    -- See note [adjStr]
    go xs | isSMTRep# xs = x
    go !xs | isSMTRep# xs = x
    go [] = x
    go (x:xs) = go xs
