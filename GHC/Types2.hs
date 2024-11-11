{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Types2
  ( T.Char (..)
  , T.Int (..)
  , Word8
  , Word (..)
  , Float (..)
  , Double (..)
  , Ordering (..)
  , isTrue#
  , unpackChar
  , intToString#
  , IO (..)
  , RealWorld (..)
  , LiftedRep (..)
  , T.Bool(..)
  , T.Coercible(..)
  ) where

import GHC.Prim2

import qualified GHC.Types as T
-- import GHC.Types (Bool(..), Char) as T

data Char = C# Char#

data Int = I# Int#

type Word8 = Int

data Word = W# Word#

data Float = F# Float#

data Double = D# Double#

data Ordering = LT | EQ | GT

isTrue# :: Int# -> T.Bool
isTrue# = tagToEnum#

unpackChar :: T.Char -> Char#
unpackChar (T.C# c) = c

intToString# :: Int# -> [T.Char]
intToString# = intToString#

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

data LiftedRep = LiftedRep