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
  , TrName (..)
  , TyCon (..)
  , KindRep (..)
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

data Module = Module
                TrName   -- ^ Package name
                TrName   -- ^ Module name

data TrName
  = TrNameS Int#  -- ^ Static
  | TrNameD [Char] -- ^ Dynamic

-- | A de Bruijn index for a binder within a 'KindRep'.
type KindBndr = Int

-- | The representation produced by GHC for conjuring up the kind of a
-- 'Data.Typeable.TypeRep'.

-- See Note [Representing TyCon kinds: KindRep] in GHC.Tc.Instance.Typeable.
data KindRep = KindRepTyConApp TyCon [KindRep]
             | KindRepVar !KindBndr
             | KindRepApp KindRep KindRep
             | KindRepFun KindRep KindRep
             | KindRepTYPE !T.RuntimeRep
             | KindRepTypeLitS TypeLitSort Addr#
             | KindRepTypeLitD TypeLitSort [Char]

data TypeLitSort = TypeLitSymbol
                 | TypeLitNat
                 | TypeLitChar

-- Show instance for TyCon found in GHC.Internal.Show
data TyCon = TyCon Word#    -- ^ Fingerprint (high)
                   Word#    -- ^ Fingerprint (low)
                   Module     -- ^ Module in which this is defined
                   TrName     -- ^ Type constructor name
                   Int#       -- ^ How many kind variables do we accept?
                   KindRep    -- ^ A representation of the type's kind

data Multiplicity = Many | One
