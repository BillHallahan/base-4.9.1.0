{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.PrimSMT where

import GHC.Prim
  ( Int#, Double#, Char#, Float#, Word#, TYPE -- , Addr#
  , coerce)
import GHC.Types
  (Bool (..), Char)

-- We can't allow these to inline, and we can't put these in GHC.Prim2,
-- because we need to avoid GHC trying to optimize based on the idea
-- that these functions lead to infinite loops

{-# NOINLINE ($==#) #-}
($==#) :: Int# -> Int# -> Bool
($==#) = ($==#)

{-# NOINLINE ($/=#) #-}
($/=#) :: Int# -> Int# -> Bool
($/=#) = ($/=#)

{-# NOINLINE ($>#) #-}
($>#) :: Int# -> Int# -> Bool
($>#) = ($>#)

{-# NOINLINE ($>=#) #-}
($>=#) :: Int# -> Int# -> Bool
($>=#) = ($>=#)

{-# NOINLINE ($<#) #-}
($<#) :: Int# -> Int# -> Bool
($<#) = ($<#)

{-# NOINLINE ($<=#) #-}
($<=#) :: Int# -> Int# -> Bool
($<=#) = ($<=#)

{-# NOINLINE ($==##) #-}
($==##) :: Double# -> Double# -> Bool
($==##) = ($==##)

{-# NOINLINE ($/=##) #-}
($/=##) :: Double# -> Double# -> Bool
($/=##) = ($/=##)

{-# NOINLINE ($>##) #-}
($>##) :: Double# -> Double# -> Bool
($>##) = ($>##)

{-# NOINLINE ($>=##) #-}
($>=##) :: Double# -> Double# -> Bool
($>=##) = ($>=##)

{-# NOINLINE ($<##) #-}
($<##) :: Double# -> Double# -> Bool
($<##) = ($<##)

{-# NOINLINE ($<=##) #-}
($<=##) :: Double# -> Double# -> Bool
($<=##) = ($<=##)

{-# NOINLINE smtEqFloat# #-}
smtEqFloat# :: Float# -> Float# -> Bool
smtEqFloat# = smtEqFloat#

{-# NOINLINE smtNeFloat# #-}
smtNeFloat# :: Float# -> Float# -> Bool
smtNeFloat# = smtNeFloat#

{-# NOINLINE smtGtFloat# #-}
smtGtFloat# :: Float# -> Float# -> Bool
smtGtFloat# = smtGtFloat#

{-# NOINLINE smtGeFloat# #-}
smtGeFloat# :: Float# -> Float# -> Bool
smtGeFloat# = smtGeFloat#

{-# NOINLINE smtLtFloat# #-}
smtLtFloat# :: Float# -> Float# -> Bool
smtLtFloat# = smtLtFloat#

{-# NOINLINE smtLeFloat# #-}
smtLeFloat# :: Float# -> Float# -> Bool
smtLeFloat# = smtLeFloat#

{-# NOINLINE smtEqChar# #-}
smtEqChar# :: Char# -> Char# -> Bool
smtEqChar# = smtEqChar#

{-# NOINLINE smtNeChar# #-}
smtNeChar# :: Char# -> Char# -> Bool
smtNeChar# = smtNeChar#
