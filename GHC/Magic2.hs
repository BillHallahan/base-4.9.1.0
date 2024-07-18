{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Magic2
  ( module GHC.Magic2
--   , module GHC.Magic
  ) where

-- import GHC.Magic
import GHC.Prim2 (State#, realWorld#, RealWorld, Int#)

oneShot :: (a -> b) -> (a -> b)
oneShot f = f

runRW# :: forall r o.
          (State# RealWorld -> o) -> o
-- See Note [runRW magic] in GHC.CoreToStg.Prep.
{-# NOINLINE runRW# #-}  -- runRW# is inlined manually in CorePrep
runRW# m = m realWorld#