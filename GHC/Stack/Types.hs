{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Trustworthy       #-}

{-# LANGUAGE PackageImports    #-}
-- 
{-# OPTIONS_HADDOCK hide #-}
-- -- we hide this module from haddock to enforce GHC.Stack as the main
-- -- access point.
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  GHC.Stack.Types
-- -- Copyright   :  (c) The University of Glasgow 2015
-- -- License     :  see libraries/ghc-prim/LICENSE
-- --
-- -- Maintainer  :  cvs-ghc@haskell.org
-- -- Stability   :  internal
-- -- Portability :  non-portable (GHC Extensions)
-- --
-- -- type definitions for implicit call-stacks.
-- -- Use "GHC.Stack" from the base package instead of importing this
-- -- module directly.
-- --
-- -----------------------------------------------------------------------------
-- 
module GHC.Stack.Types (
--     -- * Implicit call stacks
    T.CallStack(..), T.HasCallStack,
    T.emptyCallStack, T.freezeCallStack, T.fromCallSiteList,
    T.getCallStack, T.pushCallStack,

    -- * Source locations
    T.SrcLoc(..)
  ) where

import GHC.Stack.Types2

import qualified "base" GHC.Stack.Types as T
