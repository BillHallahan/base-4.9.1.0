{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- 
module GHC.Char
    ( -- * Utilities
      chr
-- 
--       -- * Monomorphic equality operators
--       -- | See GHC.Classes#matching_overloaded_methods_in_rules
    , eqChar, neChar
    ) where
-- 
import GHC.Base
-- import GHC.Show
-- 
-- -- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
chr :: Int -> Char
chr (I# i) | i $>=# 0# = C# (chr# i)
           | otherwise = errorWithoutStackTrace ("Prelude.chr: bad argument")
-- chr i@(I# i#)
--  | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
--  | otherwise
--     = errorWithoutStackTrace ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")
-- 
