{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Fingerprint.Type (Fingerprint(..)) where

import GHC.Base
import GHC.List (length, replicate)
import GHC.Num
import GHC.Show
import GHC.Word
-- import Numeric (showHex)

-- Using 128-bit MD5 fingerprints for now.

data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
--   deriving (Eq, Ord)
instance Eq Fingerprint where
    Fingerprint x1 y1 == Fingerprint x2 y2 = x1 == x2 && y1 == y2

instance Ord Fingerprint where
    compare (Fingerprint x1 y1) (Fingerprint x2 y2) =
        case compare x1 y1 of
            EQ -> compare y1 y2
            x -> x
-- 
-- instance Show Fingerprint where
--   show (Fingerprint w1 w2) = hex16 w1 ++ hex16 w2
--     where
--       -- | Formats a 64 bit number as 16 digits hex.
--       hex16 :: Word64 -> String
--       hex16 i = let hex = showHex i ""
--                  in replicate (16 - length hex) '0' ++ hex
