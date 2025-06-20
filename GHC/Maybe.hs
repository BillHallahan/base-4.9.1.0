{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

#if __GLASGOW_HASKELL__ >= 806
-- | Maybe type
module GHC.Maybe
   ( Maybe (..)
   )
where

-- import GHC.Num.Integer () -- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Types2
import GHC.Classes2

default ()

-------------------------------------------------------------------------------
-- Maybe type
-------------------------------------------------------------------------------

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'Prelude.error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.
--
data  Maybe a  =  Nothing | Just a
  -- deriving ( Eq  -- ^ @since 2.01
  --          , Ord -- ^ @since 2.01
  --          )

instance (Eq a) => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x  == Just y  = x == y
    Nothing == Just _ = False
    Just _ == Nothing = False

instance (Ord a) => Ord (Maybe a) where
    Nothing <= Nothing = True
    Nothing <= Just _  = True
    Just _  <= Nothing = False
    Just x  <= Just y  = x <= y

#else
module GHC.Maybe where
#endif
