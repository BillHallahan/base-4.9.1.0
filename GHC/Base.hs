{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
-- {-
-- 
-- NOTA BENE: Do NOT use ($) anywhere in this module! The type of ($) is
-- slightly magical (it can return unlifted types), and it is wired in.
-- But, it is also *defined* in this module, with a non-magical type.
-- GHC gets terribly confused (and *hangs*) if you try to use ($) in this
-- module, because it has different types in different scenarios.
-- 
-- This is not a problem in general, because the type ($), being wired in, is not
-- written out to the interface file, so importing files don't get confused.
-- The problem is only if ($) is used here. So don't!
-- 
-- ---------------------------------------------
-- 
-- The overall structure of the GHC Prelude is a bit tricky.
-- 
--   a) We want to avoid "orphan modules", i.e. ones with instance
--         decls that don't belong either to a tycon or a class
--         defined in the same module
-- 
--   b) We want to avoid giant modules
-- 
-- So the rough structure is as follows, in (linearised) dependency order
-- 
-- 
-- GHC.Prim        Has no implementation.  It defines built-in things, and
--                 by importing it you bring them into scope.
--                 The source file is GHC.Prim.hi-boot, which is just
--                 copied to make GHC.Prim.hi
-- 
-- GHC.Base        Classes: Eq, Ord, Functor, Monad
--                 Types:   list, (), Int, Bool, Ordering, Char, String
-- 
-- Data.Tuple      Types: tuples, plus instances for GHC.Base classes
-- 
-- GHC.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types
-- 
-- GHC.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types
-- 
-- Data.Maybe      Type: Maybe, plus instances for GHC.Base classes
-- 
-- GHC.List        List functions
-- 
-- GHC.Num         Class: Num, plus instances for Int
--                 Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)
-- 
--                 Integer is needed here because it is mentioned in the signature
--                 of 'fromInteger' in class Num
-- 
-- GHC.Real        Classes: Real, Integral, Fractional, RealFrac
--                          plus instances for Int, Integer
--                 Types:  Ratio, Rational
--                         plus intances for classes so far
-- 
--                 Rational is needed here because it is mentioned in the signature
--                 of 'toRational' in class Real
-- 
-- GHC.ST  The ST monad, instances and a few helper functions
-- 
-- Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples
-- 
-- GHC.Arr         Types: Array, MutableArray, MutableVar
-- 
--                 Arrays are used by a function in GHC.Float
-- 
-- GHC.Float       Classes: Floating, RealFloat
--                 Types:   Float, Double, plus instances of all classes so far
-- 
--                 This module contains everything to do with floating point.
--                 It is a big module (900 lines)
--                 With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi
-- 
-- 
-- Other Prelude modules are much easier with fewer complex dependencies.
-- -}
-- 
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , ExplicitForAll
           , MagicHash
           , UnboxedTuples
           , ExistentialQuantification
           , RankNTypes
           , KindSignatures
           , PolyKinds
           , DataKinds
  #-}
#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
{-# LANGUAGE TypeInType #-}
#endif
-- -- -Wno-orphans is needed for things like:
-- -- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  GHC.Base
-- -- Copyright   :  (c) The University of Glasgow, 1992-2002
-- -- License     :  see libraries/base/LICENSE
-- --
-- -- Maintainer  :  cvs-ghc@haskell.org
-- -- Stability   :  internal
-- -- Portability :  non-portable (GHC extensions)
-- --
-- -- Basic data types and classes.
-- --
-- -----------------------------------------------------------------------------
-- 
-- #include "MachDeps.h"
-- 
module GHC.Base
        (
        module GHC.Base,
--         module GHC.Classes,
        module GHC.Classes2,
--         module GHC.CString,
        module GHC.CString2,
--         module GHC.Magic,
        module GHC.Magic2,
--         module GHC.Types,
        module GHC.Types2,
--         module GHC.Prim,        -- Re-export GHC.Prim and [boot] GHC.Err,
        module GHC.Prim2,        -- Re-export GHC.Prim and [boot] GHC.Err,
--                                 -- to avoid lots of people having to
        module GHC.Err,          -- import it explicitly
        module GHC.BaseMonad,
        Maybe (..)
  )
        where
-- 
-- import GHC.Types
import GHC.Types2
-- import GHC.Classes
import GHC.Classes2
-- import GHC.CString
import GHC.CString2
-- import GHC.Magic
import GHC.Magic2
-- import GHC.Prim
import GHC.Prim2

import GHC.Err
-- import {-# SOURCE #-} GHC.IO (failIO,mplusIO)
-- 
-- import GHC.Tuple ()     -- Note [Depend on GHC.Tuple]
import GHC.Tuple2 ()     -- Note [Depend on GHC.Tuple]
-- import GHC.Integer ()   -- Note [Depend on GHC.Integer]
-- import GHC.Integer2 ()
-- 
#if __GLASGOW_HASKELL__ >= 806
import GHC.Maybe
#endif

import GHC.BaseMonad

infixr 5  ++
infixr 1  =<<
-- 
default ()              -- Double isn't available yet
-- 
-- {-
-- Note [Depend on GHC.Integer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The Integer type is special because TidyPgm uses
-- GHC.Integer.Type.mkInteger to construct Integer literal values
-- Currently it reads the interface file whether or not the current
-- module *has* any Integer literals, so it's important that
-- GHC.Integer.Type (in package integer-gmp or integer-simple) is
-- compiled before any other module.  (There's a hack in GHC to disable
-- this for packages ghc-prim, integer-gmp, integer-simple, which aren't
-- allowed to contain any Integer literals.)
-- 
-- Likewise we implicitly need Integer when deriving things like Eq
-- instances.
-- 
-- The danger is that if the build system doesn't know about the dependency
-- on Integer, it'll compile some base module before GHC.Integer.Type,
-- resulting in:
--   Failed to load interface for ‘GHC.Integer.Type’
--     There are files missing in the ‘integer-gmp’ package,
-- 
-- Bottom line: we make GHC.Base depend on GHC.Integer; and everything
-- else either depends on GHC.Base, or does not have NoImplicitPrelude
-- (and hence depends on Prelude).
-- 
-- Note [Depend on GHC.Tuple]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Similarly, tuple syntax (or ()) creates an implicit dependency on
-- GHC.Tuple, so we use the same rule as for Integer --- see Note [Depend on
-- GHC.Integer] --- to explain this to the build system.  We make GHC.Base
-- depend on GHC.Tuple, and everything else depends on GHC.Base or Prelude.
-- -}
-- 
-- #if 0
-- -- for use when compiling GHC.Base itself doesn't work
-- data  Bool  =  False | True
-- data Ordering = LT | EQ | GT
-- data Char = C# Char#
-- type  String = [Char]
-- data Int = I# Int#
-- data  ()  =  ()
-- data [] a = MkNil
-- 
-- not True = False
-- (&&) True True = True
-- otherwise = True
-- 
-- build = errorWithoutStackTrace "urk"
-- foldr = errorWithoutStackTrace "urk"
-- #endif
-- 
-- -- | The 'Maybe' type encapsulates an optional value.  A value of type
-- -- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- -- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- -- deal with errors or exceptional cases without resorting to drastic
-- -- measures such as 'error'.
-- --
-- -- The 'Maybe' type is also a monad.  It is a simple kind of error
-- -- monad, where all errors are represented by 'Nothing'.  A richer
-- -- error monad can be built using the 'Data.Either.Either' type.
-- --
#if __GLASGOW_HASKELL__ < 806
data  Maybe a  =  Nothing | Just a
--   deriving (Eq, Ord)

instance (Eq a) => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x  == Just y  = x == y

instance (Ord a) => Ord (Maybe a) where
    Nothing <= Nothing = True
    Nothing <= Just _  = True
    Just _  <= Nothing = False
    Just x  <= Just y  = x <= y
#endif

-- 
-- -- | The class of monoids (types with an associative binary operation that
-- -- has an identity).  Instances should satisfy the following laws:
-- --
-- --  * @mappend mempty x = x@
-- --
-- --  * @mappend x mempty = x@
-- --
-- --  * @mappend x (mappend y z) = mappend (mappend x y) z@
-- --
-- --  * @mconcat = 'foldr' mappend mempty@
-- --
-- -- The method names refer to the monoid of lists under concatenation,
-- -- but there are many other instances.
-- --
-- -- Some types can be viewed as a monoid in more than one way,
-- -- e.g. both addition and multiplication on numbers.
-- -- In such cases we often define @newtype@s and make those instances
-- -- of 'Monoid', e.g. 'Sum' and 'Product'.
-- 
class Monoid a where
        mempty  :: a
--         -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
--         -- ^ An associative operation
        mconcat :: [a] -> a
-- 
--         -- ^ Fold a list using the monoid.
--         -- For most types, the default definition for 'mconcat' will be
--         -- used, but the function is included in the class definition so
--         -- that an optimized version can be provided for specific types.
-- 
        mconcat = foldr mappend mempty
-- 
instance Monoid [a] where
        {-# INLINE mempty #-}
        mempty  = []
        {-# INLINE mappend #-}
        mappend = (++)
        {-# INLINE mconcat #-}
        mconcat xss = [x | xs <- xss, x <- xs]
-- -- See Note: [List comprehensions and inlining]
-- 
-- {-
-- Note: [List comprehensions and inlining]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The list monad operations are traditionally described in terms of concatMap:
-- 
-- xs >>= f = concatMap f xs
-- 
-- Similarly, mconcat for lists is just concat. Here in Base, however, we don't
-- have concatMap, and we'll refrain from adding it here so it won't have to be
-- hidden in imports. Instead, we use GHC's list comprehension desugaring
-- mechanism to define mconcat and the Applicative and Monad instances for lists.
-- We mark them INLINE because the inliner is not generally too keen to inline
-- build forms such as the ones these desugar to without our insistence.  Defining
-- these using list comprehensions instead of foldr has an additional potential
-- benefit, as described in compiler/deSugar/DsListComp.lhs: if optimizations
-- needed to make foldr/build forms efficient are turned off, we'll get reasonably
-- efficient translations anyway.
-- -}
-- 
-- instance Monoid b => Monoid (a -> b) where
--         mempty _ = mempty
--         mappend f g x = f x `mappend` g x
-- 
-- instance Monoid () where
--         -- Should it be strict?
--         mempty        = ()
--         _ `mappend` _ = ()
--         mconcat _     = ()
-- 
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--         mempty = (mempty, mempty)
--         (a1,b1) `mappend` (a2,b2) =
--                 (a1 `mappend` a2, b1 `mappend` b2)
-- 
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
--         mempty = (mempty, mempty, mempty)
--         (a1,b1,c1) `mappend` (a2,b2,c2) =
--                 (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)
-- 
-- instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
--         mempty = (mempty, mempty, mempty, mempty)
--         (a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
--                 (a1 `mappend` a2, b1 `mappend` b2,
--                  c1 `mappend` c2, d1 `mappend` d2)
-- 
-- instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
--                 Monoid (a,b,c,d,e) where
--         mempty = (mempty, mempty, mempty, mempty, mempty)
--         (a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
--                 (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
--                  d1 `mappend` d2, e1 `mappend` e2)
-- 
-- -- lexicographical ordering
-- instance Monoid Ordering where
--         mempty         = EQ
--         LT `mappend` _ = LT
--         EQ `mappend` y = y
--         GT `mappend` _ = GT
-- 
-- -- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- -- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- -- turned into a monoid simply by adjoining an element @e@ not in @S@
-- -- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\" Since
-- -- there is no \"Semigroup\" typeclass providing just 'mappend', we
-- -- use 'Monoid' instead.
-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   Nothing `mappend` m = m
--   m `mappend` Nothing = m
--   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-- 
-- instance Monoid a => Applicative ((,) a) where
--     pure x = (mempty, x)
--     (u, f) <*> (v, x) = (u `mappend` v, f x)
-- 
-- instance Monoid a => Monad ((,) a) where
--     (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)
-- 
-- instance Monoid a => Monoid (IO a) where
--     mempty = pure mempty
--     mappend = liftA2 mappend
-- 
-- 
-- {- Note [Recursive bindings for Applicative/Monad]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
-- The original Applicative/Monad proposal stated that after
-- implementation, the designated implementation of (>>) would become
-- 
--   (>>) :: forall a b. m a -> m b -> m b
--   (>>) = (*>)
-- 
-- by default. You might be inclined to change this to reflect the stated
-- proposal, but you really shouldn't! Why? Because people tend to define
-- such instances the /other/ way around: in particular, it is perfectly
-- legitimate to define an instance of Applicative (*>) in terms of (>>),
-- which would lead to an infinite loop for the default implementation of
-- Monad! And people do this in the wild.
-- 
-- This turned into a nasty bug that was tricky to track down, and rather
-- than eliminate it everywhere upstream, it's easier to just retain the
-- original default.
-- 
-- -}
-- 

-- -- | Same as '>>=', but with the arguments interchanged.
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f
-- 
-- -- | Conditional execution of 'Applicative' expressions. For example,
-- --
-- -- > when debug (putStrLn "Debugging")
-- --
-- -- will output the string @Debugging@ if the Boolean value @debug@
-- -- is 'True', and otherwise do nothing.
-- when      :: (Applicative f) => Bool -> f () -> f ()
-- {-# INLINEABLE when #-}
-- {-# SPECIALISE when :: Bool -> IO () -> IO () #-}
-- {-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
-- when p s  = if p then s else pure ()
-- 
-- -- | Evaluate each action in the sequence from left to right,
-- -- and collect the results.
-- sequence :: Monad m => [m a] -> m [a]
-- {-# INLINE sequence #-}
-- sequence = mapM id
-- -- Note: [sequence and mapM]
-- 
-- -- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- {-# INLINE mapM #-}
-- mapM f as = foldr k (return []) as
--             where
--               k a r = do { x <- f a; xs <- r; return (x:xs) }
-- 
-- {-
-- Note: [sequence and mapM]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Originally, we defined
-- 
-- mapM f = sequence . map f
-- 
-- This relied on list fusion to produce efficient code for mapM, and led to
-- excessive allocation in cryptarithm2. Defining
-- 
-- sequence = mapM id
-- 
-- relies only on inlining a tiny function (id) and beta reduction, which tends to
-- be a more reliable aspect of simplification. Indeed, this does not lead to
-- similar problems in nofib.
-- -}
-- 
-- -- | Promote a function to a monad.
-- liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
-- liftM f m1              = do { x1 <- m1; return (f x1) }
-- 
-- -- | Promote a function to a monad, scanning the monadic arguments from
-- -- left to right.  For example,
-- --
-- -- >    liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- -- >    liftM2 (+) (Just 1) Nothing = Nothing
-- --
-- liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
-- 
-- -- | Promote a function to a monad, scanning the monadic arguments from
-- -- left to right (cf. 'liftM2').
-- liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
-- liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
-- 
-- -- | Promote a function to a monad, scanning the monadic arguments from
-- -- left to right (cf. 'liftM2').
-- liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
-- liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }
-- 
-- -- | Promote a function to a monad, scanning the monadic arguments from
-- -- left to right (cf. 'liftM2').
-- liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
-- liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }
-- 
-- {-# INLINEABLE liftM #-}
-- {-# SPECIALISE liftM :: (a1->r) -> IO a1 -> IO r #-}
-- {-# SPECIALISE liftM :: (a1->r) -> Maybe a1 -> Maybe r #-}
-- {-# INLINEABLE liftM2 #-}
-- {-# SPECIALISE liftM2 :: (a1->a2->r) -> IO a1 -> IO a2 -> IO r #-}
-- {-# SPECIALISE liftM2 :: (a1->a2->r) -> Maybe a1 -> Maybe a2 -> Maybe r #-}
-- {-# INLINEABLE liftM3 #-}
-- {-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
-- {-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}
-- {-# INLINEABLE liftM4 #-}
-- {-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO r #-}
-- {-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe r #-}
-- {-# INLINEABLE liftM5 #-}
-- {-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO a5 -> IO r #-}
-- {-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe r #-}
-- 
-- {- | In many situations, the 'liftM' operations can be replaced by uses of
-- 'ap', which promotes function application.
-- 
-- >       return f `ap` x1 `ap` ... `ap` xn
-- 
-- is equivalent to
-- 
-- >       liftMn f x1 x2 ... xn
-- 
-- -}
-- 
ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- -- Since many Applicative instances define (<*>) = ap, we
-- -- cannot define ap = (<*>)
-- {-# INLINEABLE ap #-}
-- {-# SPECIALISE ap :: IO (a -> b) -> IO a -> IO b #-}
-- {-# SPECIALISE ap :: Maybe (a -> b) -> Maybe a -> Maybe b #-}
-- 
-- -- instances for Prelude types
-- 
-- 
-- 
-- 
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)
-- 
instance Applicative Maybe where
    pure = Just
-- 
    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing
-- 
    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing
-- 
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
-- 
    (>>) = (*>)
-- 
    -- fail _              = Nothing
-- 
-- -- -----------------------------------------------------------------------------
-- -- The Alternative class definition
-- 
infixl 3 <|>
-- 
-- -- | A monoid on applicative functors.
-- --
-- -- If defined, 'some' and 'many' should be the least solutions
-- -- of the equations:
-- --
-- -- * @some v = (:) '<$>' v '<*>' many v@
-- --
-- -- * @many v = some v '<|>' 'pure' []@
class Applicative f => Alternative f where
--     -- | The identity of '<|>'
    empty :: f a
--     -- | An associative binary operation
    (<|>) :: f a -> f a -> f a
-- 
--     -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (fmap (:) v) <*> many_v
-- 
--     -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (fmap (:) v) <*> many_v
-- 
-- 
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l
-- 
-- -- -----------------------------------------------------------------------------
-- -- The MonadPlus class definition
-- 
-- -- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
--    -- | the identity of 'mplus'.  It should also satisfy the equations
--    --
--    -- > mzero >>= f  =  mzero
--    -- > v >> mzero   =  mzero
--    --
   mzero :: m a
   mzero = empty
-- 
--    -- | an associative operation
   mplus :: m a -> m a -> m a
   mplus = (<|>)
-- 
-- instance MonadPlus Maybe
-- 
-- ----------------------------------------------
-- -- The list type
-- 
-- 
instance Alternative [] where
    empty = []
    (<|>) = (++)
-- 
-- instance MonadPlus []
-- 
-- {-
-- A few list functions that appear here because they are used here.
-- The rest of the prelude list functions are in GHC.List.
-- -}
-- 
-- ----------------------------------------------
-- --      foldr/build/augment
-- ----------------------------------------------
-- 
-- -- | 'foldr', applied to a binary operator, a starting value (typically
-- -- the right-identity of the operator), and a list, reduces the list
-- -- using the binary operator, from right to left:
-- --
-- -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- 
foldr            :: (a -> b -> b) -> b -> [a] -> b
-- -- foldr _ z []     =  z
-- -- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- -- Inline only in the final stage, after the foldr/cons rule has had a chance
-- -- Also note that we inline it when it has *two* parameters, which are the
-- -- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
-- 
-- -- | A list producer that can be fused with 'foldr'.
-- -- This function is merely
-- --
-- -- >    build g = g (:) []
-- --
-- -- but GHC's simplifier will transform an expression of the form
-- -- @'foldr' k z ('build' g)@, which may arise after inlining, to @g k z@,
-- -- which avoids producing an intermediate list.
-- 
build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
--         -- The INLINE is important, even though build is tiny,
--         -- because it prevents [] getting inlined in the version that
--         -- appears in the interface file.  If [] *is* inlined, it
--         -- won't match with [] appearing in rules in an importing module.
--         --
--         -- The "1" says to inline in phase 1
-- 
build g = g (:) []
-- 
-- -- | A list producer that can be fused with 'foldr'.
-- -- This function is merely
-- --
-- -- >    augment g xs = g (:) xs
-- --
-- -- but GHC's simplifier will transform an expression of the form
-- -- @'foldr' k z ('augment' g xs)@, which may arise after inlining, to
-- -- @g k ('foldr' k z xs)@, which avoids producing an intermediate list.
-- 
augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs
-- 
-- {-# RULES
-- "fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) .
--                 foldr k z (build g) = g k z
-- 
-- "foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
--                 foldr k z (augment g xs) = g k (foldr k z xs)
-- 
-- "foldr/id"                        foldr (:) [] = \x  -> x
-- "foldr/app"     [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
--         -- Only activate this from phase 1, because that's
--         -- when we disable the rule that expands (++) into foldr
-- 
-- -- The foldr/cons rule looks nice, but it can give disastrously
-- -- bloated code when commpiling
-- --      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- -- i.e. when there are very very long literal lists
-- -- So I've disabled it for now. We could have special cases
-- -- for short lists, I suppose.
-- -- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)
-- 
-- "foldr/single"  forall k z x. foldr k z [x] = k x z
-- "foldr/nil"     forall k z.   foldr k z []  = z
-- 
-- "foldr/cons/build" forall k z x (g::forall b. (a->b->b) -> b -> b) .
--                            foldr k z (x:build g) = k x (g k z)
-- 
-- "augment/build" forall (g::forall b. (a->b->b) -> b -> b)
--                        (h::forall b. (a->b->b) -> b -> b) .
--                        augment g (build h) = build (\c n -> g c (h c n))
-- "augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
--                         augment g [] = build g
--  #-}
-- 
-- -- This rule is true, but not (I think) useful:
-- --      augment g (augment h t) = augment (\cn -> g c (h c n)) t
-- 
-- ----------------------------------------------
-- --              map
-- ----------------------------------------------
-- 
-- 
-- -- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys
-- 
-- -- The rules for map work like this.
-- --
-- -- Up to (but not including) phase 1, we use the "map" rule to
-- -- rewrite all saturated applications of map with its build/fold
-- -- form, hoping for fusion to happen.
-- -- In phase 1 and 0, we switch off that rule, inline build, and
-- -- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- -- thing back into plain map.
-- --
-- -- It's important that these two rules aren't both active at once
-- -- (along with build's unfolding) else we'd get an infinite loop
-- -- in the rules.  Hence the activation control below.
-- --
-- -- The "mapFB" rule optimises compositions of map.
-- --
-- -- This same pattern is followed by many other functions:
-- -- e.g. append, filter, iterate, repeat, etc.
-- 
{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}
-- 
-- -- See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
-- -- Coercions for Haskell", section 6.5:
-- --   http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf
-- 
-- {-# RULES "map/coerce" [1] map coerce = coerce #-}
-- 
-- 
-- ----------------------------------------------
-- --              append
-- ----------------------------------------------
-- 
-- -- | Append two lists, i.e.,
-- --
-- -- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- -- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
-- --
-- -- If the first list is not finite, the result is the first list.
-- 
(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
--                              -- It's recursive, so won't inline anyway,
--                              -- but saying so is more explicit
(++) xs ys = 
    let append [] ys = ys
        append (x:xs) ys = x : (append xs ys)
    in case typeIndex# xs `adjStr` xs `adjStr` ys of
        1# -> strAppend# xs ys
        _ -> append xs ys
-- 
-- (++) [] ys = ys
-- (++) (x:xs) ys = x : xs ++ ys
{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}
-- 
-- 
-- -- |'otherwise' is defined as the value 'True'.  It helps to make
-- -- guards more readable.  eg.
-- --
-- -- >  f x | x < 0     = ...
-- -- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True
-- 
-- ----------------------------------------------
-- -- Type Char and String
-- ----------------------------------------------
-- 
-- unsafeChr :: Int -> Char
-- unsafeChr (I# i#) = C# (chr# i#)
-- 
-- -- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
-- 
-- -- | This 'String' equality predicate is used when desugaring
-- -- pattern-matches against strings.
eqString :: String -> String -> Bool
eqString xs ys =
        let
            eqString []       []       = True
            eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
            eqString _        _        = False
        in
        case typeIndex# xs `adjStr` xs `adjStr` ys  of
                1# -> strEq# xs ys
                _ -> eqString xs ys

{-# RULES "eqString" (==) = eqString #-}
-- -- eqString also has a BuiltInRule in PrelRules.lhs:
-- --      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2
-- 
-- 
-- ----------------------------------------------
-- -- 'Int' related definitions
-- ----------------------------------------------
-- 
maxInt, minInt :: Int
-- 
-- {- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
-- #if WORD_SIZE_IN_BITS == 31
-- minInt  = I# (-0x40000000#)
-- maxInt  = I# 0x3FFFFFFF#
-- #elif WORD_SIZE_IN_BITS == 32
-- minInt  = I# (-0x80000000#)
-- maxInt  = I# 0x7FFFFFFF#
-- #else
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#
-- #endif
-- 
----------------------------------------------
-- The function type
----------------------------------------------


-- Assertion function.  This simply ignores its boolean argument.
-- The compiler may rewrite it to @('assertError' line)@.

-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'AssertionFailed' exception is raised,
-- containing a 'String' with the source file and line number of the
-- call to 'assert'.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless optimisation is turned on
-- with @-O@ or the @-fignore-asserts@
-- option is given).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.

--      SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
--      but from Template Haskell onwards it's simply
--      defined here in Base.lhs
assert :: Bool -> a -> a
assert _pred r = r
-- 
-- breakpoint :: a -> a
-- breakpoint r = r
-- 
-- breakpointCond :: Bool -> a -> a
-- breakpointCond _ r = r
-- 
-- data Opaque = forall a. O a
-- 
-- 
-- 
-- 

-- 
-- -- | Strict (call-by-value) application operator. It takes a function and an
-- -- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- -- the function with that value.
-- 


-- 
-- -- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until                   :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x | p x          = x
         | otherwise    = go (f x)
-- 
-- -- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- -- used as an infix operator, and its typing forces its first argument
-- -- (which is usually overloaded) to have the same type as the second.
asTypeOf                :: a -> a -> a
asTypeOf                =  const
-- 
-- ----------------------------------------------
-- -- Functor/Applicative/Monad instances for IO
-- ----------------------------------------------
-- 

-- {-# SPECIALISE liftA :: (a1->r) -> IO a1 -> IO r #-}
{-# SPECIALISE liftA :: (a1->r) -> Maybe a1 -> Maybe r #-}
-- {-# SPECIALISE liftA2 :: (a1->a2->r) -> IO a1 -> IO a2 -> IO r #-}
{-# SPECIALISE liftA2 :: (a1->a2->r) -> Maybe a1 -> Maybe a2 -> Maybe r #-}
-- {-# SPECIALISE liftA3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
{-# SPECIALISE liftA3 :: (a1->a2->a3->r) ->
                                Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}

instance  Functor IO where
   fmap f x = x >>= (pure . f)

instance Applicative IO where
    {-# INLINE pure #-}
    {-# INLINE (*>) #-}
    pure   = returnIO
    m *> k = m >>= \ _ -> k
    (<*>)  = ap
-- 
instance  Monad IO  where
    -- {-# INLINE (>>)   #-}
    -- {-# INLINE (>>=)  #-}
    -- (>>)      = (*>)
    (>>=)     = bindIO
    -- fail s    = failIO s
-- 
-- instance Alternative IO where
--     empty = failIO "mzero"
--     (<|>) = mplusIO
-- 
-- instance MonadPlus IO
-- 
returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))
-- 
bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)
-- 
-- thenIO :: IO a -> IO b -> IO b
-- thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)
-- 
unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a
-- 
-- {- |
-- Returns the 'tag' of a constructor application; this function is used
-- by the deriving code for Eq, Ord and Enum.
-- 
-- The primitive dataToTag# requires an evaluated constructor application
-- as its argument, so we provide getTag as a wrapper that performs the
-- evaluation before calling dataToTag#.  We could have dataToTag#
-- evaluate its argument, but we prefer to do it this way because (a)
-- dataToTag# can be an inline primop if it doesn't need to do any
-- evaluation, and (b) we want to expose the evaluation to the
-- simplifier, because it might be possible to eliminate the evaluation
-- in the case when the argument is already known to be evaluated.
-- -}
-- {-# INLINE getTag #-}
-- getTag :: a -> Int#
-- getTag !x = dataToTag# x
-- 
-- ----------------------------------------------
-- -- Numeric primops
-- ----------------------------------------------
-- 
-- -- Definitions of the boxed PrimOps; these will be
-- -- used in the case of partial applications, etc.
-- 
{-# INLINE quotInt #-}
{-# INLINE remInt #-}
-- 
quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# 0#) = error "quotInt: quot by 0"
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)

(I# x) `remInt`   (I# 0#) = error "remInt: div by 0"
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)

(I# x) `divInt`   (I# 0#) = error "divInt: div by 0"
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)

(I# x) `modInt`   (I# 0#) = error "modInt: div by 0"
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)
-- 
quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# 0#) = error "quotRemInt: div by 0"
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)
-- 
divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# 0#) = error "divModInt: div by 0"
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)
-- 
divModInt# :: Int# -> Int# -> (# Int#, Int# #)
divModInt# = divModInt#
-- x# `divModInt#` y#
--  | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) =
--                                     case (x# -# 1#) `quotRemInt#` y# of
--                                       (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
--  | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) =
--                                     case (x# +# 1#) `quotRemInt#` y# of
--                                       (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
--  | otherwise                                =
--                                     x# `quotRemInt#` y#
-- 
-- -- Wrappers for the shift operations.  The uncheckedShift# family are
-- -- undefined when the amount being shifted by is greater than the size
-- -- in bits of Int#, so these wrappers perform a check and return
-- -- either zero or -1 appropriately.
-- --
-- -- Note that these wrappers still produce undefined results when the
-- -- second argument (the shift amount) is negative.
-- 
-- -- | Shift the argument left by the specified number of bits
-- -- (which must be non-negative).
-- shiftL# :: Word# -> Int# -> Word#
-- a `shiftL#` b   | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0##
--                 | otherwise                          = a `uncheckedShiftL#` b
-- 
-- -- | Shift the argument right by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- -- (although an arithmetic right shift wouldn't make sense for Word#)
-- shiftRL# :: Word# -> Int# -> Word#
-- a `shiftRL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0##
--                 | otherwise                          = a `uncheckedShiftRL#` b
-- 
-- -- | Shift the argument left by the specified number of bits
-- -- (which must be non-negative).
-- iShiftL# :: Int# -> Int# -> Int#
-- a `iShiftL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0#
--                 | otherwise                          = a `uncheckedIShiftL#` b
-- 
-- -- | Shift the argument right (signed) by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RA" means "right, arithmetic" (as opposed to RL for logical)
-- iShiftRA# :: Int# -> Int# -> Int#
-- a `iShiftRA#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = if isTrue# (a <# 0#)
--                                                           then (-1#)
--                                                           else 0#
--                 | otherwise                          = a `uncheckedIShiftRA#` b
-- 
-- -- | Shift the argument right (unsigned) by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- iShiftRL# :: Int# -> Int# -> Int#
-- a `iShiftRL#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0#
--                 | otherwise                          = a `uncheckedIShiftRL#` b
-- 
-- -- Rules for C strings (the functions themselves are now in GHC.CString)
-- {-# RULES
-- "unpack"       [~1] forall a   . unpackCString# a             = build (unpackFoldrCString# a)
-- "unpack-list"  [1]  forall a   . unpackFoldrCString# a (:) [] = unpackCString# a
-- "unpack-append"     forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n
-- 
-- -- There's a built-in rule (in PrelRules.lhs) for
-- --      unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n
-- 
--   #-}

