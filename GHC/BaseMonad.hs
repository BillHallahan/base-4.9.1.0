{-# LANGUAGE BangPatterns, CPP, KindSignatures,
    NoImplicitPrelude, PackageImports, PolyKinds, RankNTypes,
    DataKinds, PolyKinds #-}

module GHC.BaseMonad ( String
                     , id
                     , (.)
                     , const
                     , flip
                     , ($)
                     , ($!)
                     , GHCB.Functor (..)
                     , GHCB.Applicative (..)
                     , (GHCB.<**>)
                     , GHCB.liftA
                    --  , GHCB.liftA2
                     , GHCB.liftA3
                     , map
                     , GHCB.Monad (..)) where

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

#if __GLASGOW_HASKELL__ >= 806
import GHC.Maybe
#endif

import qualified "base" GHC.Base as GHCB

infixr 0  $, $!
-- 
infixl 4 <*>, <*, *>, <**>
infixr 9  .
infixl 1  >>, >>=
infixl 4  <$

-- 
-- -- | A 'String' is a list of characters.  String constants in Haskell are values
-- -- of type 'String'.
-- --
type String = [Char]

-- -- | Identity function.
id                      :: a -> a
id x                    =  x

-- -- | Function composition.
{-# INLINE (.) #-}
-- -- Make sure it has TWO args only on the left, so that it inlines
-- -- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- -- | @const x@ is a unary function which evaluates to @x@ for all inputs.
-- --
-- -- For instance,
-- --
-- -- >>> map (const 42) [0..3]
-- -- [42,42,42,42]
const                   :: a -> b -> a
const x _               =  x

-- -- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- -- | Application operator.  This operator is redundant, since ordinary
-- -- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- -- low, right-associative binding precedence, so it sometimes allows
-- -- parentheses to be omitted; for example:
-- --
-- -- >     f $ g $ h x  =  f (g (h x))
-- --
-- -- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- -- or @'Data.List.zipWith' ('$') fs xs@.
{-# INLINE ($) #-}
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
($) :: forall repa repb (a :: TYPE repa) (b :: TYPE repb). (a -> b) -> a -> b
($) f = f
#else
($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $ x = f x
#endif

($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $! x = let !vx = x in f vx  -- see #2273

-- {- | The 'Functor' class is used for types that can be mapped over.
-- Instances of 'Functor' should satisfy the following laws:
-- 
-- > fmap id  ==  id
-- > fmap (f . g)  ==  fmap f . fmap g
-- 
-- The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
-- satisfy these laws.
-- -}
-- 
class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b
-- 
--     -- | Replace all locations in the input with the same value.
--     -- The default definition is @'fmap' . 'const'@, but this may be
--     -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const
-- 
-- -- | A functor with application, providing operations to
-- --
-- -- * embed pure expressions ('pure'), and
-- --
-- -- * sequence computations and combine their results ('<*>').
-- --
-- -- A minimal complete definition must include implementations of these
-- -- functions satisfying the following laws:
-- --
-- -- [/identity/]
-- --
-- --      @'pure' 'id' '<*>' v = v@
-- --
-- -- [/composition/]
-- --
-- --      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
-- --
-- -- [/homomorphism/]
-- --
-- --      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
-- --
-- -- [/interchange/]
-- --
-- --      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
-- --
-- -- The other methods have the following default definitions, which may
-- -- be overridden with equivalent specialized implementations:
-- --
-- --   * @u '*>' v = 'pure' ('const' 'id') '<*>' u '<*>' v@
-- --
-- --   * @u '<*' v = 'pure' 'const' '<*>' u '<*>' v@
-- --
-- -- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
-- --
-- --   * @'fmap' f x = 'pure' f '<*>' x@
-- --
-- -- If @f@ is also a 'Monad', it should satisfy
-- --
-- --   * @'pure' = 'return'@
-- --
-- --   * @('<*>') = 'ap'@
-- --
-- -- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).
-- 
{-# INLINEABLE liftA #-}
{-# INLINEABLE liftA3 #-}

class Functor f => Applicative f where
--     -- | Lift a value.
    pure :: a -> f a
-- 
--     -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

#if MIN_VERSION_base(4,10,0)
-- -- | Lift a binary function to actions.
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f a b = fmap f a <*> b
    {-# INLINEABLE liftA2 #-}
#endif
-- 
--     -- | Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
--     -- This is essentially the same as liftA2 (const id), but if the
--     -- Functor instance has an optimized (<$), we want to use that instead.
-- 
--     -- | Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const
-- 
-- -- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
-- 
-- -- | Lift a function to actions.
-- -- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a
-- -- Caution: since this may be used for `fmap`, we can't use the obvious
-- -- definition of liftA = fmap.
-- 
#if MIN_VERSION_base(4,10,0)
#else
-- -- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b
#endif
-- 
-- -- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = fmap f a <*> b <*> c
-- 
-- 
-- 
-- -- | The 'join' function is the conventional monad join operator. It
-- -- is used to remove one level of monadic structure, projecting its
-- -- bound argument into the outer level.
-- join              :: (Monad m) => m (m a) -> m a
-- join x            =  x >>= id
-- 
-- {- | The 'Monad' class defines the basic operations over a /monad/,
-- a concept from a branch of mathematics known as /category theory/.
-- From the perspective of a Haskell programmer, however, it is best to
-- think of a monad as an /abstract datatype/ of actions.
-- Haskell's @do@ expressions provide a convenient syntax for writing
-- monadic expressions.
-- 
-- Instances of 'Monad' should satisfy the following laws:
-- 
-- * @'return' a '>>=' k  =  k a@
-- * @m '>>=' 'return'  =  m@
-- * @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@
-- 
-- Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:
-- 
-- * @'pure' = 'return'@
-- * @('<*>') = 'ap'@
-- 
-- The above laws imply:
-- 
-- * @'fmap' f xs  =  xs '>>=' 'return' . f@
-- * @('>>') = ('*>')@
-- 
-- and that 'pure' and ('<*>') satisfy the applicative functor laws.
-- 
-- The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
-- defined in the "Prelude" satisfy these laws.
-- -}
class Applicative m => Monad m where
--     -- | Sequentially compose two actions, passing any value produced
--     -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
-- 
--     -- | Sequentially compose two actions, discarding any value produced
--     -- by the first, like sequencing operators (such as the semicolon)
--     -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}
-- 
--     -- | Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure
-- 
--     -- | Fail with a message.  This operation is not part of the
--     -- mathematical definition of a monad, but is invoked on pattern-match
--     -- failure in a @do@ expression.
--     --
--     -- As part of the MonadFail proposal (MFP), this function is moved
--     -- to its own class 'MonadFail' (see "Control.Monad.Fail" for more
--     -- details). The definition here will be removed in a future
--     -- release.
#if MIN_VERSION_base(4,13,0)
#else
    fail        :: String -> m a
    fail s      = errorWithoutStackTrace s
#endif

instance Functor ((->) r) where
    fmap = (.)
-- 
instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)
-- 
instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

-- -- | 'map' @f xs@ is the list obtained by applying @f@ to each element
-- -- of @xs@, i.e.,
-- --
-- -- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- -- > map f [x1, x2, ...] == [f x1, f x2, ...]
-- 
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
--   -- We want the RULEs "map" and "map/coerce" to fire first.
--   -- map is recursive, so won't inline anyway,
--   -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs

instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map
-- 
-- -- See Note: [List comprehensions and inlining]
instance Applicative [] where
    {-# INLINE pure #-}
    pure x    = [x]
    {-# INLINE (<*>) #-}
    fs <*> xs = [f x | f <- fs, x <- xs]
    {-# INLINE (*>) #-}
    xs *> ys  = [y | _ <- xs, y <- ys]
-- 
-- -- See Note: [List comprehensions and inlining]
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    {-# INLINE (>>) #-}
    (>>) = (*>)
    -- {-# INLINE fail #-}
    -- fail _              = []

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)
