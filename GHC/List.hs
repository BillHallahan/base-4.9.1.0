{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, MagicHash #-}
{-# LANGUAGE BangPatterns, TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  GHC.List
-- -- Copyright   :  (c) The University of Glasgow 1994-2002
-- -- License     :  see libraries/base/LICENSE
-- --
-- -- Maintainer  :  cvs-ghc@haskell.org
-- -- Stability   :  internal
-- -- Portability :  non-portable (GHC Extensions)
-- --
-- -- The List data type and its operations
-- --
-- -----------------------------------------------------------------------------
-- 
module GHC.List (
--    -- [] (..),          -- built-in syntax; can't be used in export list
-- 
   map, (++), filter, concat,
   head, last, tail, init, uncons, unsnoc, null, length, (!!), (!?),
   foldl, foldl', foldl1, foldl1', scanl, scanl1, scanl', foldr, foldr1,
   scanr, scanr1, iterate, repeat, replicate, cycle,
   take, drop, sum, product, maximum, minimum, splitAt, takeWhile, dropWhile,
   span, break, reverse, and, or,
   any, all, elem, notElem, lookup,
   concatMap,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
   errorEmptyList,
-- 
 ) where
-- 
import Data.Maybe
import GHC.Base
import GHC.Num (Num(..))
-- import GHC.Integer (Integer)
import GHC.Integer2
import GHC.Prim2

import GHC.Stack.Types
-- 
infixl 9  !!, !?
infix  4 `elem`, `notElem`
-- 
-- --------------------------------------------------------------
-- -- List-manipulation functions
-- --------------------------------------------------------------
-- 
-- -- | Extract the first element of a list, which must be non-empty.
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
head                    :: HasCallStack => [a] -> a
#else
head                    :: [a] -> a
#endif
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}
-- 
badHead :: a
-- badHead = errorEmptyList "head"
badHead = errorEmptyList "head"
-- 
-- -- This rule is useful in cases like
-- --      head [y | (x,y) <- ps, x==t]
{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b->b) .
                head (build g) = g (\x _ -> x) badHead
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}
-- 
-- -- | Decompose a list into its head and tail. If the list is empty,
-- -- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- -- where @x@ is the head of the list and @xs@ its tail.
-- --
-- -- @since 4.8.0.0
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)
-- 
-- Decompose a list into 'init' and 'last'.
--
-- * If the list is empty, returns 'Nothing'.
-- * If the list is non-empty, returns @'Just' (xs, x)@,
-- where @xs@ is the 'init'ial part of the list and @x@ is its 'last' element.
--
--
-- 'unsnoc' is dual to 'uncons': for a finite list @xs@
--
-- > unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)
--
unsnoc :: [a] -> Maybe ([a], a)
-- The lazy pattern ~(a, b) is important to be productive on infinite lists
-- and not to be prone to stack overflows.
-- Expressing the recursion via 'foldr' provides for list fusion.
unsnoc xs = let
                unsnoc' = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
                strUnsnoc xs = let !len = strLen# xs
                                   !end = len -# 1#
                                   !end_lst = strAt# xs end
                               in
                               case end_lst of
                                   (x:_) -> Just (strSubstr# xs 0# end, x)
                                   _ -> Nothing
            in
            case typeIndex# xs `adjStr` xs of
                1# -> strUnsnoc xs
                _ -> unsnoc' xs
{-# INLINABLE unsnoc #-}

-- -- | Extract the elements after the head of a list, which must be non-empty.
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
tail                    :: HasCallStack => [a] -> [a]
#else
tail                    :: [a] -> [a]
#endif
tail (_:xs)             =  xs
-- tail []                 =  errorEmptyList "tail"
tail []                 = errorEmptyList "tail"
-- 
-- -- | Extract the last element of a list, which must be finite and non-empty.
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
last                    :: HasCallStack => [a] -> a
#else
last                    :: [a] -> a
#endif
last ys =
    let
        -- #ifdef USE_REPORT_PRELUDE
        last' [x]                =  x
        last' (_:xs)             =  last' xs
        -- last []                 =  errorEmptyList "last"
        last' []                 =  errorEmptyList "last"

        strLast zs = let !len = strLen# zs
                         !end = len -# 1#
                         !i = strAt# zs end in
                     case i of
                          (h:_) -> h
                          _ -> errorEmptyList "last"
    in
    case typeIndex# ys `adjStr` ys of
        1# -> strLast ys
        _ -> last' ys
-- #else
-- -- Use foldl to make last a good consumer.
-- -- This will compile to good code for the actual GHC.List.last.
-- -- (At least as long it is eta-expaned, otherwise it does not, #10260.)
-- last xs = foldl (\_ x -> x) lastError xs
-- {-# INLINE last #-}
-- -- The inline pragma is required to make GHC remember the implementation via
-- -- foldl.
-- lastError :: a
-- lastError = errorEmptyList "last"
-- #endif
-- 
-- -- | Return all the elements of a list except the last one.
-- -- The list must be non-empty.
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
init                    :: HasCallStack => [a] -> [a]
#else
init                    :: [a] -> [a]
#endif
-- #ifdef USE_REPORT_PRELUDE
init ys =
    let
        init' [x]                =  []
        init' (x:xs)             =  x : init' xs
        -- init []                 =  errorEmptyList "init"
        init' []                 =  errorEmptyList "init"
    in case typeIndex# ys `adjStr` ys of
          1# -> case ys of
                    [] -> errorEmptyList "init"
                    _ -> let !len = strLen# ys; !end = len -# 1# in strSubstr# ys 0# end
          _ -> init' ys
-- #else
-- -- eliminate repeated cases
-- init []                 =  errorEmptyList "init"
-- init (x:xs)             =  init' x xs
--   where init' _ []     = []
--         init' y (z:zs) = y : init' z zs
-- #endif
-- 
-- -- | Test whether a list is empty.
null                    :: [a] -> Bool
null xs =
    let
        null' []                 =  True
        null' (_:_)              =  False
    in
    case typeIndex# xs `adjStr` xs of
        1# -> let !len = strLen# xs in len $==# 0#
        _ -> null' xs
-- 
-- -- | /O(n)/. 'length' returns the length of a finite list as an 'Int'.
-- -- It is an instance of the more general 'Data.List.genericLength',
-- -- the result type of which may be any kind of number.
{-# NOINLINE [1] length #-}
length :: [a] -> Int
length xs =
    case typeIndex# xs `adjStr` xs of
        1# -> I# (strLen# xs)
        _ -> length' xs

length'                  :: [a] -> Int
-- length xs               = lenAcc xs 0
length' xs               = lenAcc xs (fromInteger zeroInteger)
-- 
lenAcc          :: [a] -> Int -> Int
lenAcc []     n = n
-- lenAcc (_:ys) n = lenAcc ys (n+1)
lenAcc (_:ys) !n = lenAcc ys (n+(fromInteger (Z# 1#)))
-- 
{-# RULES
-- "length" [~1] forall xs . length xs = foldr lengthFB idLength xs 0
"length" [~1] forall xs . length xs = foldr lengthFB idLength xs (fromInteger zeroInteger)
"lengthList" [1] foldr lengthFB idLength = lenAcc
 #-}
-- 
-- -- The lambda form turns out to be necessary to make this inline
-- -- when we need it to and give good performance.
{-# INLINE [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
-- lengthFB _ r = \ !a -> r (a + 1)
lengthFB _ r = \ !a -> r (a + fromInteger oneInteger)
-- 
{-# INLINE [0] idLength #-}
idLength :: Int -> Int
idLength = id
-- 
-- -- | 'filter', applied to a predicate and a list, returns the list of
-- -- those elements that satisfy the predicate; i.e.,
-- --
-- -- > filter p xs = [ x | x <- xs, p x]
-- 
{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
-- 
{-# NOINLINE [0] filterFB #-}
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r | p x       = x `c` r
                 | otherwise = r
-- 
{-# RULES
"filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
 #-}
-- 
-- -- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
-- --     filterFB (filterFB c p) q a b
-- --   = if q a then filterFB c p a b else b
-- --   = if q a then (if p a then c a b else b) else b
-- --   = if q a && p a then c a b else b
-- --   = filterFB c (\x -> q x && p x) a b
-- -- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- -- gave rise to a live bug report.  SLPJ.
-- 
-- 
-- -- | 'foldl', applied to a binary operator, a starting value (typically
-- -- the left-identity of the operator), and a list, reduces the list
-- -- using the binary operator, from left to right:
-- --
-- -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- --
-- -- The list must be finite.
-- 
-- -- We write foldl as a non-recursive thing, so that it
-- -- can be inlined, and then (often) strictness-analysed,
-- -- and hence the classic space leak on foldl (+) 0 xs
-- 
foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl #-}
foldl k z0 xs =
  foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> fn (k z v))) (id :: b -> b) xs z0
--   -- See Note [Left folds via right fold]
-- 
-- {-
-- Note [Left folds via right fold]
-- 
-- Implementing foldl et. al. via foldr is only a good idea if the compiler can
-- optimize the resulting code (eta-expand the recursive "go"). See #7994.
-- We hope that one of the two measure kick in:
-- 
--    * Call Arity (-fcall-arity, enabled by default) eta-expands it if it can see
--      all calls and determine that the arity is large.
--    * The oneShot annotation gives a hint to the regular arity analysis that
--      it may assume that the lambda is called at most once.
--      See [One-shot lambdas] in CoreArity and especially [Eta expanding thunks]
--      in CoreArity.
-- 
-- The oneShot annotations used in this module are correct, as we only use them in
-- argumets to foldr, where we know how the arguments are called.
-- -}
-- 
-- -- ----------------------------------------------------------------------------
-- 
-- -- | A strict version of 'foldl'.
foldl'           :: forall a b . (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl' #-}
foldl' k z0 xs =
  foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> z `seq` fn (k z v))) (id :: b -> b) xs z0
--   -- See Note [Left folds via right fold]
-- 
-- -- | 'foldl1' is a variant of 'foldl' that has no starting value argument,
-- -- and thus must be applied to non-empty lists.
foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
-- foldl1 _ []             =  errorEmptyList "foldl1"
foldl1 _ []             =  errorEmptyList "foldl1"
-- 
-- -- | A strict version of 'foldl1'
foldl1'                  :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs)         =  foldl' f x xs
-- foldl1' _ []             =  errorEmptyList "foldl1'"
foldl1' _ []             =  errorEmptyList "foldl1'"
-- 
-- -- -----------------------------------------------------------------------------
-- -- List sum and product
-- 
-- -- | The 'sum' function computes the sum of a finite list of numbers.
sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl (+) (fromInteger zeroInteger)
-- 
-- -- | The 'product' function computes the product of a finite list of numbers.
product                 :: (Num a) => [a] -> a
{-# INLINE product #-}
product                 =  foldl (*) (fromInteger oneInteger)
-- 
-- -- | 'scanl' is similar to 'foldl', but returns a list of successive
-- -- reduced values from the left:
-- --
-- -- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- --
-- -- Note that
-- --
-- -- > last (scanl f z xs) == foldl f z xs.
-- 
-- -- This peculiar arrangement is necessary to prevent scanl being rewritten in
-- -- its own right-hand side.
{-# NOINLINE [1] scanl #-}
scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl                   = scanlGo
  where
    scanlGo           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo f q ls    = q : (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f q x) xs)
-- 
-- -- Note [scanl rewrite rules]
-- {-# RULES
-- "scanl"  [~1] forall f a bs . scanl f a bs =
--   build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)
-- "scanlList" [1] forall f (a::a) bs .
--     foldr (scanlFB f (:)) (constScanl []) bs a = tail (scanl f a bs)
--  #-}
-- 
{-# INLINE [0] scanlFB #-}
scanlFB :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB f c = \b g -> oneShot (\x -> let b' = f x b in b' `c` g b')
--   -- See Note [Left folds via right fold]
-- 
{-# INLINE [0] constScanl #-}
constScanl :: a -> b -> a
constScanl = const
-- 
-- 
-- -- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
-- --
-- -- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
-- 
scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []
-- 
-- -- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
-- -- This peculiar form is needed to prevent scanl' from being rewritten
-- -- in its own right hand side.
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)
-- 
-- -- Note [scanl rewrite rules]
-- {-# RULES
-- "scanl'"  [~1] forall f a bs . scanl' f a bs =
--   build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
-- "scanlList'" [1] forall f a bs .
--     foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
--  #-}
-- 
{-# INLINE [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> oneShot (\x -> let !b' = f x b in b' `c` g b')
--   -- See Note [Left folds via right fold]
-- 
{-# INLINE [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' a !_b = a
-- 
-- {-
-- Note [scanl rewrite rules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
-- In most cases, when we rewrite a form to one that can fuse, we try to rewrite it
-- back to the original form if it does not fuse. For scanl, we do something a
-- little different. In particular, we rewrite
-- 
-- scanl f a bs
-- 
-- to
-- 
-- build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)
-- 
-- When build is inlined, this becomes
-- 
-- a : foldr (scanlFB f (:)) (constScanl []) bs a
-- 
-- To rewrite this form back to scanl, we would need a rule that looked like
-- 
-- forall f a bs. a : foldr (scanlFB f (:)) (constScanl []) bs a = scanl f a bs
-- 
-- The problem with this rule is that it has (:) at its head. This would have the
-- effect of changing the way the inliner looks at (:), not only here but
-- everywhere.  In most cases, this makes no difference, but in some cases it
-- causes it to come to a different decision about whether to inline something.
-- Based on nofib benchmarks, this is bad for performance. Therefore, we instead
-- match on everything past the :, which is just the tail of scanl.
-- -}
-- 
-- -- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- -- above functions.
-- 
-- -- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- -- and thus must be applied to non-empty lists.
-- 
foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 f = go
  where go [x]            =  x
        go (x:xs)         =  f x (go xs)
        -- go []             =  errorEmptyList "foldr1"
        go []             =  errorEmptyList "foldr1"
{-# INLINE [0] foldr1 #-}
-- 
-- -- | 'scanr' is the right-to-left dual of 'scanl'.
-- -- Note that
-- --
-- -- > head (scanr f z xs) == foldr f z xs.
{-# NOINLINE [1] scanr #-}
scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs
-- 
{-# INLINE [0] strictUncurryScanr #-}
strictUncurryScanr :: (a -> b -> c) -> (a, b) -> c
strictUncurryScanr f pair = case pair of
                              (x, y) -> f x y
-- 
{-# INLINE [0] scanrFB #-}
scanrFB :: (a -> b -> b) -> (b -> c -> c) -> a -> (b, c) -> (b, c)
scanrFB f c = \x (r, est) -> (f x r, r `c` est)
-- 
{-# RULES
"scanr" [~1] forall f q0 ls . scanr f q0 ls =
  build (\c n -> strictUncurryScanr c (foldr (scanrFB f c) (q0,n) ls))
"scanrList" [1] forall f q0 ls .
              strictUncurryScanr (:) (foldr (scanrFB f (:)) (q0,[]) ls) =
                 scanr f q0 ls
 #-}
-- 
-- -- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs
-- 
-- -- | 'maximum' returns the maximum value from a list,
-- -- which must be non-empty, finite, and of an ordered type.
-- -- It is a special case of 'Data.List.maximumBy', which allows the
-- -- programmer to supply their own comparison function.
{-# INLINEABLE maximum #-}
maximum                 :: forall a . (Ord a) => [a] -> a
maximum xs =
    let
        strMaximum =
            let
                !y = symgen @a
                y_list = [y]
                !index_y = strIndexOf# xs y_list 0#
                !index_non_neg = index_y $>=# 0#

                !sl_xs = strLen# xs
                y_is_min i = 0# $<=# i &&# i $<# sl_xs ==> y_list `strGe#` strAt# xs i
            in
            assume (index_non_neg) . assume (forAllInt# y_is_min) $ y

        maximum' []              =  errorEmptyList "maximum"
        maximum' xs'              =  foldl1 max xs'
    in
    case strQuantifiers (typeIndex# xs `adjStr` xs) of
        1# -> strMaximum
        _ -> maximum' xs

-- 
-- -- We want this to be specialized so that with a strict max function, GHC
-- -- produces good code. Note that to see if this is happending, one has to
-- -- look at -ddump-prep, not -ddump-core!
{-# SPECIALIZE  maximum :: [Int] -> Int #-}
-- {-# SPECIALIZE  maximum :: [Integer] -> Integer #-}
-- 
-- -- | 'minimum' returns the minimum value from a list,
-- -- which must be non-empty, finite, and of an ordered type.
-- -- It is a special case of 'Data.List.minimumBy', which allows the
-- -- programmer to supply their own comparison function.
{-# INLINEABLE minimum #-}
minimum                 :: forall a . (Ord a) => [a] -> a
minimum xs =
    let
        strMinimum =
            let
                !y = symgen @a
                y_list = [y]
                !index_y = strIndexOf# xs y_list 0#
                !index_non_neg = index_y $>=# 0#

                !sl_xs = strLen# xs
                y_is_min i = 0# $<=# i &&# i $<# sl_xs ==> y_list `strLe#` strAt# xs i
            in
            assume (index_non_neg) . assume (forAllInt# y_is_min) $ y
            
        minimum' []              =  errorEmptyList "minimum"
        minimum' xs'              =  foldl1 min xs'
    in
    case strQuantifiers (typeIndex# xs `adjStr` xs) of
        1# -> strMinimum
        _ -> minimum' xs
-- 
{-# SPECIALIZE  minimum :: [Int] -> Int #-}
-- {-# SPECIALIZE  minimum :: [Integer] -> Integer #-}
-- 
-- 
-- -- | 'iterate' @f x@ returns an infinite list of repeated applications
-- -- of @f@ to @x@:
-- --
-- -- > iterate f x == [x, f x, f (f x), ...]
-- 
{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
-- 
{-# NOINLINE [0] iterateFB #-}
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where go x = x `c` go (f x)
-- 
{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:) = iterate
 #-}
-- 
-- 
-- -- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- -- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs
-- 
{-# INLINE [0] repeatFB #-}     -- ditto
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs
-- 
-- 
{-# RULES
"repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
"repeatFB"  [1]  repeatFB (:)       = repeat
 #-}
-- 
-- -- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- -- every element.
-- -- It is an instance of the more general 'Data.List.genericReplicate',
-- -- in which @n@ may be of any integral type.
{-# INLINE replicate #-}
replicate               :: forall a . Int -> a -> [a]
replicate n x           =
    let
      potential_str = (x:[])

      rep n x = take n (repeat x)

      smt_rep_quant =
          let
              I# len = n
              !xs = symgen @[a]

              !sl_xs = strLen# xs
              rep_prop1 = sl_xs $==# len
              rep_prop2 i = 0# $<=# i &&# i $<# sl_xs ==> strAt# xs i `strEq#`potential_str
          in
          assume rep_prop1 (assume (forAllInt# rep_prop2) xs)

      -- Non-infinite version for SMT Strings
      -- Not an optimization- needed to prevent infinite computation,
      -- otherwise genericTake will try to fully evaluate `repeat x`
      smt_rep n x = map (const x) [1..n]
    in
    case typeIndex# potential_str `adjStr` potential_str of
        1# -> case strQuantifiers 1# of
                1# -> smt_rep_quant
                _ -> smt_rep n x
        _ -> rep n x
-- 
-- -- | 'cycle' ties a finite list into a circular one, or equivalently,
-- -- the infinite repetition of the original list.  It is the identity
-- -- on infinite lists.
-- 
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
cycle                   :: HasCallStack => [a] -> [a]
#else
cycle                   :: [a] -> [a]
#endif
-- cycle []                = errorEmptyList "cycle"
cycle []                = errorEmptyList "cycle"
cycle xs                = xs' where xs' = xs ++ xs'
-- 
-- -- | 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
-- -- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@:
-- --
-- -- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- -- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- -- > takeWhile (< 0) [1,2,3] == []
-- --
-- 
{-# NOINLINE [1] takeWhile #-}
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []
-- 
{-# INLINE [0] takeWhileFB #-}
takeWhileFB :: (a -> Bool) -> (a -> b -> b) -> b -> a -> b -> b
takeWhileFB p c n = \x r -> if p x then x `c` r else n
-- 
-- -- The takeWhileFB rule is similar to the filterFB rule. It works like this:
-- -- takeWhileFB q (takeWhileFB p c n) n =
-- -- \x r -> if q x then (takeWhileFB p c n) x r else n =
-- -- \x r -> if q x then (\x' r' -> if p x' then x' `c` r' else n) x r else n =
-- -- \x r -> if q x then (if p x then x `c` r else n) else n =
-- -- \x r -> if q x && p x then x `c` r else n =
-- -- takeWhileFB (\x -> q x && p x) c n
{-# RULES
"takeWhile"     [~1] forall p xs. takeWhile p xs =
                                build (\c n -> foldr (takeWhileFB p c n) n xs)
"takeWhileList" [1]  forall p.    foldr (takeWhileFB p (:) []) [] = takeWhile p
"takeWhileFB"        forall c n p q. takeWhileFB q (takeWhileFB p c n) n =
                        takeWhileFB (\x -> q x && p x) c n
 #-}
-- 
-- -- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@:
-- --
-- -- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
-- -- > dropWhile (< 9) [1,2,3] == []
-- -- > dropWhile (< 0) [1,2,3] == [1,2,3]
-- --
-- 
dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs
-- 
-- -- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- -- of length @n@, or @xs@ itself if @n > 'length' xs@:
-- --
-- -- > take 5 "Hello World!" == "Hello"
-- -- > take 3 [1,2,3,4,5] == [1,2,3]
-- -- > take 3 [1,2] == [1,2]
-- -- > take 3 [] == []
-- -- > take (-1) [1,2] == []
-- -- > take 0 [1,2] == []
-- --
-- -- It is an instance of the more general 'Data.List.genericTake',
-- -- in which @n@ may be of any integral type.
take                   :: Int -> [a] -> [a]
take n xs = let
                take' n _ | n <= (fromInteger zeroInteger) =  []
                take' _ [] =  []
                take' n (x:xs) = x : take' (n - (fromInteger oneInteger)) xs
                
                I# n' = n
            in case typeIndex# xs `adjStr` xs of
                1# -> strSubstr# xs 0# n'
                _ -> take' n xs
            
-- #ifdef USE_REPORT_PRELUDE
-- take n _      | n <= (fromInteger zeroInteger) =  []
-- take _ []              =  []
-- take n (x:xs)          =  x : take (n-(fromInteger oneInteger)) xs
-- #else
-- 
-- {- We always want to inline this to take advantage of a known length argument
-- sign. Note, however, that it's important for the RULES to grab take, rather
-- than trying to INLINE take immediately and then letting the RULES grab
-- unsafeTake. Presumably the latter approach doesn't grab it early enough; it led
-- to an allocation regression in nofib/fft2. -}
-- {-# INLINE [1] take #-}
-- take n xs | 0 < n     = unsafeTake n xs
--           | otherwise = []
-- 
-- -- A version of take that takes the whole list if it's given an argument less
-- -- than 1.
-- {-# NOINLINE [1] unsafeTake #-}
-- unsafeTake :: Int -> [a] -> [a]
-- unsafeTake !_  []     = []
-- unsafeTake 1   (x: _) = [x]
-- unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs
-- 
-- {-# RULES
-- "take"     [~1] forall n xs . take n xs =
--   build (\c nil -> if 0 < n
--                    then foldr (takeFB c nil) (flipSeqTake nil) xs n
--                    else nil)
-- "unsafeTakeList"  [1] forall n xs . foldr (takeFB (:) []) (flipSeqTake []) xs n
--                                         = unsafeTake n xs
--  #-}
-- 
-- {-# INLINE [0] flipSeqTake #-}
-- -- Just flip seq, specialized to Int, but not inlined too early.
-- -- It's important to force the numeric argument here, even though
-- -- it's not used. Otherwise, take n [] doesn't force n. This is
-- -- bad for strictness analysis and unboxing, and leads to increased
-- -- allocation in T7257.
-- flipSeqTake :: a -> Int -> a
-- flipSeqTake x !_n = x
-- 
-- {-# INLINE [0] takeFB #-}
-- takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
-- -- The \m accounts for the fact that takeFB is used in a higher-order
-- -- way by takeFoldr, so it's better to inline.  A good example is
-- --     take n (repeat x)
-- -- for which we get excellent code... but only if we inline takeFB
-- -- when given four arguments
-- takeFB c n x xs
--   = \ m -> case m of
--             1 -> x `c` n
--             _ -> x `c` xs (m - 1)
-- #endif
-- 
-- -- | 'drop' @n xs@ returns the suffix of @xs@
-- -- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
-- --
-- -- > drop 6 "Hello World!" == "World!"
-- -- > drop 3 [1,2,3,4,5] == [4,5]
-- -- > drop 3 [1,2] == []
-- -- > drop 3 [] == []
-- -- > drop (-1) [1,2] == [1,2]
-- -- > drop 0 [1,2] == [1,2]
-- --
-- -- It is an instance of the more general 'Data.List.genericDrop',
-- -- in which @n@ may be of any integral type.
drop                   :: Int -> [a] -> [a]
-- #ifdef USE_REPORT_PRELUDE
drop k ys =
    let
        drop' n xs     | n <= (fromInteger zeroInteger) =  xs
        drop' _ []              =  []
        drop' n (_:xs)          =  drop' (n-(fromInteger oneInteger)) xs

        I# k' = k
    in
    case typeIndex# ys `adjStr` ys of
        1# -> let !len = strLen# ys in strSubstr# ys k' len
        _ -> drop' k ys
-- #else /* hack away */
-- {-# INLINE drop #-}
-- drop n ls
--   | n <= 0     = ls
--   | otherwise  = unsafeDrop n ls
--   where
--     -- A version of drop that drops the whole list if given an argument
--     -- less than 1
--     unsafeDrop :: Int -> [a] -> [a]
--     unsafeDrop !_ []     = []
--     unsafeDrop 1  (_:xs) = xs
--     unsafeDrop m  (_:xs) = unsafeDrop (m - 1) xs
-- #endif
-- 
-- -- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- -- length @n@ and second element is the remainder of the list:
-- --
-- -- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- -- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- -- > splitAt 1 [1,2,3] == ([1],[2,3])
-- -- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- -- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- -- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- -- > splitAt (-1) [1,2,3] == ([],[1,2,3])
-- --
-- -- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- -- (@splitAt _|_ xs = _|_@).
-- -- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- -- in which @n@ may be of any integral type.
splitAt                :: Int -> [a] -> ([a],[a])
-- 
-- #ifdef USE_REPORT_PRELUDE
splitAt n xs           =  (take n xs, drop n xs)
-- #else
-- splitAt n ls
--   | n <= 0 = ([], ls)
--   | otherwise          = splitAt' n ls
--     where
--         splitAt' :: Int -> [a] -> ([a], [a])
--         splitAt' _  []     = ([], [])
--         splitAt' 1  (x:xs) = ([x], xs)
--         splitAt' m  (x:xs) = (x:xs', xs'')
--           where
--             (xs', xs'') = splitAt' (m - 1) xs
-- #endif /* USE_REPORT_PRELUDE */
-- 
-- -- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- -- first element is longest prefix (possibly empty) of @xs@ of elements that
-- -- satisfy @p@ and second element is the remainder of the list:
-- --
-- -- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- -- > span (< 9) [1,2,3] == ([1,2,3],[])
-- -- > span (< 0) [1,2,3] == ([],[1,2,3])
-- --
-- -- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
-- 
span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)
-- 
-- -- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- -- first element is longest prefix (possibly empty) of @xs@ of elements that
-- -- /do not satisfy/ @p@ and second element is the remainder of the list:
-- --
-- -- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- -- > break (< 9) [1,2,3] == ([],[1,2,3])
-- -- > break (> 9) [1,2,3] == ([1,2,3],[])
-- --
-- -- 'break' @p@ is equivalent to @'span' ('not' . p)@.
-- 
break                   :: (a -> Bool) -> [a] -> ([a],[a])
-- #ifdef USE_REPORT_PRELUDE
break p                 =  span (not . p)
-- #else
-- -- HBC version (stolen)
-- break _ xs@[]           =  (xs, xs)
-- break p xs@(x:xs')
--            | p x        =  ([],xs)
--            | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)
-- #endif
-- 
-- -- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- -- @xs@ must be finite.
reverse                 :: forall a . [a] -> [a]
-- #ifdef USE_REPORT_PRELUDE
reverse               xs  =
  let
    strRevQuant =
      let !ys = symgen @[a]
          !sl_xs = strLen# xs
          !sl_ys = strLen# ys
          rev_prop1 = sl_xs $==# sl_ys
          rev_prop2 i =
            0# $<=# i &&# i $<# strLen# xs ==> strAt# xs i `strEq#` strAt# ys ((strLen# xs -# 1#) -# i)
      in
      assume rev_prop1 (assume (forAllInt# rev_prop2) ys)

    strRev _ _ [] = []
    strRev sl_xs i (y:ys) = 
      let
        !pos = sl_xs -# i
        !xs_at_pos = strAt# xs pos

        rev_prop = [y] `strEq#` xs_at_pos
        
        !i_plus_one = i +# 1#
      in
      assume rev_prop (y:strRev sl_xs i_plus_one ys)
  in
  case typeIndex# xs `adjStr` xs of
      1# -> case strQuantifiers 1# of
                1# -> strRevQuant
                _ ->
                  let
                      !ys = symgen @[a]
                      !sl_xs = strLen# xs
                      !sl_ys = strLen# ys
                      rev_prop = sl_xs $==# sl_ys
                  in
                  assume rev_prop (strRev sl_xs 1# ys)
      _ -> foldl (flip (:)) [] xs
-- #else
-- reverse l =  rev l []
--   where
--     rev []     a = a
--     rev (x:xs) a = rev xs (x:a)
-- #endif
-- 
-- -- | 'and' returns the conjunction of a Boolean list.  For the result to be
-- -- 'True', the list must be finite; 'False', however, results from a 'False'
-- -- value at a finite index of a finite or infinite list.
and                     :: [Bool] -> Bool
-- #ifdef USE_REPORT_PRELUDE
and                     =  foldr (&&) True
-- #else
-- and []          =  True
-- and (x:xs)      =  x && and xs
-- {-# NOINLINE [1] and #-}
-- 
-- {-# RULES
-- "and/build"     forall (g::forall b.(Bool->b->b)->b->b) .
--                 and (build g) = g (&&) True
--  #-}
-- #endif
-- 
-- -- | 'or' returns the disjunction of a Boolean list.  For the result to be
-- -- 'False', the list must be finite; 'True', however, results from a 'True'
-- -- value at a finite index of a finite or infinite list.
or                      :: [Bool] -> Bool
-- #ifdef USE_REPORT_PRELUDE
or                      =  foldr (||) False
-- #else
-- or []           =  False
-- or (x:xs)       =  x || or xs
-- {-# NOINLINE [1] or #-}
-- 
-- {-# RULES
-- "or/build"      forall (g::forall b.(Bool->b->b)->b->b) .
--                 or (build g) = g (||) False
--  #-}
-- #endif
-- 
-- -- | Applied to a predicate and a list, 'any' determines if any element
-- -- of the list satisfies the predicate.  For the result to be
-- -- 'False', the list must be finite; 'True', however, results from a 'True'
-- -- value for the predicate applied to an element at a finite index of a finite or infinite list.
any                     :: (a -> Bool) -> [a] -> Bool
-- 
-- #ifdef USE_REPORT_PRELUDE
any p                   =  or . map p
-- #else
-- any _ []        = False
-- any p (x:xs)    = p x || any p xs
-- 
-- {-# NOINLINE [1] any #-}
-- 
-- {-# RULES
-- "any/build"     forall p (g::forall b.(a->b->b)->b->b) .
--                 any p (build g) = g ((||) . p) False
--  #-}
-- #endif
-- 
-- -- | Applied to a predicate and a list, 'all' determines if all elements
-- -- of the list satisfy the predicate. For the result to be
-- -- 'True', the list must be finite; 'False', however, results from a 'False'
-- -- value for the predicate applied to an element at a finite index of a finite or infinite list.
all                     :: (a -> Bool) -> [a] -> Bool
-- #ifdef USE_REPORT_PRELUDE
all p                   =  and . map p
-- #else
-- all _ []        =  True
-- all p (x:xs)    =  p x && all p xs
-- 
-- {-# NOINLINE [1] all #-}
-- 
-- {-# RULES
-- "all/build"     forall p (g::forall b.(a->b->b)->b->b) .
--                 all p (build g) = g ((&&) . p) True
--  #-}
-- #endif
-- 
-- -- | 'elem' is the list membership predicate, usually written in infix form,
-- -- e.g., @x \`elem\` xs@.  For the result to be
-- -- 'False', the list must be finite; 'True', however, results from an element
-- -- equal to @x@ found at a finite index of a finite or infinite list.
elem                    :: (Eq a) => a -> [a] -> Bool
-- #ifdef USE_REPORT_PRELUDE
-- elem x                  =  any (== x)
elem x xs = let
                elem' x = any (== x)
                strElem x xs = let !x' = x
                                   !x_as_list = [x']
                                   !pos = strIndexOf# xs x_as_list 0# 
                               in pos $/=# (-1#)
            in case typeIndex# xs `adjStr` xs of
                1# -> strElem x xs
                _ -> elem' x xs  
-- #else
-- elem _ []       = False
-- elem x (y:ys)   = x==y || elem x ys
-- {-# NOINLINE [1] elem #-}
-- {-# RULES
-- "elem/build"    forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b)
--    . elem x (build g) = g (\ y r -> (x == y) || r) False
--  #-}
-- #endif
-- 
-- -- | 'notElem' is the negation of 'elem'.
notElem                 :: (Eq a) => a -> [a] -> Bool
-- #ifdef USE_REPORT_PRELUDE
-- notElem x               =  all (/= x)
notElem x xs = let
                notElem' x = all (/= x)
                strNotElem x xs = let !x' = x
                                      !x_as_list = [x']
                                      !pos = strIndexOf# xs x_as_list 0# 
                                  in pos $==# (-1#)
            in case typeIndex# xs `adjStr` xs of
                1# -> strNotElem x xs
                _ -> notElem' x xs
-- #else
-- notElem _ []    =  True
-- notElem x (y:ys)=  x /= y && notElem x ys
-- {-# NOINLINE [1] notElem #-}
-- {-# RULES
-- "notElem/build" forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b)
--    . notElem x (build g) = g (\ y r -> (x /= y) && r) True
--  #-}
-- #endif
-- 
-- -- | 'lookup' @key assocs@ looks up a key in an association list.
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys
-- 
-- -- | Map a function over a list and concatenate the results.
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []
-- 
{-# NOINLINE [1] concatMap #-}
-- 
{-# RULES
"concatMap" forall f xs . concatMap f xs =
    build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
 #-}
-- 
-- 
-- -- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = foldr (++) []
-- 
{-# NOINLINE [1] concat #-}
-- 
{-# RULES
  "concat" forall xs. concat xs =
     build (\c n -> foldr (\x y -> foldr c y x) n xs)
-- We don't bother to turn non-fusible applications of concat back into concat
 #-}
-- 
-- -- | List index (subscript) operator, starting from 0.
-- -- It is an instance of the more general 'Data.List.genericIndex',
-- -- which takes an index of any integral type.
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
(!!)                    :: HasCallStack => [a] -> Int -> a
#else
(!!)                    :: [a] -> Int -> a
#endif
-- #ifdef USE_REPORT_PRELUDE
-- xs     !! n | n < 0 =  errorWithoutStackTrace "Prelude.!!: negative index"
-- xs     !! n | n < (fromInteger zeroInteger) =  errorWithoutStackTrace "Prelude.!!: negative index"
-- []     !! _         =  errorWithoutStackTrace "Prelude.!!: index too large"
-- []     !! _         =  errorWithoutStackTrace "Prelude.!!: index too large"
-- (x:_)  !! 0         =  x
-- (_:xs) !! n         =  xs !! (n-1)
-- (x:xs) !! n = if n == fromInteger zeroInteger then x else xs !! (n - fromInteger oneInteger)

-- For strings, smt-lib has a function str.at that takes a string and an index
-- and returns a string with the char at that index or an empty string if the index is invalid
xs !! n =
    let reg_idx xs n | n < (fromInteger zeroInteger) = errorWithoutStackTrace "Prelude.!!: negative index"
        reg_idx [] _     = errorWithoutStackTrace "Prelude.!!: index too large"
        reg_idx (x:xs) n = if n == fromInteger zeroInteger then x else xs !! (n - fromInteger oneInteger)

        str_idx xs n
            | (h:_) <- i = h
            | otherwise  = errorWithoutStackTrace "Prelude.!!: error with smtlib str.at"
            where I# n' = n
                  i = strAt# xs n'

    in case typeIndex# xs `adjStr` xs of
        1# -> str_idx xs n
        _ -> reg_idx xs n
-- #else
-- 
-- -- We don't really want the errors to inline with (!!).
-- -- We may want to fuss around a bit with NOINLINE, and
-- -- if so we should be careful not to trip up known-bottom
-- -- optimizations.
-- tooLarge :: Int -> a
-- tooLarge _ = errorWithoutStackTrace (prel_list_str ++ "!!: index too large")
-- 
-- negIndex :: a
-- negIndex = errorWithoutStackTrace $ prel_list_str ++ "!!: negative index"
-- 
-- {-# INLINABLE (!!) #-}
-- xs !! n
--   | n < 0     = negIndex
--   | otherwise = foldr (\x r k -> case k of
--                                    0 -> x
--                                    _ -> r (k-1)) tooLarge xs n
-- #endif
--
-- | List index (subscript) operator, starting from 0. Returns 'Nothing'
-- if the index is out of bounds
--
-- This is the total variant of the partial '!!' operator.
--
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n = let
              totalIndex xs n
                  | n < 0     = Nothing
                  | otherwise = foldr (\x r k -> case k of
                                                   0 -> Just x
                                                   _ -> r (k-1)) (const Nothing) xs n
              strTotalIndex xs n
                  | (h:_) <- i = Just h
                  | otherwise = Nothing
                  where I# n' = n
                        i = strAt# xs n'
          in case typeIndex# xs `adjStr` xs of
              1# -> strTotalIndex xs n
              _ -> totalIndex xs n
--
-- --------------------------------------------------------------
-- -- The zip family
-- --------------------------------------------------------------
-- 
foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 k z = go
  where
        go []    _ys     = z
        go _xs   []      = z
        go (x:xs) (y:ys) = k x y (go xs ys)
{-# INLINE [0] foldr2 #-}
-- 
foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)
-- 
-- -- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
{-# RULES
"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) .
                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
 #-}
-- -- There used to be a foldr2/right rule, allowing foldr2 to fuse with a build
-- -- form on the right. However, this causes trouble if the right list ends in
-- -- a bottom that is only avoided by the left list ending at that spot. That is,
-- -- foldr2 f z [a,b,c] (d:e:f:_|_), where the right list is produced by a build
-- -- form, would cause the foldr2/right rule to introduce bottom. Example:
-- --
-- -- zip [1,2,3,4] (unfoldr (\s -> if s > 4 then undefined else Just (s,s+1)) 1)
-- --
-- -- should produce
-- --
-- -- [(1,1),(2,2),(3,3),(4,4)]
-- --
-- -- but with the foldr2/right rule it would instead produce
-- --
-- -- (1,1):(2,2):(3,3):(4,4):_|_
-- 
-- -- Zips for larger tuples are in the List module.
-- 
-- ----------------------------------------------
-- -- | 'zip' takes two lists and returns a list of corresponding pairs.
-- -- If one input list is short, excess elements of the longer list are
-- -- discarded.
-- --
-- -- 'zip' is right-lazy:
-- --
-- -- > zip [] _|_ = []
{-# NOINLINE [1] zip #-}
zip :: [a] -> [b] -> [(a,b)]
zip []     _bs    = []
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs
-- 
{-# INLINE [0] zipFB #-}
zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
zipFB c = \x y r -> (x,y) `c` r
-- 
{-# RULES
"zip"      [~1] forall xs ys. zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
"zipList"  [1]  foldr2 (zipFB (:)) []   = zip
 #-}
-- 
-- ----------------------------------------------
-- -- | 'zip3' takes three lists and returns a list of triples, analogous to
-- -- 'zip'.
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- -- Specification
-- -- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []
-- 
-- 
-- -- The zipWith family generalises the zip family by zipping with the
-- -- function given as the first argument, instead of a tupling function.
-- 
-- ----------------------------------------------
-- -- | 'zipWith' generalises 'zip' by zipping with the function given
-- -- as the first argument, instead of a tupling function.
-- -- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- -- list of corresponding sums.
-- --
-- -- 'zipWith' is right-lazy:
-- --
-- -- > zipWith f [] _|_ = []
{-# NOINLINE [1] zipWith #-}
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith _f []     _bs    = []
zipWith _f _as    []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs
-- 
-- -- zipWithFB must have arity 2 since it gets two arguments in the "zipWith"
-- -- rule; it might not get inlined otherwise
{-# INLINE [0] zipWithFB #-}
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r
-- 
{-# RULES
"zipWith"       [~1] forall f xs ys.    zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
"zipWithList"   [1]  forall f.  foldr2 (zipWithFB (:) f) [] = zipWith f
  #-}
-- 
-- -- | The 'zipWith3' function takes a function which combines three
-- -- elements, as well as three lists and returns a list of their point-wise
-- -- combination, analogous to 'zipWith'.
zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []
-- 
-- -- | 'unzip' transforms a list of pairs into a list of first components
-- -- and a list of second components.
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
-- 
-- -- | The 'unzip3' function takes a list of triples and returns three
-- -- lists, analogous to 'unzip'.
unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])
-- 
-- --------------------------------------------------------------
-- -- Error code
-- --------------------------------------------------------------
-- 
-- -- Common up near identical calls to `error' to reduce the number
-- -- constant strings created when compiled:
-- 
errorEmptyList :: String -> a
errorEmptyList fun =
  -- errorWithoutStackTrace (prel_list_str ++ fun ++ ": empty list")
  errorWithoutStackTrace (prel_list_str ++ fun ++ ": empty list")
-- 
prel_list_str :: String
-- prel_list_str = "Prelude."
prel_list_str = "Prelude."
