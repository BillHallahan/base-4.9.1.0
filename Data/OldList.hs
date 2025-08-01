{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, TypeApplications, MagicHash, BangPatterns #-}
-- 
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  Data.List
-- -- Copyright   :  (c) The University of Glasgow 2001
-- -- License     :  BSD-style (see the file libraries/base/LICENSE)
-- --
-- -- Maintainer  :  libraries@haskell.org
-- -- Stability   :  stable
-- -- Portability :  portable
-- --
-- -- Operations on lists.
-- --
-- -----------------------------------------------------------------------------
-- 
module Data.OldList
   (
--    -- * Basic functions
-- 
     (++)
   , head
   , last
   , tail
   , init
   , uncons
   , null
   , length
-- 
--    -- * List transformations
   , map
   , reverse
-- 
   , intersperse
   , intercalate
   , transpose

   , subsequences
   , permutations

   -- * Reducing lists (folds)

   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1
-- 
--    -- ** Special folds
-- 
   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum
-- 
--    -- * Building lists
-- 
--    -- ** Scans
   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1
-- 
--    -- ** Accumulating maps
   , mapAccumL
   , mapAccumR
-- 
--    -- ** Infinite lists
   , iterate
   , repeat
   , replicate
   , cycle
-- 
--    -- ** Unfolding
   , unfoldr
-- 
--    -- * Sublists
-- 
--    -- ** Extracting sublists
   , take
   , drop
   , splitAt
-- 
   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break

   , stripPrefix

   , group

   , inits
   , tails

   -- ** Predicates
   , isPrefixOf
   , isSuffixOf
   , isInfixOf

   -- * Searching lists

   -- ** Searching by equality
   , elem
   , notElem
   , lookup
-- 
--    -- ** Searching with a predicate
   , find
   , filter
   , partition
-- 
--    -- * Indexing lists
--    -- | These functions treat a list @xs@ as a indexed collection,
--    -- with indices ranging from 0 to @'length' xs - 1@.
-- 
   , (!!)

   , elemIndex
   , elemIndices

   , findIndex
   , findIndices

--    -- * Zipping and unzipping lists
-- 
   , zip
   , zip3, zip4, zip5, zip6, zip7
-- 
   , zipWith
   , zipWith3, zipWith4, zipWith5, zipWith6, zipWith7
-- 
   , unzip
   , unzip3, unzip4, unzip5, unzip6, unzip7
-- 
--    -- * Special lists
-- 
--    -- ** Functions on strings
   , lines
   , words
   , unlines
   , unwords
-- 
--    -- ** \"Set\" operations
-- 
   , nub
-- 
   , delete
   , (\\)
-- 
   , union
   , intersect
-- 
--    -- ** Ordered lists
   , sort
   , sortOn
   , insert

   -- * Generalized functions

   -- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.
   --
   -- It is often convenient to use these functions together with
   -- 'Data.Function.on', for instance @'sortBy' ('compare'
   -- \`on\` 'fst')@.

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate

   ) where
-- 
import Data.Maybe
-- import Data.Bits        ( (.&.) )
import Data.Char        ( isSpace )
import Data.Ord         ( comparing )
import Data.Tuple       ( fst, snd )

import GHC.Num
import GHC.Real
import GHC.List
import GHC.Base
-- 
infix 5 \\ -- comment to fool cpp: https://www.haskell.org/ghc/docs/latest/html/users_guide/options-phases.html#cpp-string-gaps
-- 
-- -- -----------------------------------------------------------------------------
-- -- List functions
-- 
-- -- | The 'dropWhileEnd' function drops the largest suffix of a list
-- -- in which the given predicate holds for all elements.  For example:
-- --
-- -- > dropWhileEnd isSpace "foo\n" == "foo"
-- -- > dropWhileEnd isSpace "foo bar" == "foo bar"
-- -- > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
-- --
-- -- @since 4.5.0.0
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | The 'stripPrefix' function drops the given prefix from a list.
-- It returns 'Nothing' if the list did not start with the prefix
-- given, or 'Just' the list after the prefix, if it does.
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix "foo" "foo" == Just ""
-- > stripPrefix "foo" "barfoo" == Nothing
-- > stripPrefix "foo" "barfoobaz" == Nothing
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix pre zs =
   let
      stripPrefix' [] ys = Just ys
      stripPrefix' (x:xs) (y:ys)
         | x == y = stripPrefix' xs ys
      stripPrefix' _ _ = Nothing
   in
   case typeIndex# pre `adjStr` pre `adjStr` zs of
      1# -> let !is_pre = strPrefixOf# pre zs in
            if is_pre then Just (strReplace# zs pre []) else Nothing
      _ -> stripPrefix' pre zs

-- | The 'elemIndex' function returns the index of the first element
-- in the given list which is equal (by '==') to the query element,
-- or 'Nothing' if there is no such element.
-- elemIndex x = findIndex (x==) 
elemIndex x xs  = let elemIndex' x xs = findIndex (x==) xs
                      strElemIndex x xs | pos $/=# (-1#) = Just (I# pos)
                                        | otherwise = Nothing
                                   where !x' = x
                                         !x_as_list = [x']
                                         !pos = strIndexOf# xs x_as_list 0#
                  in case typeIndex# xs `adjStr` xs of
                        1# -> strElemIndex x xs
                        _ -> elemIndex' x xs

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
elemIndices     :: forall a . Eq a => a -> [a] -> [Int]
elemIndices x xs  =
   let
      strElemIndices n@(I# n') ys =
         let
            !end = symgen @Bool
         in
         case end of
            True ->
                  let
                     !x_list = [x]

                     !ys_index_x = strIndexOf# ys x_list 0#
                     !ys_no_x = ys_index_x $==# -1#
                  in
                  assume (ys_no_x) []
            False ->
                  let
                     !x_list = [x]

                     !as = symgen @[a]
                     !bs = symgen @[a]

                     !sl_as = strLen# as
                  
                     !as_index_x = strIndexOf# as x_list 0# 
                     !as_no_x = as_index_x $==# -1#

                     !as_x_app = as `strAppend#` x_list
                     !full_list = as_x_app `strAppend#` bs
                     !ys_eq_fl = ys `strEq#` full_list

                     !pos = n' +# sl_as
                  in
                  assume as_no_x . assume ys_eq_fl $ I# pos:strElemIndices (I# pos + 1) bs
   in
   case typeIndex# xs `adjStr` xs of
         1# -> strElemIndices 0 xs
         _ -> findIndices (x==) xs

-- | The 'find' function takes a predicate and a list and returns the
-- first element in the list matching the predicate, or 'Nothing' if
-- there is no such element.
find            :: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

-- | The 'findIndex' function takes a predicate and a list and returns
-- the index of the first element in the list satisfying the predicate,
-- or 'Nothing' if there is no such element.
findIndex       :: (a -> Bool) -> [a] -> Maybe Int
findIndex p     = listToMaybe . findIndices p

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices      :: (a -> Bool) -> [a] -> [Int]
-- #ifdef USE_REPORT_PRELUDE
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
-- #else
-- Efficient definition, adapted from Data.Sequence
-- {-# INLINE findIndices #-}
-- findIndices p ls = build $ \c n ->
--   let go x r k | p x       = I# k `c` r (k +# 1#)
--                | otherwise = r (k +# 1#)
--   in foldr go (\_ -> n) ls 0#
-- #endif  /* USE_REPORT_PRELUDE */

-- | The 'isPrefixOf' function takes two lists and returns 'True'
-- iff the first list is a prefix of the second.
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf as bs =
   let
      isPrefixOf' [] _         =  True
      isPrefixOf' _  []        =  False
      isPrefixOf' (x:xs) (y:ys)=  x == y && isPrefixOf' xs ys
   in
   case typeIndex# as `adjStr` as `adjStr` bs of
      1# -> strPrefixOf# as bs
      _ -> isPrefixOf' as bs

-- | The 'isSuffixOf' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
xs `isSuffixOf` ys      = 
   let
      isSuffixOf' ns hs = maybe False id $ do
         delta <- dropLengthMaybe ns hs
         return $ ns == dropLength delta hs
      -- Since dropLengthMaybe ns hs succeeded, we know that (if hs is finite)
      -- length ns + length delta = length hs
      -- so dropping the length of delta from hs will yield a suffix exactly
      -- the length of ns.
   in
   case typeIndex# xs `adjStr` xs `adjStr` ys of
      1# -> strSuffixOf# xs ys
      _ -> isSuffixOf' xs ys

-- A version of drop that drops the length of the first argument from the
-- second argument. If xs is longer than ys, xs will not be traversed in its
-- entirety.  dropLength is also generally faster than (drop . length)
-- Both this and dropLengthMaybe could be written as folds over their first
-- arguments, but this reduces clarity with no benefit to isSuffixOf.
dropLength :: [a] -> [b] -> [b]
dropLength [] y = y
dropLength _ [] = []
dropLength (_:x') (_:y') = dropLength x' y'

-- A version of dropLength that returns Nothing if the second list runs out of
-- elements before the first.
dropLengthMaybe :: [a] -> [b] -> Maybe [b]
dropLengthMaybe [] y = Just y
dropLengthMaybe _ [] = Nothing
dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- iff the first list is contained, wholly and intact,
-- anywhere within the second.
--
-- Example:
--
-- >isInfixOf "Haskell" "I really like Haskell." == True
-- >isInfixOf "Ial" "I really like Haskell." == False
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
-- isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
isInfixOf needle haystack = let isInfixOf' n h = any (isPrefixOf n) (tails h)
                                strInfixOf n h = let !pos = strIndexOf# h n 0#
                                                 in pos $/=# (-1#)
                            in case typeIndex# haystack `adjStr` haystack `adjStr` needle of
                                1# -> strInfixOf needle haystack
                                _ -> isInfixOf' needle haystack

-- | /O(n^2)/. The 'nub' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \`essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to supply
-- their own equality test.
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)
-- 
-- -- | The 'nubBy' function behaves just like 'nub', except it uses a
-- -- user-supplied equality predicate instead of the overloaded '=='
-- -- function.
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
-- #ifdef USE_REPORT_PRELUDE
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
-- #else
-- -- stolen from HBC
-- nubBy eq l              = nubBy' l []
--   where
--     nubBy' [] _         = []
--     nubBy' (y:ys) xs
--        | elem_by eq y xs = nubBy' ys xs
--        | otherwise       = y : nubBy' ys (y:xs)
-- 
-- -- Not exported:
-- -- Note that we keep the call to `eq` with arguments in the
-- -- same order as in the reference (prelude) implementation,
-- -- and that this order is different from how `elem` calls (==).
-- -- See #2528, #3280 and #7913.
-- -- 'xs' is the list of things we've seen so far,
-- -- 'y' is the potential new element
-- elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
-- elem_by _  _ []         =  False
-- elem_by eq y (x:xs)     =  x `eq` y || elem_by eq y xs
-- #endif
-- 
-- 
-- -- | 'delete' @x@ removes the first occurrence of @x@ from its list argument.
-- -- For example,
-- --
-- -- > delete 'a' "banana" == "bnana"
-- --
-- -- It is a special case of 'deleteBy', which allows the programmer to
-- -- supply their own equality test.
-- 
delete                  :: (Eq a) => a -> [a] -> [a]
delete x xs                  = 
   case typeIndex# xs `adjStr` xs of
      1# -> let !x' = x; !x_list = [x'] in strReplace# xs x_list []
      _ -> deleteBy (==) x xs
-- 
-- -- | The 'deleteBy' function behaves like 'delete', but takes a
-- -- user-supplied equality predicate.
deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys
-- 
-- | The '\\' function is list difference (non-associative).
-- In the result of @xs@ '\\' @ys@, the first occurrence of each element of
-- @ys@ in turn (if any) has been removed from @xs@.  Thus
--
-- > (xs ++ ys) \\ xs == ys.
--
-- It is a special case of 'deleteFirstsBy', which allows the programmer
-- to supply their own equality test.

(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)

-- | The 'union' function returns the list union of the two lists.
-- For example,
--
-- > "dog" `union` "cow" == "dogcw"
--
-- Duplicates, and elements of the first list, are removed from the
-- the second list, but if the first list contains duplicates, so will
-- the result.
-- It is a special case of 'unionBy', which allows the programmer to supply
-- their own equality test.

union                   :: (Eq a) => [a] -> [a] -> [a]
union                   = unionBy (==)

-- | The 'unionBy' function is the non-overloaded version of 'union'.
unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | The 'intersect' function takes the list intersection of two lists.
-- For example,
--
-- > [1,2,3,4] `intersect` [2,4,6,8] == [2,4]
--
-- If the first list contains duplicates, so will the result.
--
-- > [1,2,2,3,4] `intersect` [6,4,4,2] == [2,2,4]
--
-- It is a special case of 'intersectBy', which allows the programmer to
-- supply their own equality test. If the element is found in both the first
-- and the second list, the element from the first list will be used.

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect               =  intersectBy (==)

-- | The 'intersectBy' function is the non-overloaded version of 'intersect'.
intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]

-- | The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
--
-- > intersperse ',' "abcde" == "a,b,c,d,e"

intersperse             :: forall a . a -> [a] -> [a]
intersperse s xs =
   let
         strIntersperse =
            let
               !ys = symgen @[a]
               !s' = s
               !s_str = [s']

               !sl_xs = strLen# xs
               !sl_ys = strLen# ys

               !len_two_xs = 2# *# sl_xs
               !len_two_xs_m = len_two_xs -# 1#
               !len_prop = ite (sl_xs $==# 0#) (sl_ys $==# 0#) (len_two_xs_m $==# sl_ys)

               copy_prop i =
                  0# $<=# i &&# i $<# strLen# xs ==> strAt# xs i `strEq#` strAt# ys (2# *# i)

               inter_prop i = 
                  0# $<=# i &&# i $<# (strLen# xs -# 1#) ==> s_str `strEq#` strAt# ys ((2# *# i) +# 1#)
            in
            assume len_prop . assume (forAllInt# copy_prop) . assume (forAllInt# inter_prop) $ ys

         intersperse' _   []      = []
         intersperse' sep (x:xs')  = x : prependToAll sep xs'
   in
   case strQuantifiers (typeIndex# xs `adjStr` xs) of
      1# -> strIntersperse
      _ -> intersperse' s xs
-- 
-- 
-- Not exported:
-- We want to make every element in the 'intersperse'd list available
-- as soon as possible to avoid space leaks. Experiments suggested that
-- a separate top-level helper is more efficient than a local worker.
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs
-- 
-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
-- result.
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- | The 'transpose' function transposes the rows and columns of its argument.
-- For example,
--
-- > transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
--
-- If some of the rows are shorter than the following rows, their elements are skipped:
--
-- > transpose [[10,11],[20],[],[30,31,32]] == [[10,20,30],[11,31],[32]]

transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])


-- | The 'partition' function takes a predicate a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)

partition               :: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs
-- 
select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)
-- 
-- -- | The 'mapAccumL' function behaves like a combination of 'map' and
-- -- 'foldl'; it applies a function to each element of a list, passing
-- -- an accumulating parameter from left to right, and returning a final
-- -- value of this accumulator together with the new list.
mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
--                                     -- and accumulator, returning new
--                                     -- accumulator and elt of result list
          -> acc            -- Initial accumulator
          -> [x]            -- Input list
          -> (acc, [y])     -- Final accumulator and result list
{-# NOINLINE [1] mapAccumL #-}
mapAccumL _ s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs
-- 
{-# RULES
"mapAccumL" [~1] forall f s xs . mapAccumL f s xs = foldr (mapAccumLF f) pairWithNil xs s
"mapAccumLList" [1] forall f s xs . foldr (mapAccumLF f) pairWithNil xs s = mapAccumL f s xs
 #-}
-- 
pairWithNil :: acc -> (acc, [y])
{-# INLINE [0] pairWithNil #-}
pairWithNil x = (x, [])
-- 
mapAccumLF :: (acc -> x -> (acc, y)) -> x -> (acc -> (acc, [y])) -> acc -> (acc, [y])
{-# INLINE [0] mapAccumLF #-}
mapAccumLF f = \x r -> oneShot (\s ->
                         let (s', y)   = f s x
                             (s'', ys) = r s'
                         in (s'', y:ys))
--   -- See Note [Left folds via right fold]
-- 
-- 
-- -- | The 'mapAccumR' function behaves like a combination of 'map' and
-- -- 'foldr'; it applies a function to each element of a list, passing
-- -- an accumulating parameter from right to left, and returning a final
-- -- value of this accumulator together with the new list.
mapAccumR :: (acc -> x -> (acc, y))     -- Function of elt of input list
--                                         -- and accumulator, returning new
--                                         -- accumulator and elt of result list
            -> acc              -- Initial accumulator
            -> [x]              -- Input list
            -> (acc, [y])               -- Final accumulator and result list
mapAccumR _ s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s'', y:ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs
-- 
-- -- | The 'insert' function takes an element and a list and inserts the
-- -- element into the list at the first position where it is less
-- -- than or equal to the next element.  In particular, if the list
-- -- is sorted before the call, the result will also be sorted.
-- -- It is a special case of 'insertBy', which allows the programmer to
-- -- supply their own comparison function.
insert :: forall a . Ord a => a -> [a] -> [a]
insert e ls =
   let
      strInsertQuant =
         let
            !e' = e
            !e_ls = [e']

            -- Inserting makes the list one longer
            !sl_ls = strLen# ls
            !sl_ys = strLen# ys
            !sl_ls_plus_one = sl_ls +# 1#
            ins_prop_len = sl_ls_plus_one $==# sl_ys

            -- ins_pos is the position to insert at
            I# ins_pos = symgen @Int

            -- All elements less than ins_pos must be < e
            ins_prop_bound1 i =
               0# $<=# i &&# i $<# ins_pos ==> strAt# ls i `strLt#` e_ls

            -- e must be the last list element or <= to the elements at ins_pos + 1
            !str_at1 = strAt# ls ins_pos
            !e_le_next = e_ls `strLe#` str_at1
            !not_e_last = ins_pos $<# sl_ls
            !ins_prop_bound2 = not_e_last ==> e_le_next

            -- All elements less than ins_pos get copied directly from ls to xs
            -- All elements greater than or equal to ins_pos get copied from their position in ls to that position + 1 in xs
            !ls_pre = strSubstr# ls 0# ins_pos
            !ls_pos = strSubstr# ls ins_pos sl_ls
            
            !inter1 = ls_pre `strAppend#` e_ls
            !ys = inter1 `strAppend#` ls_pos
         in
           assume ins_prop_len
         . assume (forAllInt# ins_prop_bound1)
         . assume ins_prop_bound2
         $ ys
      
      -- Copies values from ls to ys.
      -- pos_ys is updated on each step
      -- pos_ls is updated on each step EXCEPT the step where we insert e 
      strInsert e_ls pos_ls pos_ys [] = []
      strInsert e_ls pos_ls pos_ys [y] =
         let
            !ls_at_pos = strAt# ls pos_ls
         in
         ite (pos_ls $==# pos_ys) e_ls ls_at_pos
      strInsert e_ls pos_ls pos_ys (y:ys) =
         let
            !ls_at_pos = strAt# ls pos_ls

            !pos_ls_plus_one = pos_ls +# 1#

            !comp_char = e_ls `strLe#` ls_at_pos
            !cond_pos = pos_ls $==# pos_ys

            -- Is e is less the next character in ls, and pos_ys == pos_ls (indicating that a
            -- character has not been inserted.)
            !cond_comp = comp_char &&# cond_pos

            !y_eq_e = [y] `strEq#` e_ls
            !y_eq_pos = [y] `strEq#` ls_at_pos

            !set_y = ite cond_comp y_eq_e y_eq_pos
            !pos_ls_next = iteInt# cond_comp pos_ls pos_ls_plus_one
            I# (pos_ls_next_var) = symgen @Int

            !pos_ys_plus_one = pos_ys +# 1#
         in
           assume set_y
         . assume (pos_ls_next_var $==# pos_ls_next)
         $ y:strInsert e_ls pos_ls_next_var pos_ys_plus_one ys
   in
   case typeIndex# ls `adjStr` ls of
      1# -> case strQuantifiers 1# of
               1# -> strInsertQuant
               _ ->
                  let
                     !e' = e
                     !e_ls = [e']

                     !ys = symgen @[a]
                     !sl_ls = strLen# ls
                     !sl_ys = strLen# ys
                     !sl_ls_plus_one = sl_ls +# 1#
                     ins_prop = sl_ls_plus_one $==# sl_ys
                  in
                  assume ins_prop $ strInsert e_ls 0# 0# ys
      _ -> insertBy (compare) e ls

-- 
-- -- | The non-overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys

-- | The 'maximumBy' function takes a comparison function and a list
-- and returns the greatest element of the list by the comparison function.
-- The list must be finite and non-empty.
maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []          =  errorWithoutStackTrace "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 maxBy xs
                        where
                           maxBy x y = case cmp x y of
                                       GT -> x
                                       _  -> y

-- | The 'minimumBy' function takes a comparison function and a list
-- and returns the least element of the list by the comparison function.
-- The list must be finite and non-empty.
minimumBy               :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []          =  errorWithoutStackTrace "List.minimumBy: empty list"
minimumBy cmp xs        =  foldl1 minBy xs
                        where
                           minBy x y = case cmp x y of
                                       GT -> y
                                       _  -> x
-- 
-- | The 'genericLength' function is an overloaded version of 'length'.  In
-- particular, instead of returning an 'Int', it returns any type which is
-- an instance of 'Num'.  It is, however, less efficient than 'length'.
genericLength           :: (Num i) => [a] -> i
{-# NOINLINE [1] genericLength #-}
genericLength xs = let
                     genericLength' []        =  fromInteger (Z# 0#)
                     genericLength' (_:l)     =  fromInteger (Z# 1#) + genericLength' l
                   in
                     case typeIndex# xs `adjStr` xs of
                        1# -> fromInteger $ Z# (strLen# xs)
                        _ -> genericLength' xs
-- 
-- {-# RULES
--   "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
--   "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
--  #-}
-- 
-- strictGenericLength     :: (Num i) => [b] -> i
-- strictGenericLength l   =  gl l 0
--                         where
--                            gl [] a     = a
--                            gl (_:xs) a = let a' = a + 1 in a' `seq` gl xs a'

-- | The 'genericTake' function is an overloaded version of 'take', which
-- accepts any 'Integral' value as the number of elements to take.
genericTake             :: (Integral i) => i -> [a] -> [a]
genericTake n xs = let
                     genericTake' n _ | n <= fromInteger (Z# 0#) = []
                     genericTake' _ []        =  []
                     genericTake' n (x:xs)    =  x : genericTake' (n-fromInteger (Z# 1#)) xs

                     I# n' = fromIntegral n
                   in case typeIndex# xs `adjStr` xs of
                     1# -> strSubstr# xs 0# n'
                     _ -> genericTake' n xs

-- | The 'genericDrop' function is an overloaded version of 'drop', which
-- accepts any 'Integral' value as the number of elements to drop.
genericDrop             :: (Integral i) => i -> [a] -> [a]
genericDrop n xs = let
                     genericDrop' n xs | n <= fromInteger (Z# 0#) = xs
                     genericDrop' _ []        =  []
                     genericDrop' n (_:xs)    =  genericDrop' (n-fromInteger (Z# 1#)) xs

                     I# n' = fromIntegral n
                   in case typeIndex# xs `adjStr` xs of
                     1# -> let !len = strLen# xs in strSubstr# xs n' len
                     _ -> genericDrop' n xs



-- | The 'genericSplitAt' function is an overloaded version of 'splitAt', which
-- accepts any 'Integral' value as the position at which to split.
genericSplitAt          :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt n xs = (genericTake n xs, genericDrop n xs)
-- genericSplitAt n xs | n <= fromInteger (Z# 0#) =  ([],xs)
-- genericSplitAt _ []     =  ([],[])
-- genericSplitAt n (x:xs) =  (x:xs',xs'') where
--     (xs',xs'') = genericSplitAt (n-fromInteger (Z# 1#)) xs

-- | The 'genericIndex' function is an overloaded version of '!!', which
-- accepts any 'Integral' value as the index.
genericIndex :: (Integral i) => [a] -> i -> a
genericIndex xs m = let
                        genericIndex' (x:_)  n | n == fromInteger (Z# 0#) = x
                        genericIndex' (_:xs) n
                           | n > fromInteger (Z# 0#)     = genericIndex' xs (n-fromInteger (Z# 1#))
                           | otherwise = errorWithoutStackTrace "List.genericIndex: negative argument."
                        genericIndex' _ _      = errorWithoutStackTrace "List.genericIndex: index too large."

                        strGenericIndex xs n
                           -- | n < fromInteger (Z# 0#) = errorWithoutStackTrace "SMT String List.genericIndex: negative argument."
                           | (h:_) <- i = h
                           | otherwise  = errorWithoutStackTrace "SMT String List.genericIndex: error with smtlib str.at"
                           where I# n' = fromIntegral n
                                 i = strAt# xs n'
                    in case typeIndex# xs `adjStr` xs of
                        1# -> strGenericIndex xs m
                        _ -> genericIndex' xs m

-- | The 'genericReplicate' function is an overloaded version of 'replicate',
-- which accepts any 'Integral' value as the number of repetitions to make.
genericReplicate        :: forall i a . (Integral i) => i -> a -> [a]
genericReplicate n x = let
                            potential_str = (x:[])

                            rep n x = genericTake n (repeat x)

                            smt_rep_quant =
                              let
                                 I# len = fromIntegral n
                                 !xs = symgen @[a]

                                 !sl_xs = strLen# xs
                                 rep_prop1 = sl_xs $==# len
                                 rep_prop2 i = 0# $<=# i &&# i $<# sl_xs ==> strAt# xs i `strEq#`potential_str
                              in
                              assume rep_prop1 (assume (forAllInt# rep_prop2) xs)
                            -- Non-infinite version for SMT Strings
                            -- Not an optimization- needed to prevent infinite computation,
                            -- otherwise genericTake will try to fully evaluate `repeat x`
                            smt_rep n x = map (const x) [1..(fromIntegral n) :: Int]

                       in case typeIndex# potential_str `adjStr` potential_str of
                            1# -> case strQuantifiers 1# of
                                    1# -> smt_rep_quant
                                    _ -> smt_rep n x
                            _ -> rep n x

-- | The 'zip4' function takes four lists and returns a list of
-- quadruples, analogous to 'zip'.
zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4                    =  zipWith4 (,,,)

-- | The 'zip5' function takes five lists and returns a list of
-- five-tuples, analogous to 'zip'.
zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5                    =  zipWith5 (,,,,)

-- | The 'zip6' function takes six lists and returns a list of six-tuples,
-- analogous to 'zip'.
zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [(a,b,c,d,e,f)]
zip6                    =  zipWith6 (,,,,,)

-- | The 'zip7' function takes seven lists and returns a list of
-- seven-tuples, analogous to 'zip'.
zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
zip7                    =  zipWith7 (,,,,,,)

-- | The 'zipWith4' function takes a function which combines four
-- elements, as well as four lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                        =  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _      =  []

-- | The 'zipWith5' function takes a function which combines five
-- elements, as well as five lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith5                :: (a->b->c->d->e->f) ->
                           [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                        =  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _    = []

-- | The 'zipWith6' function takes a function which combines six
-- elements, as well as six lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith6                :: (a->b->c->d->e->f->g) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                        =  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _  = []

-- | The 'zipWith7' function takes a function which combines seven
-- elements, as well as seven lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith7                :: (a->b->c->d->e->f->g->h) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

-- | The 'unzip4' function takes a list of quadruples and returns four
-- lists, analogous to 'unzip'.
unzip4                  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4                  =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
                                        (a:as,b:bs,c:cs,d:ds))
                                 ([],[],[],[])

-- | The 'unzip5' function takes a list of five-tuples and returns five
-- lists, analogous to 'unzip'.
unzip5                  :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5                  =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es))
                                 ([],[],[],[],[])

-- | The 'unzip6' function takes a list of six-tuples and returns six
-- lists, analogous to 'unzip'.
unzip6                  :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6                  =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
                                 ([],[],[],[],[],[])

-- | The 'unzip7' function takes a list of seven-tuples and returns
-- seven lists, analogous to 'unzip'.
unzip7          :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7          =  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
                                (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
                         ([],[],[],[],[],[],[])


-- | The 'deleteFirstsBy' function takes a predicate and two lists and
-- returns the first list with the first occurrence of each element of
-- the second list removed.
deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

-- | The 'group' function takes a list and returns a list of lists such
-- that the concatenation of the result is equal to the argument.  Moreover,
-- each sublist in the result contains only equal elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to supply
-- their own equality test.
group                   :: Eq a => [a] -> [[a]]
group                   =  groupBy (==)

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- | The 'inits' function returns all initial segments of the argument,
-- shortest first.  For example,
--
-- > inits "abc" == ["","a","ab","abc"]
--
-- Note that 'inits' has the following strictness property:
-- @inits (xs ++ _|_) = inits xs ++ _|_@
--
-- In particular,
-- @inits _|_ = [] : _|_@
inits                   :: [a] -> [[a]]
inits                   = map toListSB . scanl' snocSB emptySB
{-# NOINLINE inits #-}

-- We do not allow inits to inline, because it plays havoc with Call Arity
-- if it fuses with a consumer, and it would generally lead to serious
-- loss of sharing if allowed to fuse with a producer.

-- | The 'tails' function returns all final segments of the argument,
-- longest first.  For example,
--
-- > tails "abc" == ["abc", "bc", "c",""]
--
-- Note that 'tails' has the following strictness property:
-- @tails _|_ = _|_ : _|_@
tails                   :: [a] -> [[a]]
{-# INLINABLE tails #-}
tails lst               =  build (\c n ->
  let tailsGo xs = xs `c` case xs of
                             []      -> n
                             _ : xs' -> tailsGo xs'
  in tailsGo lst)

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r


-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)


------------------------------------------------------------------------------
-- Quick Sort algorithm taken from HBC's QSort library.

-- | The 'sort' function implements a stable sorting algorithm.
-- It is a special case of 'sortBy', which allows the programmer to supply
-- their own comparison function.
sort :: (Ord a) => [a] -> [a]
-- 
-- -- | The 'sortBy' function is the non-overloaded version of 'sort'.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- 
-- #ifdef USE_REPORT_PRELUDE
sort = sortBy compare
sortBy cmp = foldr (insertBy cmp) []
-- #else
-- 
-- {-
-- GHC's mergesort replaced by a better implementation, 24/12/2009.
-- This code originally contributed to the nhc12 compiler by Thomas Nordin
-- in 2002.  Rumoured to have been based on code by Lennart Augustsson, e.g.
--     http://www.mail-archive.com/haskell@haskell.org/msg01822.html
-- and possibly to bear similarities to a 1982 paper by Richard O'Keefe:
-- "A smooth applicative merge sort".
-- 
-- Benchmarks show it to be often 2x the speed of the previous implementation.
-- Fixes ticket http://ghc.haskell.org/trac/ghc/ticket/2143
-- -}
-- 
-- sort = sortBy compare
-- sortBy cmp = mergeAll . sequences
--   where
--     sequences (a:b:xs)
--       | a `cmp` b == GT = descending b [a]  xs
--       | otherwise       = ascending  b (a:) xs
--     sequences xs = [xs]
-- 
--     descending a as (b:bs)
--       | a `cmp` b == GT = descending b (a:as) bs
--     descending a as bs  = (a:as): sequences bs
-- 
--     ascending a as (b:bs)
--       | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
--     ascending a as bs   = as [a]: sequences bs
-- 
--     mergeAll [x] = x
--     mergeAll xs  = mergeAll (mergePairs xs)
-- 
--     mergePairs (a:b:xs) = merge a b: mergePairs xs
--     mergePairs xs       = xs
-- 
--     merge as@(a:as') bs@(b:bs')
--       | a `cmp` b == GT = b:merge as  bs'
--       | otherwise       = a:merge as' bs
--     merge [] bs         = bs
--     merge as []         = as
-- 
-- {-
-- sortBy cmp l = mergesort cmp l
-- sort l = mergesort compare l
-- 
-- Quicksort replaced by mergesort, 14/5/2002.
-- 
-- From: Ian Lynagh <igloo@earth.li>
-- 
-- I am curious as to why the List.sort implementation in GHC is a
-- quicksort algorithm rather than an algorithm that guarantees n log n
-- time in the worst case? I have attached a mergesort implementation along
-- with a few scripts to time it's performance, the results of which are
-- shown below (* means it didn't finish successfully - in all cases this
-- was due to a stack overflow).
-- 
-- If I heap profile the random_list case with only 10000 then I see
-- random_list peaks at using about 2.5M of memory, whereas in the same
-- program using List.sort it uses only 100k.
-- 
-- Input style     Input length     Sort data     Sort alg    User time
-- stdin           10000            random_list   sort        2.82
-- stdin           10000            random_list   mergesort   2.96
-- stdin           10000            sorted        sort        31.37
-- stdin           10000            sorted        mergesort   1.90
-- stdin           10000            revsorted     sort        31.21
-- stdin           10000            revsorted     mergesort   1.88
-- stdin           100000           random_list   sort        *
-- stdin           100000           random_list   mergesort   *
-- stdin           100000           sorted        sort        *
-- stdin           100000           sorted        mergesort   *
-- stdin           100000           revsorted     sort        *
-- stdin           100000           revsorted     mergesort   *
-- func            10000            random_list   sort        0.31
-- func            10000            random_list   mergesort   0.91
-- func            10000            sorted        sort        19.09
-- func            10000            sorted        mergesort   0.15
-- func            10000            revsorted     sort        19.17
-- func            10000            revsorted     mergesort   0.16
-- func            100000           random_list   sort        3.85
-- func            100000           random_list   mergesort   *
-- func            100000           sorted        sort        5831.47
-- func            100000           sorted        mergesort   2.23
-- func            100000           revsorted     sort        5872.34
-- func            100000           revsorted     mergesort   2.24
-- 
-- mergesort :: (a -> a -> Ordering) -> [a] -> [a]
-- mergesort cmp = mergesort' cmp . map wrap
-- 
-- mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
-- mergesort' _   [] = []
-- mergesort' _   [xs] = xs
-- mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)
-- 
-- merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
-- merge_pairs _   [] = []
-- merge_pairs _   [xs] = [xs]
-- merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss
-- 
-- merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- merge _   [] ys = ys
-- merge _   xs [] = xs
-- merge cmp (x:xs) (y:ys)
--  = case x `cmp` y of
--         GT -> y : merge cmp (x:xs)   ys
--         _  -> x : merge cmp    xs (y:ys)
-- 
-- wrap :: a -> [a]
-- wrap x = [x]
-- 
-- 
-- 
-- OLDER: qsort version
-- 
-- -- qsort is stable and does not concatenate.
-- qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- qsort _   []     r = r
-- qsort _   [x]    r = x:r
-- qsort cmp (x:xs) r = qpart cmp x xs [] [] r
-- 
-- -- qpart partitions and sorts the sublists
-- qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
-- qpart cmp x [] rlt rge r =
--     -- rlt and rge are in reverse order and must be sorted with an
--     -- anti-stable sorting
--     rqsort cmp rlt (x:rqsort cmp rge r)
-- qpart cmp x (y:ys) rlt rge r =
--     case cmp x y of
--         GT -> qpart cmp x ys (y:rlt) rge r
--         _  -> qpart cmp x ys rlt (y:rge) r
-- 
-- -- rqsort is as qsort but anti-stable, i.e. reverses equal elements
-- rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- rqsort _   []     r = r
-- rqsort _   [x]    r = x:r
-- rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r
-- 
-- rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
-- rqpart cmp x [] rle rgt r =
--     qsort cmp rle (x:qsort cmp rgt r)
-- rqpart cmp x (y:ys) rle rgt r =
--     case cmp y x of
--         GT -> rqpart cmp x ys rle (y:rgt) r
--         _  -> rqpart cmp x ys (y:rle) rgt r
-- -}
-- 
-- #endif /* USE_REPORT_PRELUDE */

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy (comparing f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- @since 4.8.0.0
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
-- 
-- -- | The 'unfoldr' function is a \`dual\' to 'foldr': while 'foldr'
-- -- reduces a list to a summary value, 'unfoldr' builds a list from
-- -- a seed value.  The function takes the element and returns 'Nothing'
-- -- if it is done producing the list or returns 'Just' @(a,b)@, in which
-- -- case, @a@ is a prepended to the list and @b@ is used as the next
-- -- element in a recursive call.  For example,
-- --
-- -- > iterate f == unfoldr (\x -> Just (x, f x))
-- --
-- -- In some cases, 'unfoldr' can undo a 'foldr' operation:
-- --
-- -- > unfoldr f' (foldr f z xs) == xs
-- --
-- -- if the following holds:
-- --
-- -- > f' (f x y) = Just (x,y)
-- -- > f' z       = Nothing
-- --
-- -- A simple use of unfoldr:
-- --
-- -- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- -- >  [10,9,8,7,6,5,4,3,2,1]
-- --
-- 
-- -- Note [INLINE unfoldr]
-- -- We treat unfoldr a little differently from some other forms for list fusion
-- -- for two reasons:
-- --
-- -- 1. We don't want to use a rule to rewrite a basic form to a fusible
-- -- form because this would inline before constant floating. As Simon Peyton-
-- -- Jones and others have pointed out, this could reduce sharing in some cases
-- -- where sharing is beneficial. Thus we simply INLINE it, which is, for
-- -- example, how enumFromTo::Int becomes eftInt. Unfortunately, we don't seem
-- -- to get enough of an inlining discount to get a version of eftInt based on
-- -- unfoldr to inline as readily as the usual one. We know that all the Maybe
-- -- nonsense will go away, but the compiler does not.
-- --
-- -- 2. The benefit of inlining unfoldr is likely to be huge in many common cases,
-- -- even apart from list fusion. In particular, inlining unfoldr often
-- -- allows GHC to erase all the Maybes. This appears to be critical if unfoldr
-- -- is to be used in high-performance code. A small increase in code size
-- -- in the relatively rare cases when this does not happen looks like a very
-- -- small price to pay.
-- --
-- -- Doing a back-and-forth dance doesn't seem to accomplish anything if the
-- -- final form has to be inlined in any case.
-- 
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- 
{-# INLINE unfoldr #-} -- See Note [INLINE unfoldr]
unfoldr f b0 = build (\c n ->
  let go b = case f b of
               Just (a, new_b) -> a `c` go new_b
               Nothing         -> n
  in go b0)
-- 
-- -- -----------------------------------------------------------------------------
-- -- Functions on strings
-- 
-- -- | 'lines' breaks a string up into a list of strings at newline
-- -- characters.  The resulting strings do not contain newlines.
-- --
-- -- Note that after splitting the string at newline characters, the
-- -- last part of the string is considered a line even if it doesn't end
-- -- with a newline. For example,
-- --
-- -- > lines "" == []
-- -- > lines "\n" == [""]
-- -- > lines "one" == ["one"]
-- -- > lines "one\n" == ["one"]
-- -- > lines "one\n\n" == ["one",""]
-- -- > lines "one\ntwo" == ["one","two"]
-- -- > lines "one\ntwo\n" == ["one","two"]
-- --
-- -- Thus @'lines' s@ contains at least as many elements as newlines in @s@.
lines                   :: String -> [String]
-- lines ""                =  []
lines xs =
   let
      smtLines ys =
         let
            !end_ln = ['\n']
            !end = symgen @Bool
         in
         case end of
            True ->
                  let
                     !ys_index_x = strIndexOf# ys end_ln 0#
                     !ys_no_x = ys_index_x $==# -1#
                  in
                  case ys == "" of
                     False -> assume (ys_no_x) [ys]
                     True -> []
            False ->
                  let
                     !as = symgen @String
                     !bs = symgen @String
                  
                     !as_index_x = strIndexOf# as end_ln 0# 
                     !as_no_x = as_index_x $==# -1#

                     !as_x_app = as `strAppend#` end_ln
                     !full_list = as_x_app `strAppend#` bs
                     !ys_eq_fl = ys `strEq#` full_list
                  in
                  assume as_no_x . assume ys_eq_fl $ as:smtLines bs

      lines' [] = []
      -- -- Somehow GHC doesn't detect the selector thunks in the below code,
      -- -- so s' keeps a reference to the first line via the pair and we have
      -- -- a space leak (cf. #4334).
      -- -- So we need to make GHC see the selector thunks with a trick.
      -- lines s                 =  cons (case break (== '\n') s of
      lines' s                 =  cons (case break (== (C# '\n'#)) s of
                                          (l, s') -> (l, case s' of
                                                         []      -> []
                                                         _:s''   -> lines' s''))
            where
               cons ~(h, t)        =  h : t
   in
   case typeIndex# xs `adjStr` xs of
      1# -> smtLines xs
      _ -> lines' xs
   
-- 
-- -- | 'unlines' is an inverse operation to 'lines'.
-- -- It joins lines, after appending a terminating newline to each.
unlines                 :: [String] -> String
-- #ifdef USE_REPORT_PRELUDE
-- unlines                 =  concatMap (++ "\n")
unlines                 =  concatMap (++ [C# '\n'#])
-- #else
-- -- HBC version (stolen)
-- -- here's a more efficient version
-- unlines [] = []
-- unlines (l:ls) = l ++ '\n' : unlines ls
-- #endif
-- 
-- -- | 'words' breaks a string up into a list of words, which were delimited
-- -- by white space.
words                   :: String -> [String]
{-# NOINLINE [1] words #-}
words s                 =  case dropWhile {-partain:Char.-}isSpace s of
--                                 "" -> []
                                [] -> []
                                s' -> w : words s''
                                      where (w, s'') =
                                             break {-partain:Char.-}isSpace s'
-- 
{-# RULES
"words" [~1] forall s . words s = build (\c n -> wordsFB c n s)
"wordsList" [1] wordsFB (:) [] = words
 #-}
wordsFB :: ([Char] -> b -> b) -> b -> String -> b
{-# NOINLINE [0] wordsFB #-}
wordsFB c n = go
  where
    go s = case dropWhile isSpace s of
--              "" -> n
             [] -> n
             s' -> w `c` go s''
                   where (w, s'') = break isSpace s'
-- 
-- -- | 'unwords' is an inverse operation to 'words'.
-- -- It joins words with separating spaces.
unwords                 :: [String] -> String
-- #ifdef USE_REPORT_PRELUDE
-- unwords []              =  ""
unwords []              =  []
-- unwords ws              =  foldr1 (\w s -> w ++ ' ':s) ws
unwords ws              =  foldr1 (\w s -> w ++ (C# ' '#):s) ws
-- #else
-- -- Here's a lazier version that can get the last element of a
-- -- _|_-terminated list.
-- {-# NOINLINE [1] unwords #-}
-- unwords []              =  ""
-- unwords (w:ws)          = w ++ go ws
--   where
--     go []     = ""
--     go (v:vs) = ' ' : (v ++ go vs)
-- 
-- -- In general, the foldr-based version is probably slightly worse
-- -- than the HBC version, because it adds an extra space and then takes
-- -- it back off again. But when it fuses, it reduces allocation. How much
-- -- depends entirely on the average word length--it's most effective when
-- -- the words are on the short side.
-- {-# RULES
-- "unwords" [~1] forall ws .
--    unwords ws = tailUnwords (foldr unwordsFB "" ws)
-- "unwordsList" [1] forall ws .
--    tailUnwords (foldr unwordsFB "" ws) = unwords ws
--  #-}
-- 
-- {-# INLINE [0] tailUnwords #-}
-- tailUnwords           :: String -> String
-- tailUnwords []        = []
-- tailUnwords (_:xs)    = xs
-- 
-- {-# INLINE [0] unwordsFB #-}
-- unwordsFB               :: String -> String -> String
-- unwordsFB w r           = ' ' : w ++ r
-- #endif
-- 
{- A "SnocBuilder" is a version of Chris Okasaki's banker's queue that supports
toListSB instead of uncons. In single-threaded use, its performance
characteristics are similar to John Hughes's functional difference lists, but
likely somewhat worse. In heavily persistent settings, however, it does much
better, because it takes advantage of sharing. The banker's queue guarantees
(amortized) O(1) snoc and O(1) uncons, meaning that we can think of toListSB as
an O(1) conversion to a list-like structure a constant factor slower than
normal lists--we pay the O(n) cost incrementally as we consume the list. Using
functional difference lists, on the other hand, we would have to pay the whole
cost up front for each output list. -}

{- We store a front list, a rear list, and the length of the queue.  Because we
only snoc onto the queue and never uncons, we know it's time to rotate when the
length of the queue plus 1 is a power of 2. Note that we rely on the value of
the length field only for performance.  In the unlikely event of overflow, the
performance will suffer but the semantics will remain correct.  -}

data SnocBuilder a = SnocBuilder {-# UNPACK #-} !Word [a] [a]

{- Smart constructor that rotates the builder when lp is one minus a power of
2. Does not rotate very small builders because doing so is not worth the
trouble. The lp < 255 test goes first because the power-of-2 test gives awful
branch prediction for very small n (there are 5 powers of 2 between 1 and
16). Putting the well-predicted lp < 255 test first avoids branching on the
power-of-2 test until powers of 2 have become sufficiently rare to be predicted
well. -}

{-# INLINE sb #-}
sb :: Word -> [a] -> [a] -> SnocBuilder a
sb lp f r
   = SnocBuilder lp f r
--   | lp < 255 || (lp .&. (lp + 1)) /= 0 = SnocBuilder lp f r
--   | otherwise                          = SnocBuilder lp (f ++ reverse r) []

-- The empty builder

emptySB :: SnocBuilder a
emptySB = SnocBuilder (fromInteger (Z# 0#)) [] []

-- Add an element to the end of a queue.

snocSB :: SnocBuilder a -> a -> SnocBuilder a
snocSB (SnocBuilder lp f r) x = sb (lp + fromInteger (Z# 1#)) f (x:r)

-- Convert a builder to a list

toListSB :: SnocBuilder a -> [a]
toListSB (SnocBuilder _ f r) = f ++ reverse r
