{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative(
  Applicative(..)
, lift2
, lift3
, lift4
, (*>)
, (<*)
, sequence
, replicateA
, filtering
, return
, fail
, (>>)
) where

import Course.Core
import Course.Functor hiding ((<$>))
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Applicative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
--
-- * The law of left identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of right identity
--   `∀x. x <*> pure id ≅ x`
class Functor f => Applicative f where
  pure ::
    a -> f a
  (<*>) ::       -- apply given an f of a function and an f of a.
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
--(<$>) = (<$>)
(<$>) k fa =
    pure k <*> fa
--given pure and apply we can write fmap!
--  error "todo: Course.Applicative#(<$>)"

-- | Insert into Id.
--
-- prop> pure x == Id x
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Applicative Id where
  pure ::
    a
    -> Id a
  pure = Id
-- or pure a = Id a
--    error "todo: Course.Applicative pure#instance Id"
  (<*>) :: 
    Id (a -> b)
    -> Id a
    -> Id b
  (<*>) (Id f) (Id a) = Id (f a) 
--    error "todo: Course.Applicative (<*>)#instance Id"

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where
  pure ::
    a
    -> List a
  pure a = a :. Nil
--  pure = 
--    error "todo: Course.Applicative pure#instance List"
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
--  (<*>) Nil _ = Nil
--  (<*>) _ Nil = Nil
--  (<*>) (h:.t) a = (map h a) ++ ((<*>) t a)
-- can we fold right, we are doing recursion, we probably can
-- replace ??
  (<*>) f a = foldRight ((++) . (\g -> map g a)) Nil f
-- which then looks like (and is flatmap and map! - everythng that has flatmap and map is apply
--    error "todo: Course.Apply (<*>)#instance List"

-- | Insert into an Optional.
--
-- prop> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure = Full
-- or pure a = Full a
--    error "todo: Course.Applicative pure#instance Optional"
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  (<*>) _ Empty = Empty
  (<*>) Empty _ = Empty
  (<*>) (Full f) (Full a) = Full (f a)
-- of course we could also just use applyOptional!
--    error "todo: Course.Apply (<*>)#instance Optional"

-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure = const 
--    error "todo: Course.Applicative pure#((->) t)"
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
--  (<*>) f g = (\t -> undefined) g
  (<*>) f g t = f t (g t)
--    error "todo: Course.Apply (<*>)#instance ((->) t)"
-- you need a function t->b
-- or given a t you need a b

-- these two functions above themselves are a turing complete programming language (S K I combinator calculus)

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18

-- lift 2 does what? takes a function of two args and apply it to two Applicative data type args and return an applicative data type
lift2 ::
  Applicative f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
--lift2 g fa fb = pure g <*> fa <*> fb
lift2 g fa fb = g <$> fa <*> fb 
--  error "todo: Course.Applicative#lift2"
-- map the function


-- | Apply a ternary function in the environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Applicative f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
--lift3 g fa fb fc = pure g <*> fa <*> fb <*> fc
lift3 g fa fb fc = lift2 g fa fb <*> fc
--  error "todo: Course.Applicative#lift3"

-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Applicative f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 g fa fb fc fd = lift3 g fa fb fc <*> fd
--  error "todo: Course.Applicative#lift4"

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
--(*>) fa fb = (const void fa) <*> fb
--(*>) fa fb = (<$>) (const ()) fa <*> fb
--  error "todo: Course.Applicative#(*>)"
--(*>) da db = _undefined <*> db
(*>) da db = pure (id) <$> da <*> db
-- rewrite in terms of lift2

-- I wanted an identiy function in the shape of the first argument to then apply to fb

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Applicative f =>
  f b
  -> f a
  -> f b
--(<*) da db =  da <*> (pure (id) <$> db)
--(<*) da db = da <*> _undefined
(<*) = lift2 const
-- or (<*) = lift2 pure
--rewrite in terms of pure and id and fmap and apply 
--  error "todo: Course.Applicative#(<*)"

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence Nil = pure Nil
sequence (h:.t) = lift2 (:.) h (sequence t)

-- foldRight (lift2 (:.)) (pure Nil)
 -- h :: f a
 -- t :: List (f a)
 -- sequence t :: f (List a)
 ----
 -- ? :: f (List a)
--  error "todo: Course.Applicative#sequence"

--facts  
--       (:.) ::   x ->    List x  ->   List x
-- lift2 (:.) :: f x -> f (List x) -> f List x)

-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA n fa = sequence (replicate n fa)
--  error "todo: Course.Applicative#replicateA"

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering p = foldRight (\a -> lift2 (\b -> if b then (a:.) else id) (p a)) (pure Nil)
--  error "todo: Course.Applicative#filtering"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist

(>>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
