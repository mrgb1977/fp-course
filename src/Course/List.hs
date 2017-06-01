{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Course.Optional
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t =
  Nil
  | t :. List t
  deriving (Eq, Ord)

-- recursive algebraic data type

-- Right-associative
infixr 5 :.

instance Show t => Show (List t) where
  show = show . foldRight (:) []

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  let inf x = x :. inf (x+1)
  in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> x `headOr` infinity == 0
--
-- prop> x `headOr` Nil == x
headOr ::
  a
  -> List a
  -> a
--headOr a Nil = a
--headOr _ (h:._) = h
--headOr a (h:.t) = foldRight (\a' _ -> a') a (h:.t)
--headOr = foldRight (\a' _ -> a')
headOr = foldRight (const)

-- foldRight replace cons with function and Nil with z (b value)

-- const (a -> b -> a - once inhabited type)


--  error "todo: Course.List#headOr"

-- | The product of the elements of a list.
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product ::
  List Int
  -> Int
--product Nil = 1
--product (h:.t) = h*(product t) 
product = foldRight (*) 1
--  error "todo: Course.List#product"

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> foldLeft (-) (sum x) x == 0
sum ::
  List Int
  -> Int
--sum Nil = 0
--sum (h:.t) = h + sum(t)
sum = foldRight (+) 0
--  error "todo: Course.List#sum"

-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> sum (map (const 1) x) == length x
length ::
  List a
  -> Int
--length Nil = 0
--length (_:.t) = 1 + length(t)
--length = foldLeft (\a' _ -> a' + 1) 0
--length = foldLeft (\a' -> const (a' +1)) 0
--length = foldLeft (\a' -> const ((+1) a')) 0
length = foldLeft (const . (+1)) 0
--(.) compose is a once inhabited type
-- :t (.) 

-- compose definition once inhabited
--(...) :: (b->c) -> (a->b) -> a -> c
--(...)     f         g        x = f (g x)
--(...) f g = \x -> f (g x)

--map onto length above we can get
-- length = foldLeft (const . (+1)) 0
-- conclude \x -> f (g x) equiv to f . g

--  error "todo: Course.List#length"

-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> headOr x (map (+1) infinity) == 1
--
-- prop> map id x == x
map ::
  (a -> b)
  -> List a
  -> List b
--map _ Nil = Nil
--map f (h :. t) = (f h) :. (map f t)
map f = foldRight ((:.) . f) Nil

-- think about it... replace cons with calling k then cons and replace Nil with Nil, which looks like
--map k list = 
-- list = a :. b :. c :. Nil
-- list (:.) a ((:.) b ((:.) c Nil)))

-- mapAgain k list = foldRight undefined _undefined list
-- mapAgain k list = foldRight (\a -> list b -> _undefined) Nil list
-- mapAgain k list = foldRight (\a listb -> k a :. listb) Nil list
-- mapAgain k = foldRight (\a listb -> k a :. listb) Nil
-- mapAgain k = foldRight (\a listb -> (:.) (k a) listb) Nil
-- mapAgain k = foldRight (\a -> (:.) (k a)) Nil
-- mapAgain k = foldRight ((:.) . k) Nil

--READ composition from right to left. First do k and then cons

-- see pointfree.io

-- another way to think about it!
--1.
map2 ::
  (a -> b)
  -> List a
  -> List b 
map2 _ Nil = Nil
map2 f (h:.t) = 
  -- f :: a -> b
  -- h :: a
  -- t :: list a
--  undefined

--2. f h :. undefined

--3. 
--  f h :. _undefined

--4.
  f h :. map2 f t 

--  error "todo: Course.List#map"

-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> headOr x (filter (const True) infinity) == 0
--
-- prop> filter (const True) x == x
--
-- prop> filter (const False) x == Nil
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter _ Nil = Nil
filter f (h:.t) = if f h then h :. (filter f t) else filter f t

--syntax:
--filter f (h:.t) = 
--  let n = filter p t
--  in if p h
--     then h :. n
--     else n

bool :: x -> x -> Bool -> x
bool f _ False = f
bool _ t True = t

--syntax:
--filter f (h:.t) = 
--  let n = filter p t
--  in bool (id n) (h:.n) (p h)

--  in bool (id) (h:.) (p h) n

--  in bool (id) (h:.) (p h)

--filter f = foldRight (\a b -> if f a then 
--  error "todo: Course.List#filter"

--filterr ::
--  (a -> Bool)
--  -> List a
--  -> List a
-- filterr p list = foldRight undefined undefined list
-- filterr p list = foldRight undefined Nil list
-- filterr p list = foldRight (\h t -> if p h then h :. t else t) Nil list
-- filterr p list = foldRight (\h t -> bool id (h:.) (p h)) Nil list
-- filterr p = foldRight (\h -> bool id (h:.) (p h)) Nil 

-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> headOr x (Nil ++ infinity) == 0
--
-- prop> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> x ++ Nil == x
(++) ::
  List a
  -> List a
  -> List a
(++) Nil b = b
(++) (h:.t) b = h :. (++) t b 
--  error "todo: Course.List#(++)"

append2 :: 
  List a
  -> List a
  -> List a
append2 a b = foldRight (:.) b a
-- in append we will replace cons with cons and Nil with the second list
--append2 = \a b -> (foldRight (:.)) b a

-- need function of type (a->b->c) -> (b->a->c);
-- ask hoogle if there is such a thing with this tyype
-- flip
--append2 = flip (foldright(:.))


infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> sum (map length x) == length (flatten x)
flatten ::
  List (List a)
  -> List a
--flatten Nil = Nil
--flatten (h:.t) = h ++ flatten t
flatten = foldRight (++) Nil
--  error "todo: Course.List#flatten"

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> flatMap id (x :: List (List Int)) == flatten x
flatMap ::
  (a -> List b)
  -> List a
  -> List b
--flatMap _ Nil = Nil
--flatMap f (h:.t) = f h ++ flatMap f t
flatMap f = foldRight ((++).f) Nil
--  error "todo: Course.List#flatMap"
-- flatmap f list is the same thing as replacing :. with f and then ++ and replacing Nil with Nil.

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain ::
  List (List a)
  -> List a
--flattenAgain = flatMap (\a -> a)
flattenAgain = flatMap id
--  error "todo: Course.List#flattenAgain"

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values, 
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional = foldRight (twiceOptional (:.)) (Full Nil)
--seqOptional = foldRight _undefined undefined undefined
--seqOptional Nil = Nil
--seqOptional (h :. t) = Full (
--seqOptional = 
--  error "todo: Course.List#seqOptional"

-- use foldRight for a start foldRight undefined undefined, no pattern matching, use bindOptional and mapOptional or twiceOptional

--seqOptional = foldRight undefined _undefined
--seqOptional = foldRight undefined (Full Nil)
--seqOptional = foldRight _undefined (Full Nil)
--seqOptional = foldRight (\optional_a optional_list_a -> _undefined) (Full Nil)
--seqOptional = foldRight (\optional_a optional_list_a -> if ) (Full Nil)

-- :t bindOptional
-- bindOptional :: (a -> Optional b) -> Optional a -> Optional b
-- :t mapOptional
-- mapOptional :: (a -> b) -> Optional a -> Optional b
-- :t twiceOptional
-- twiceOptional
--  :: (a -> b -> c) -> Optional a -> Optional b -> Optional c

-- seqOptional = 
--  foldRight (\opt_a opt_list_a -> bindOptional (\a -> mapOptional (\list_a -> a :. list_a) opt_list_a) opt_a) (Full Nil)

 

   -- opt a :; Optional a
   -- opt_list_a :: Optional (List a)
   -- bindOptional :: (x -> Optional y) -> Optional x -> Optioanl y
   -- mapOptional :: (q -> Optional r) -> Optional q -> optional r
   ----
   -- ? :: List (Optional a)

--  (\opt_a opt_list_a -> bindOptional (\a -> mapOptioanl _undefined opt_list_a) opt_a
    

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
find _ Nil = Empty
find p (h:.t) = if p h then Full h else find p t 

--headOr Empty (filter f l)
--  error "todo: Course.List#find"

--mrb: replace cons with match predicate. check

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
lengthGT4 (_:._:._:._:._) =
  True
lengthGT4 _ =
  False

-- length x > 4

data Natural = Z | Succ Natural
  deriving (Eq, Show)

one = Succ Z
two = Succ one

lengthAgain :: List a -> Natural
lengthAgain Nil = Z
lengthAgain (_:.t) = Succ (lengthAgain t)

--  error "todo: Course.List#lengthGT4"

-- some have written length x > 4, but it fails the last test! infinity does not fit inside Int!

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
--reverse Nil = Nil
--reverse (h:.t) = reverse(t) ++ (h :. Nil) -- wrong!! doesn't work for infinity case, runs in quadratic time ...
--reverse (h:.t) = (++) (reverse t) (h :. Nil) -- not tail recursive, so solution not linear!
reverse = reverse0 Nil

-- to move into tail position we need a helper function with an accumulator
-- this allows us to move reverse into tail position, we can now call it from reverse
reverse0 ::
  List a -> List a -> List a
reverse0 acc Nil = acc
reverse0 acc (h:.t) = reverse0 (h:.acc) t -- this is now tail recursive

reverseAgain ::
  List a
  -> List a
reverseAgain = foldLeft (\r x -> x :. r) Nil

-- use flip
-- foldLeft (flip (:.)) Nil

-- what about foldLeft!!
-- replace z and f with whatever you like to get reverse, z with Nil and f with (x :. r)


--reverse (h:.Nil) = h:.Nil
--reverse (h:.h2:.t) = reverse(h2:.t) :. h
--  error "todo: Course.List#reverse"

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce =
  error "todo: Course.List#produce"

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse =
  error "todo: Is it even possible?"

---- End of list exercises

largeList ::
  List Int
largeList =
  listh [1..50000]

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  Filename
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  Filename
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Chars =
  List Char

type Filename =
  Chars

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Functor List where
  fmap =
    M.liftM

instance A.Applicative List where
  (<*>) =
    M.ap
  pure =
    (:. Nil)

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
