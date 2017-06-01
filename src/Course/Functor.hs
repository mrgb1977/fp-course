{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
  (<$>) ::
    (a -> b)
    -> Id a
    -> Id b
  (<$>) f (Id a) = Id (f a)
--    error "todo: Course.Functor (<$>)#instance Id"

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
--  (<$>) _ Nil = Nil
--  (<$>) f (h:.t) = (f h) :. (<$>) f t 
--  (<$>) f = foldRight ((:.).f) Nil
  (<$>) = map
--    error "todo: Course.Functor (<$>)#instance List"

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
--  (<$>) _ Empty = Empty
--  (<$>) f (Full a) = Full (f a)
  (<$>) = mapOptional
--    error "todo: Course.Functor (<$>)#instance Optional"

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17

--((->) t) type constryctur that takes 1 argument
-- :kind Optional * -> *
-- functor takes one type to a type (unary type constrctor) as per ((->) t)
-- :k (->) * -> * -> *, not a functor (*->*), incorrect kind
-- :k ((->) Int) * -> *
-- return me a unary type constrcuor of the right kind
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
--  (<$>) f g = f . g
  (<$>) = (.)
--    error "todo: Course.Functor (<$>)#((->) t)"

-- (a -> b) -> (typevar -> a) -> (typevar -> b)
--  f       -> (t -> a) -> (t -> b)
-- we have a function and we want to map it over (t -> a), we do this by first doing (t -> a) then
-- put it into the function f which is f . (t->a), or f . g , or (.)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
--(<$) a = (<$>) (const a)
(<$) = (<$>) . const
--  error "todo: Course.Functor#(<$)"

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
-- we want to map the a constant onto our type variable, the constant is
void = (<$) ()

--  error "todo: Course.Functor#void"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
