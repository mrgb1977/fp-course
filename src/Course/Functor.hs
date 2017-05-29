{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P

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
    -> Id b -- data Id x = Id x (Id is data type constructor (as well as data type name, just like Empty/Full, or even :. or Nil from our list!)
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
  (<$>) _ Nil = Nil
  (<$>) f (h:.t) = f h :. (<$>) f t
-- or just
--  (<$>) = map
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
  (<$>) _ Empty = Empty
  (<$>) f (Full a) = Full (f a)
--    error "todo: Course.Functor (<$>)#instance Optional"

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
-- (->) t == t -> ?
-- (->) has kind * -> * -> * (type to type to type)
-- (->) String has kind * -> *
-- (->) String Int has kind *
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a) -- (t -> a)
    -> ((->) t b) -- (t -> b)
-- point full style
--          (a -> b) -> (t -> a) -> (t -> b)
--  (<$>)    a_to_b      t_to_a      t =
--           a_to_b (t_to_a t)
--  (<$>) f g = f . g
  (<$>) = (.)
--    error "todo: Course.Functor (<$>)#((->) t)"

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ [1,2,3]
-- [7,7,7]
--
-- prop> x <$ [a,b,c] == [x,x,x]
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) a g = (\_ -> a) <$> g
--(<$) a b = const a <$> b
--(<$) = (<$>) . const
--  error "todo: Course.Functor#(<$)"

-- | Anonymous map producing unit value.
--
-- >>> void [1,2,3]
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
-- just use the anonymous map to map the unit ()
void ::
  Functor f =>
  f a
  -> f ()
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

instance Functor [] where
  (<$>) =
    P.fmap

instance Functor P.Maybe where
  (<$>) =
    P.fmap
