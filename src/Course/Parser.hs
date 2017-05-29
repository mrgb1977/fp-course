{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseError =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving Eq


instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show Failed =
    "Parse failed"

data ParseResult a =
  ErrorResult ParseError
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) =
    show e
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (ErrorResult _) =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser ::
  Char
  -> Parser a
unexpectedCharParser c =
  P (\_ -> ErrorResult (UnexpectedChar c))

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
valueParser a = P (\input -> Result input a)
--  error "todo: Course.Parser#valueParser"

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True
failed ::
  Parser a
failed = P (\_ -> ErrorResult Failed)
--  error "todo: Course.Parser#failed"

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character ::
  Parser Char
character =
  P (\input -> case input of 
                 Nil -> ErrorResult UnexpectedEof
                 h:.t -> Result t h)
-- i did this but it is not exhaustive
--character = P (\(h:.t) -> if ((h:.t)==Nil) then ErrorResult Failed else Result t h)
--  error "todo: Course.Parser#character"

-- | Return a parser that maps any succeeding result with the given function.
--
-- >>> parse (mapParser succ character) "amz"
-- Result >mz< 'b'
--
-- >>> parse (mapParser (+10) (valueParser 7)) ""
-- Result >< 17
mapParser ::
  (a -> b)
  -> Parser a
  -> Parser b
-- f :: a -> b
-- k :: Input -> ParseResult a
-- k input :: ParseResult a
---
-- ? :: ParseResult b
mapParser f (P k) = 
  P(\input -> case k input of
                ErrorResult e -> ErrorResult e
                Result j a -> Result j (f a))
--  error "todo: Course.Parser#mapParser"
-- instead of (P k) could just use p and (case parse p input)

-- | This is @mapParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flmapParser ::
  Parser a
  -> (a -> b)
  -> Parser b
flmapParser =
  flip mapParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True
bindParser ::
  (a -> Parser b)
  -> Parser a
  -> Parser b
bindParser f p = P(\input -> case parse p input of
                               ErrorResult e -> ErrorResult e
                               -- j :: Input
                               -- a :: a
                               -- f :: a -> Parser b
                               ----
                               -- ? :: ParseResult b
                               Result j a -> parse (f a) j)
--  error "todo: Course.Parser#bindParser"

-- | This is @bindParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
flbindParser =
  flip bindParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@ or @flbindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
(>>>) x y = flbindParser x (\_ -> y)
-- could point free using const
--
--  error "todo: Course.Parser#(>>>)"

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
--(|||) x y = P(\input -> case parse x input of
--                          ErrorResult _ -> parse y input
--                          Result _ _ -> parse x input)
--
-- overlapping patterns
--(|||) x y = P(\input -> case parse x input of
--                          ErrorResult _ -> parse y input
--                          r -> r)
--
-- @ notations
--(|||) x y = P(\input -> case parse x input of
--                          ErrorResult _ -> parse y input
--                          r@(Result _ _) -> r)
--
-- use let might be neater and more efficient
(|||) x y = P(\input -> let o = parse x input in
                        case o of
                          ErrorResult _ -> parse y input
                          Result _ _ -> o)
--  error "todo: Course.Parser#(|||)"
infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list ::
  Parser a
  -> Parser (List a)
list p = list1 p ||| (valueParser Nil)
--  error "todo: Course.Parser#list"

-- list - parsing a non-empty list OR (|||) the parser that always produces Nil
-- list1 p - get a from p, then as from list of p then cons a:.as

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @bindParser@, @list@ and @valueParser@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
list1 ::
  Parser a
  -> Parser (List a)
list1 p = 
  flbindParser p (\x ->
    flbindParser (list p) (\xs ->
      valueParser (x:.xs)))
-- get a from p (run p to produce a) flbindParser p (\a ->
-- get as from list of p (run list of p to produce as) flbindParser (list p) (\a ->
-- return (a:.as) valueParser (a:.as)
--  error "todo: Course.Parser#list1"

-- do notation
-- do a <- p
--  as <- list p
--  pure (a:.as)

--or
-- p >>= \a ->
--   list p >>= \as ->
--   valueParser (a:.as)

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy p = character >>= \c -> if p c then valueParser c else unexpectedCharParser c
--satisfy p = P(\(h:.t) -> if f h then valueParser h else 
--  error "todo: Course.Parser#satisfy"

-- 
--
--

-- fails if input empty or character does not satisfy predicate

--
--
--
-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
is a = satisfy (\x -> x == a)
-- satisy ((==) c)
-- is = satisfy . (==)
--  error "todo: Course.Parser#is"

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
digit ::
  Parser Char
digit = satisfy (\x -> isDigit(x))
-- digit = satisfy isDigit
--  error "todo: Course.Parser#digit"

-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a valid series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list1@, @read@ and @digit@
-- functions.
-- >>> parse natural "123"
-- Result >< 123
--
-- >>> parse natural "123ab"
-- Result >ab< 123
--
-- >>> isErrorResult (parse natural "abc")
-- True
--
-- >>> isErrorResult (parse natural "")
-- True
natural ::
  Parser Int
natural = 
  list1 digit >>= \d ->
  case read d of
    Full n -> valueParser n
    Empty -> failed

-- run 1 or more digits to produce d
-- if (read d) gives Full (Int) then yay else no
--  error "todo: Course.Parser#natural"

-- can factor out pattern matching Empty/ Full using optional below
-- or in Haskell use maybe for Maybe a
-- catamporphism
optional ::
  x
  -> (a-> x)
  -> Optional a
  -> x
optional e _ Empty = e
optional _ f (Full a) = f a

-- foldRight is the catamorphism for list
--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
space ::
  Parser Char
space = satisfy isSpace
--  error "todo: Course.Parser#space"

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 ::
  Parser Chars
spaces1 = list1 space --if list1 space then spaces else
--  error "todo: Course.Parser#spaces1"

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
lower ::
  Parser Char
lower = satisfy isLower
--  error "todo: Course.Parser#lower"

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
upper ::
  Parser Char
upper = satisfy isUpper
--  error "todo: Course.Parser#upper"

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
alpha ::
  Parser Char
alpha = satisfy isAlpha
--  error "todo: Course.Parser#alpha"

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
--sequenceParser = bindParser 
-- same as in Applicative
-- this means Parsers are Applicative
sequenceParser = foldRight (lift2 (:.)) (pure Nil)
--  error "todo: Course.Parser#sequenceParser"

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)
thisMany = replicateA
--  error "todo: Course.Parser#thisMany"

-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@.
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
ageParser = natural
--  error "todo: Course.Parser#ageParser"

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
firstNameParser = --list lower
 do c <- upper
    d <- list lower
    pure (c:.d)
--  error "todo: Course.Parser#firstNameParser"
--
--
-- do notation bind?
--

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Chars
surnameParser =
 do c <- upper
    d <- thisMany 5 lower
    e <- list lower
    valueParser (c:.(d ++ e))
--  error "todo: Course.Parser#surnameParser"

--
-- bind notation
-- run upper case to get c      : upper >>= \c ->
-- run 5 lower to get d         : thisMany 5 lower >>= \d ->
-- run 0 or more lower to get e : list lower >>= \e ->
--                              : pure (c:. d ++ e)
--
--

-- (\c d e -> c :. d ++ e) <$> upper <*> thisMany 5 lower <*> list lower
--
-- which is lift3
-- lift3 (\c d e -> c :. d ++ e) upper (thisMany 5 lower) (list lower)
--
-- NB valueParser is pure
--
--

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< 'y'
--
-- >>> parse smokerParser "nabc"
-- Result >abc< 'n'
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser ::
  Parser Char
smokerParser = is('y') ||| is ('n')
--  error "todo: Course.Parser#smokerParser"

-- | Write part of a parser for Person#phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Chars
phoneBodyParser = list (digit ||| is('.') ||| is ('-'))
--  error "todo: Course.Parser#phoneBodyParser"

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser ::
  Parser Chars
phoneParser =
 do c <- digit
    d <- phoneBodyParser
    e <- is('#')
    pure (c:.d)
--  error "todo: Course.Parser#phoneParser"

-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
personParser ::
  Parser Person
personParser =
-- do c <-
--return Person age, firstname...
-- pure Person(123, "fred", "clark", 'y', 123-256.789)
  do a <- natural
     _ <- spaces1
     f <- firstNameParser
     _ <- spaces1
     s <- surnameParser
     _ <- spaces1
     k <- smokerParser
     _ <- spaces1
     p <- phoneParser
 --    pure Person {age = age1, firstName = firstname1, surname = surname1, smoker = smoker1, phone = phone1}
     pure (Person a f s k p)
--  error "todo: Course.Parser#personParser"
--  do a <- natural
--     spaces1
--     f <- firstNameParser
--     spaces1
--     s <- surnameParser
--     spaces1
--     k <- smokerParser
--     spaces1
--     p <- phoneParser
--     pure (Person a f s k p)

--  Person <$> ageParser <* spaces1 <*>
--             firstNameParse <* spaces1 <*>
--             surnameParser <* spaces1 <*>
--             smokerParser <* spaces1 <*>
--             phoneParser


-- run age

-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  (<$>) ::
    (a -> b)
    -> Parser a
    -> Parser b
  (<$>) = mapParser
--     error "todo: Course.Parser (<$>)#instance Parser"

-- | Write a Apply instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Apply Parser where
  (<*>) ::
    Parser (a -> b)
    -> Parser a
    -> Parser b
  (<*>) f a = bindParser (\f' -> mapParser f' a) f
--    error "todo: Course.Parser (<*>)#instance Parser"

-- | Write an Applicative functor instance for a @Parser@.
instance Applicative Parser where
  pure ::
    a
    -> Parser a
  pure = valueParser
 --   error "todo: Course.Parser pure#instance Parser"

-- | Write a Bind instance for a @Parser@.
instance Bind Parser where
  (=<<) ::
    (a -> Parser b)
    -> Parser a
    -> Parser b
  (=<<) = bindParser
--    error "todo: Course.Parser (=<<)#instance Parser"

instance Monad Parser where
