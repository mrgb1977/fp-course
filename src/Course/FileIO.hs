{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \a ->
  case a of
    (h:._) -> run h
    Nil -> putStrLn "need to pass args!"
--  error "todo: Course.FileIO#main"


--getArgs >>= \a ->
--  case a of
--    (h:._) -> undefined
--    Nil -> putStrLn "need to pass args!"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run file =
  readFile file >>= \c ->
  getFiles (lines c) >>= \d ->
  printFiles d 
--
--alt do notation:
-- do c <- readFile file
--      <- getFiles (lines c)
--      printFiles d
--
--
-- gluing IO actions together?
--  error "todo: Course.FileIO#run"

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles files = sequence (getFile <$> files)
-- sequence . (<$>) getFile
--  error "todo: Course.FileIO#getFiles"

getFile ::
  FilePath                -- 
  -> IO (FilePath, Chars) -- 
getFile n = -- f = ((pure f), (readFile f))
--  readFile n >>= \c -> pure (n, c)
  (>>=) (readFile n) (\c -> pure (n, c))
--       (IO Chars)
--(>>=)  f        a -> (a -> f    b) ->     f b
--(>>=) :: Bind f => f a -> (a -> f b) -> f b
--pure :: Applicative f => a -> f a
-- or
-- bind and pure may as well have just done fmap
-- (\c -> (n,c)) <$> readFile n)
--  error "todo: Course.FileIO#getFile"

-- <$> :: (               Char -> (FilePath, Chars)) -> IO Chars -> IO (FilePath, Chars)
-- (<$>) :: Functor f => (a    ->  b)                -> f  a     -> f   b
-- 
--

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files = 
  void (sequence ((\(n,c) -> printFile n c) <$> files))
-- void . sequence . (<$>) . (uncurry printFile)
--  error "todo: Course.FileIO#printFiles"

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile n c = --fp = readFile fp
  putStrLn("======== " ++n) >>= \_ ->
  putStrLn c

--  error "todo: Course.FileIO#printFile"

