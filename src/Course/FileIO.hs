{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
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

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= \a ->
         case a of
           Nil ->
             putStrLn "no Arguments"
           h:._ ->
             run h
--  error "todo: Course.FileIO#main"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run name = readFile name >>= \c -> getFiles (lines c) >>= \z -> printFiles z
--  error "todo: Course.FileIO#run"

--  do c <- readFile name
--     z <- getFiles (lines c)
--     printFiles z

-- referential transparency substitute expressions with their value without changing program

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
--getFiles Nil = pure ()
--getFiles (h:.t) = getFile h <* getFiles t
--  error "todo: Course.FileIO#getFiles"
--getFiles la = join . ((<$>) getFile la)
getFiles ps = sequence ((<$>) getFile ps) -- :: List (IO (fp,c))


getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp =  (<$>) (\c -> (fp, c)) (readFile fp)
--  error "todo: Course.FileIO#getFile"

-- getFile

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil = pure ()
printFiles ((fp, contents):.t) = printFile fp contents <* printFiles t

--recursion so we should be able to foldright
--printFiles = foldRight (\(fp, contents) _ -> printFile fp contents) (pure ())
--  error "todo: Course.FileIO#printFiles"

--Useful Functions --
--
--  getArgs :: IO (List Chars)
--  putStrLn :: Chars -> IO ()
--  readFile :: Chars -> IO Chars
--  lines :: Chars -> List Chars
--  void :: IO a -> IO ()

-- sequence :: Applicative f => List (f a) -> f (List a)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents = 
  do putStrLn ("======== " ++ name)
     putStrLn contents

-- Or putStrLn name *> putStrLn contents
--readFile fp >>= putStrLn 
--  error "todo: Course.FileIO#printFile"

