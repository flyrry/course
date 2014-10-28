{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
anagrams word file =
  (\text -> intersectBy (\a b -> equalIgnoringCase a b) (lines text) (permutations word)) <$> (readFile file)

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase s1 s2 = on (\a b -> a == b) (\s -> map toLower s) s1 s2
