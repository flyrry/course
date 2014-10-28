{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams word file =
  let sorted_word = qsort $ map toLower word
  in (\text -> foldLeft (\ans w -> if (qsort $ map toLower w) == sorted_word then (w:.ans) else ans) Nil (lines text)) <$> (readFile file)

qsort :: Ord a => List a -> List a
qsort Nil = Nil
qsort (x:.xs) = (qsortby (<=) xs) ++ (x:.Nil) ++ (qsortby (>) xs)
              where qsortby fn ys = qsort $ filter (\y -> y `fn` x) ys


newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
