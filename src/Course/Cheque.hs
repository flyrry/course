{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Bind

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars text =
  let (before, after) = span (\c -> c /= '.') text
      digitFilter = foldRight (\c acc -> case fromChar c of
                                          Full d -> (d:.acc)
                                          Empty -> acc) Nil
      dllrs = groupDigits $ dropWhile isZero $ digitFilter before
      cents = let centDigits = take 2 $ digitFilter after -- "round" cents to 2 digits
              in groupDigits $ if (length centDigits > 1) then centDigits else (centDigits ++ (Zero:.Nil))
      dollarText = if (singles $ reverse dllrs) then "dollar" else "dollars"
      centText = if (singles $ reverse cents) then "cent" else "cents"
      singles Nil = False
      singles (d:._) = case d of
                    D1 x -> isOne x
                    D2 y x -> (not $ isOne y) && isOne x
                    D3 _ y x -> (not $ isOne y) && isOne x
                    where isOne x = x == One
  in spellOut dllrs ++ dollarText ++ " and " ++ spellOut cents ++ centText

spellOut :: List Digit3 -> Chars
spellOut l = foldRight (\(d3, suffix) acc -> adjust (showExtDigit d3) suffix acc)
             Nil
             (reverse $ filter (\(d3, _) -> not $ isRedundant d3) $ zip (reverse l) illion)
           where adjust num suff rest = if (isEmpty suff) then num ++ (' ':.rest) else num ++ (' ':.suff) ++ (' ':.rest)
                 isRedundant z = case z of
                                  --D1 Zero -> True
                                  --D2 Zero Zero -> True
                                  D3 Zero Zero Zero -> True
                                  _ -> False

isZero :: Digit -> Bool
isZero Zero = True
isZero _ = False

groupDigits :: List Digit -> List Digit3
groupDigits Nil = (D1 Zero) :. Nil
groupDigits ld@(x:.xs) =
  let len = length ld
      r = len `mod` 3
      groupIn3s (a:.b:.c:.rest) = (D3 a b c) :. groupIn3s rest
      groupIn3s _ = Nil
  in case r of
      1 -> (D1 x) :. groupIn3s xs
      2 -> (\(x1:.x2:._) -> (D2 x1 x2) :. (groupIn3s $ drop 2 ld)) $ take 2 ld
      _ -> groupIn3s ld

showMDPrefix :: Digit -> Chars
showMDPrefix Two = "twen"
showMDPrefix Three = "thir"
showMDPrefix Four = "for"
showMDPrefix Five = "fif"
showMDPrefix Eight = "eigh"
showMDPrefix x = showDigit x

showExtDigit :: Digit3 -> Chars
showExtDigit (D1 d) = showDigit d
showExtDigit (D2 d1 d2) =
  case d1 of
  Zero -> case d2 of
          Zero -> "zero"
          _ -> showDigit d2
  One -> case d2 of
         Zero -> "ten"
         One -> "eleven"
         Two -> "twelve"
         Three -> "thirteen"
         Four -> "fourteen"
         Five -> "fifteen"
         _ -> showDigit d2 ++ "teen"
  _ ->  case d2 of
        Zero -> showMDPrefix d1 ++ "ty"
        _ -> showMDPrefix d1 ++ "ty-" ++ showDigit d2
showExtDigit (D3 d1 d2 d3) = case d1 of
                              Zero -> showExtDigit (D2 d2 d3)
                              _ -> case d2 of
                                    Zero -> case d3 of
                                            Zero -> showDigit d1 ++ " hundred"
                                            _ -> showDigit d1 ++ " hundred and " ++ showDigit d3
                                    _ -> showDigit d1 ++ " hundred and " ++ showExtDigit (D2 d2 d3)
