{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Data.Scientific
-- Copyright   :  Bas van Dijk 2013
-- License     :  BSD3
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides the number type 'Scientific'. Scientific numbers are
-- arbitrary precision and space efficient. They are represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>. The
-- implementation uses an 'Integer' 'coefficient' @c@ and an 'Int'
-- 'base10Exponent' @e@. A scientific number corresponds to the 'Fractional'
-- number: @'fromInteger' c * 10 '^^' e@.
--
-- Note that since we're using an 'Int' to represent the exponent these numbers
-- aren't truly arbitrary precision. I intend to change the type of the exponent
-- to 'Integer' in a future release.
--
-- The main application of 'Scientific' is to be used as the target of parsing
-- arbitrary precision numbers coming from an untrusted source. The advantages
-- over using 'Rational' for this are that:
--
-- * A 'Scientific' is more efficient to construct. Rational numbers need to be
-- constructed using '%' which has to compute the 'gcd' of the 'numerator' and
-- 'denominator'.
--
-- * 'Scientific' is safe against numbers with huge exponents. For example:
-- @1e1000000000 :: 'Rational'@ will fill up all space and crash your
-- program. Scientific works as expected:
--
--  > > read "1e1000000000" :: Scientific
--  > 1.0e1000000000
--
-- * Also, the space usage of converting scientific numbers with huge exponents
-- to @'Integral's@ (like: 'Int') or @'RealFloat's@ (like: 'Double' or 'Float')
-- will always be bounded by the target type.
--
-- /WARNING:/ Although @Scientific@ is an instance of 'Fractional', the methods
-- are only partially defined! Specifically 'recip' and '/' will diverge
-- (i.e. loop and consume all space) when their outputs have an infinite decimal
-- expansion. 'fromRational' will diverge when the input 'Rational' has an
-- infinite decimal expansion. Consider using 'fromRationalRepetend' for these
-- rationals which will detect the repetition and indicate where it starts.
--
-- This module is designed to be imported qualified:
--
-- @import Data.Scientific as Scientific@
module Data.Scientific
    ( Scientific

      -- * Construction
    , scientific

      -- * Projections
    , coefficient
    , base10Exponent

      -- * Predicates
    , isFloating
    , isInteger

      -- * Conversions
    , fromRationalRepetend
    , toRationalRepetend
    , floatingOrInteger
    , toRealFloat
    , toBoundedRealFloat
    , toBoundedInteger
    , fromFloatDigits

      -- * Pretty printing
    , formatScientific
    , FPFormat(..)

    , toDecimalDigits

      -- * Normalization
    , normalize
    ) where


----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Data.Scientific.Base as Base
import           Data.Text.Lazy.Builder.RealFloat (FPFormat(..))


----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | An arbitrary-precision number represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.
--
-- This type describes the set of all @'Real's@ which have a finite
-- decimal expansion.
--
-- A scientific number with 'coefficient' @c@ and 'base10Exponent' @e@
-- corresponds to the 'Fractional' number: @'fromInteger' c * 10 '^^' e@
type Scientific = Base.Scientific 10

-- | The coefficient of a scientific number.
--
-- Note that this number is not necessarily normalized, i.e.
-- it could contain trailing zeros.
--
-- Scientific numbers are automatically normalized when pretty printed or
-- in 'toDecimalDigits'.
--
-- Use 'normalize' to do manual normalization.
coefficient :: Scientific -> Integer
coefficient = Base.coefficient
{-# INLINE coefficient #-}

-- | The base-10 exponent of a scientific number.
base10Exponent :: Scientific -> Int
base10Exponent = Base.baseExponent
{-# INLINE base10Exponent #-}

-- | @scientific c e@ constructs a scientific number which corresponds
-- to the 'Fractional' number: @'fromInteger' c * 10 '^^' e@.
scientific :: Integer -> Int -> Scientific
scientific = Base.scientific
{-# INLINE scientific #-}

-- | Like 'fromRational', this function converts a `Rational` to a `Scientific`
-- but instead of diverging (i.e loop and consume all space) on
-- <https://en.wikipedia.org/wiki/Repeating_decimal repeating decimals>
-- it detects the repeating part, the /repetend/, and returns where it starts.
--
-- To detect the repetition this function consumes space linear in the number of
-- digits in the resulting scientific. In order to bound the space usage an
-- optional limit can be specified. If the number of digits reaches this limit
-- @Left (s, r)@ will be returned. Here @s@ is the 'Scientific' constructed so
-- far and @r@ is the remaining 'Rational'. @toRational s + r@ yields the
-- original 'Rational'
--
-- If the limit is not reached or no limit was specified @Right (s,
-- mbRepetendIx)@ will be returned. Here @s@ is the 'Scientific' without any
-- repetition and @mbRepetendIx@ specifies if and where in the fractional part
-- the repetend begins.
--
-- For example:
--
-- @fromRationalRepetend Nothing (1 % 28) == Right (3.571428e-2, Just 2)@
--
-- This represents the repeating decimal: @0.03571428571428571428...@
-- which is sometimes also unambiguously denoted as @0.03(571428)@.
-- Here the repetend is enclosed in parentheses and starts at the 3rd digit (index 2)
-- in the fractional part. Specifying a limit results in the following:
--
-- @fromRationalRepetend (Just 4) (1 % 28) == Left (3.5e-2, 1 % 1400)@
--
-- You can expect the following property to hold.
--
-- @ forall (mbLimit :: Maybe Int) (r :: Rational).
-- r == (case 'fromRationalRepetend' mbLimit r of
--        Left (s, r') -> toRational s + r'
--        Right (s, mbRepetendIx) ->
--          case mbRepetendIx of
--            Nothing         -> toRational s
--            Just repetendIx -> 'toRationalRepetend' s repetendIx)
-- @
fromRationalRepetend
    :: Maybe Int -- ^ Optional limit
    -> Rational
    -> Either (Scientific, Rational)
              (Scientific, Maybe Int)
fromRationalRepetend = Base.fromRationalRepetend
{-# INLINE fromRationalRepetend #-}

-- |
-- Converts a `Scientific` with a /repetend/ (a repeating part in the fraction),
-- which starts at the given index, into its corresponding 'Rational'.
--
-- For example to convert the repeating decimal @0.03(571428)@ you would use:
-- @toRationalRepetend 0.03571428 2 == 1 % 28@
--
-- Preconditions for @toRationalRepetend s r@:
--
-- * @r >= 0@
--
-- * @r < -(base10Exponent s)@
--
-- The formula to convert the @Scientific@ @s@
-- with a repetend starting at index @r@ is described in the paper:
-- <http://fiziko.bureau42.com/teaching_tidbits/turning_repeating_decimals_into_fractions.pdf turning_repeating_decimals_into_fractions.pdf>
-- and is defined as follows:
--
-- @
--   (fromInteger nonRepetend + repetend % nines) /
--   fromInteger (10^^r)
-- where
--   c  = coefficient s
--   e  = base10Exponent s
--
--   -- Size of the fractional part.
--   f = (-e)
--
--   -- Size of the repetend.
--   n = f - r
--
--   m = 10^^n
--
--   (nonRepetend, repetend) = c \`quotRem\` m
--
--   nines = m - 1
-- @
-- Also see: 'fromRationalRepetend'.
toRationalRepetend
    :: Scientific
    -> Int -- ^ Repetend index
    -> Rational
toRationalRepetend = Base.toRationalRepetend
{-# INLINE toRationalRepetend #-}

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

-- | Convert a 'RealFloat' (like a 'Double' or 'Float') into a 'Scientific'
-- number.
--
-- Note that this function uses 'Numeric.floatToDigits' to compute the digits
-- and exponent of the 'RealFloat' number. Be aware that the algorithm used in
-- 'Numeric.floatToDigits' doesn't work as expected for some numbers, e.g. as
-- the 'Double' @1e23@ is converted to @9.9999999999999991611392e22@, and that
-- value is shown as @9.999999999999999e22@ rather than the shorter @1e23@; the
-- algorithm doesn't take the rounding direction for values exactly half-way
-- between two adjacent representable values into account, so if you have a
-- value with a short decimal representation exactly half-way between two
-- adjacent representable values, like @5^23*2^e@ for @e@ close to 23, the
-- algorithm doesn't know in which direction the short decimal representation
-- would be rounded and computes more digits
fromFloatDigits :: (RealFloat a) => a -> Scientific
fromFloatDigits = Base.fromFloatDigits
{-# INLINE fromFloatDigits #-}

-- | Safely convert a 'Scientific' number into a 'RealFloat' (like a 'Double' or a
-- 'Float').
--
-- Note that this function uses 'realToFrac' (@'fromRational' . 'toRational'@)
-- internally but it guards against computing huge Integer magnitudes (@10^e@)
-- that could fill up all space and crash your program. If the 'base10Exponent'
-- of the given 'Scientific' is too big or too small to be represented in the
-- target type, Infinity or 0 will be returned respectively. Use
-- 'toBoundedRealFloat' which explicitly handles this case by returning 'Left'.
--
-- Always prefer 'toRealFloat' over 'realToFrac' when converting from scientific
-- numbers coming from an untrusted source.
toRealFloat :: (RealFloat a) => Scientific -> a
toRealFloat = Base.toRealFloat
{-# INLINE toRealFloat #-}

-- | Preciser version of `toRealFloat`. If the 'base10Exponent' of the given
-- 'Scientific' is too big or too small to be represented in the target type,
-- Infinity or 0 will be returned as 'Left'.
toBoundedRealFloat :: forall a. (RealFloat a) => Scientific -> Either a a
toBoundedRealFloat = Base.toBoundedRealFloat
{-# INLINE toBoundedRealFloat #-}

-- | Convert a `Scientific` to a bounded integer.
--
-- If the given `Scientific` doesn't fit in the target representation, it will
-- return `Nothing`.
--
-- This function also guards against computing huge Integer magnitudes (@10^e@)
-- that could fill up all space and crash your program.
toBoundedInteger :: forall i. (Integral i, Bounded i) => Scientific -> Maybe i
toBoundedInteger = Base.toBoundedInteger
{-# INLINE toBoundedInteger #-}

-- | @floatingOrInteger@ determines if the scientific is floating point
-- or integer. In case it's floating-point the scientific is converted
-- to the desired 'RealFloat' using 'toRealFloat'.
--
-- Also see: 'isFloating' or 'isInteger'.
floatingOrInteger :: (RealFloat r, Integral i) => Scientific -> Either r i
floatingOrInteger = Base.floatingOrInteger
{-# INLINE floatingOrInteger #-}


----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

-- | Return 'True' if the scientific is a floating point, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isFloating :: Scientific -> Bool
isFloating = Base.isFloating
{-# INLINE isFloating #-}

-- | Return 'True' if the scientific is an integer, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isInteger :: Scientific -> Bool
isInteger = Base.isInteger
{-# INLINE isInteger #-}


----------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------

-- | Like 'show' but provides rendering options.
formatScientific :: FPFormat
                 -> Maybe Int  -- ^ Number of decimal places to render.
                 -> Scientific
                 -> String
formatScientific = Base.formatScientific
{-# INLINE formatScientific #-}


----------------------------------------------------------------------

-- | Similar to 'Numeric.floatToDigits', @toDecimalDigits@ takes a
-- positive 'Scientific' number, and returns a list of digits and
-- a base-10 exponent. In particular, if @x>=0@, and
--
-- > toDecimalDigits x = ([d1,d2,...,dn], e)
--
-- then
--
--     1. @n >= 1@
--     2. @x = 0.d1d2...dn * (10^^e)@
--     3. @0 <= di <= 9@
--     4. @null $ takeWhile (==0) $ reverse [d1,d2,...,dn]@
--
-- The last property means that the coefficient will be normalized, i.e. doesn't
-- contain trailing zeros.
toDecimalDigits :: Scientific -> ([Int], Int)
toDecimalDigits = Base.toBaseDigits
{-# INLINE toDecimalDigits #-}


----------------------------------------------------------------------
-- Normalization
----------------------------------------------------------------------

-- | Normalize a scientific number by dividing out powers of 10 from the
-- 'coefficient' and incrementing the 'base10Exponent' each time.
--
-- You should rarely have a need for this function since scientific numbers are
-- automatically normalized when pretty-printed and in 'toDecimalDigits'.
normalize :: Scientific -> Scientific
normalize = Base.normalize
{-# INLINE normalize #-}
