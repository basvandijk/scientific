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
-- /WARNING:/ Although @Scientific@ has instances for all numeric classes the
-- methods should be used with caution when applied to scientific numbers coming
-- from untrusted sources. See the warnings of the instances belonging to
-- 'Scientific'.
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
      -- ** Rational
    , unsafeFromRational
    , fromRationalRepetend
    , fromRationalRepetendLimited
    , fromRationalRepetendUnlimited
    , toRationalRepetend

      -- ** Floating & integer
    , floatingOrInteger
    , toRealFloat
    , toBoundedRealFloat
    , toBoundedInteger
    , toUnboundedInteger
    , fromFloatDigits

      -- * Parsing
    , scientificP

      -- * Pretty printing
    , formatScientific
    , FPFormat(..)

    , toDecimalDigits

      -- * Normalization
    , normalize
    ) where

import Data.Scientific.Internal
