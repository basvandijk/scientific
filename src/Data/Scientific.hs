{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

#ifdef GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

-- |
-- Module      :  Data.Scientific
-- Copyright   :  Bas van Dijk 2013
-- License     :  BSD3
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- @Data.Scientific@ provides a space efficient and arbitrary precision
-- scientific number type.
--
-- 'Scientific' numbers are represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>. It
-- uses an 'Integer' 'coefficient' @c@ and an 'Int' 'base10Exponent' @e@ (do
-- note that since we're using an 'Int' to represent the exponent these numbers
-- aren't truly arbitrary precision). A scientific number corresponds to the
-- 'Fractional' number: @'fromInteger' c * 10 '^^' e@.
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
-- infinite decimal expansion.
--
-- This module is designed to be imported qualified:
--
-- @import Data.Scientific as Scientific@
module Data.Scientific
    ( Scientific

      -- * Construction
    , scientific
    , scientificWithDisplayMode

      -- * Projections
    , coefficient
    , base10Exponent

      -- * Predicates
    , isFloating
    , isInteger

      -- * Conversions
    , floatingOrInteger
    , toRealFloat
    , toBoundedRealFloat
    , toBoundedInteger
    , fromFloatDigits

      -- * Pretty printing
    , DisplayMode(..)
    , displayMode
    , setDisplayMode

    , formatScientific
    , FPFormat(..)

    , toDecimalDigits
    , toDecimalDigits'

      -- * Normalization
    , normalize
    ) where


----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Exception            (throw, ArithException(DivideByZero))
import           Control.Monad                (mplus)
import           Control.Monad.ST             (runST)
import           Control.DeepSeq              (NFData(rnf))
import           Data.Char                    (intToDigit, ord)
import           Data.Data                    (Data)
import           Data.Function                (on)
import           Data.Functor                 ((<$>))
import           Data.Hashable                (Hashable(..))
import           Data.Ratio                   ((%), numerator, denominator)
import           Data.Typeable                (Typeable)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           Math.NumberTheory.Logarithms (integerLog10')
import qualified Numeric                      (floatToDigits)
import qualified Text.Read                       as Read
import           Text.Read                        (readPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP     ( ReadP )
import           Data.Text.Lazy.Builder.RealFloat (FPFormat(..))

#if MIN_VERSION_base(4,5,0)
import           Data.Bits                    (unsafeShiftR)
#else
import           Data.Bits                    (shiftR)
#endif

#ifdef GENERICS
import           GHC.Generics ( Generic )
#endif

import GHC.Integer (quotRemInteger, divInteger, quotInteger)


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
data Scientific = Scientific
    { displayMode :: !DisplayMode
      -- ^ Get the display mode of a scientific number.

    , coefficient :: !Integer
      -- ^ The coefficient of a scientific number.
      --
      -- Note that this number is not necessarily normalized, i.e.
      -- it could contain trailing zeros.
      --
      -- Scientific numbers are automatically normalized when pretty printed or
      -- in 'toDecimalDigits'.
      --
      -- Use 'normalize' to do manual normalization.

    , base10Exponent :: {-# UNPACK #-} !Int
      -- ^ The base-10 exponent of a scientific number.
    } deriving (Typeable, Data)

-- | Specifies how a 'Scientific' number should be rendered.
--
-- By default a 'Scientific' is constructed with the 'DisplayGeneric' mode. The
-- following are exceptions to this rule:
--
-- * A scientific can be constructed with a specific display mode using
--   'scientificWithDisplayMode'.
--
-- * The display mode of an exisiting scientific can be set using
-- 'setDisplayMode'.
--
-- * The 'fromInteger` method will automatically set the mode to
--   'DisplayInteger'.
--
-- * When scientific numbers are combined (for example in '+') the resulting
--   scienfific number will have the highest display mode of the two input
--   scientifics (@'max' d1 d2@) according to the derived 'Ord' instance of
--   'DisplayMode'.
data DisplayMode =
    DisplayInteger  -- ^ Always display as an integer so no decimal point,
                    --   this will truncate a fractional as if it was converted to an integer.
  | DisplayFixed    -- ^ Displays as fixed point, includes decimal point.
  | DisplayGeneric  -- ^ Mix mode, this is the default for show.
                    --   Use decimal notation for values between 0.1 and 9,999,999,
                    --   and scientific notation otherwise.
  | DisplayExponent -- ^ Always display in scientific notation (e.g. 2.3e123),
                    --   equivalent to @printf %e@.
  deriving ( Eq, Ord, Bounded, Enum, Show, Read, Typeable, Data
#ifdef GENERICS
           , Generic
#endif
           )

-- | @scientific c e@ constructs a scientific number which corresponds
-- to the 'Fractional' number: @'fromInteger' c * 10 '^^' e@.
scientific :: Integer -> Int -> Scientific
scientific = Scientific DisplayGeneric

-- | Like 'scientific' but also allows setting the 'DisplayMode'.
scientificWithDisplayMode :: DisplayMode -> Integer -> Int -> Scientific
scientificWithDisplayMode = Scientific

-- | Set the display mode of a scientific number.
setDisplayMode :: DisplayMode -> Scientific -> Scientific
setDisplayMode d s = s {displayMode=d}


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance NFData Scientific where
    rnf (Scientific _ _ _) = ()

instance Hashable Scientific where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Eq Scientific where
    (==) = (==) `on` toRational
    {-# INLINE (==) #-}

    (/=) = (/=) `on` toRational
    {-# INLINE (/=) #-}

instance Ord Scientific where
    (<) = (<) `on` toRational
    {-# INLINE (<) #-}

    (<=) = (<=) `on` toRational
    {-# INLINE (<=) #-}

    (>) = (>) `on` toRational
    {-# INLINE (>) #-}

    (>=) = (>=) `on` toRational
    {-# INLINE (>=) #-}

    compare = compare `on` toRational
    {-# INLINE compare #-}

-- | Note that the 'fromInteger` method will automatically
-- set the 'DisplayMode' to 'DisplayInteger'.
instance Num Scientific where
    Scientific d1 c1 e1 + Scientific d2 c2 e2
       | e1 < e2   = Scientific (max d1 d2) (c1   + c2*l) e1
       | otherwise = Scientific (max d1 d2) (c1*r + c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (+) #-}

    Scientific d1 c1 e1 - Scientific d2 c2 e2
       | e1 < e2   = Scientific (max d1 d2) (c1   - c2*l) e1
       | otherwise = Scientific (max d1 d2) (c1*r - c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (-) #-}

    Scientific d1 c1 e1 * Scientific d2 c2 e2 =
        Scientific (max d1 d2) (c1 * c2) (e1 + e2)
    {-# INLINE (*) #-}

    abs (Scientific d c e) = Scientific d (abs c) e
    {-# INLINE abs #-}

    negate (Scientific d c e) = Scientific d (negate c) e
    {-# INLINE negate #-}

    signum (Scientific d c _) = Scientific d (signum c) 0
    {-# INLINE signum #-}

    fromInteger i = Scientific DisplayInteger i 0
    {-# INLINE fromInteger #-}

-- | /WARNING:/ 'toRational' needs to compute the 'Integer' magnitude:
-- @10^e@. If applied to a huge exponent this could fill up all space
-- and crash your program!
--
-- Avoid applying 'toRational' (or 'realToFrac') to scientific numbers
-- coming from an untrusted source and use 'toRealFloat' instead. The
-- latter guards against excessive space usage.
instance Real Scientific where
    toRational (Scientific _ c e)
      | e < 0     =  c % magnitude (-e)
      | otherwise = (c * magnitude   e) % 1
    {-# INLINE toRational #-}

{-# RULES
  "realToFrac_toRealFloat_Double"
   realToFrac = toRealFloat :: Scientific -> Double #-}

{-# RULES
  "realToFrac_toRealFloat_Float"
   realToFrac = toRealFloat :: Scientific -> Float #-}

-- | /WARNING:/ 'recip' and '/' will diverge (i.e. loop and consume all space)
-- when their outputs have an infinite decimal expansion. 'fromRational' will
-- diverge when the input 'Rational' has an infinite decimal expansion.
instance Fractional Scientific where
    recip = fromRational . recip . toRational
    {-# INLINE recip #-}

    x / y = fromRational $ toRational x / toRational y
    {-# INLINE (/) #-}

    fromRational rational
        | d == 0    = throw DivideByZero
        | otherwise = positivize (longDiv 0 0) (numerator rational)
      where
        -- Divide the numerator by the denominator using long division.
        longDiv :: Integer -> Int -> (Integer -> Scientific)
        longDiv !c !e  0 = Scientific DisplayGeneric c e
        longDiv !c !e !n
                          -- TODO: Use a logarithm here!
            | n < d     = longDiv (c * 10) (e - 1) (n * 10)
            | otherwise = case n `quotRemInteger` d of
                            (#q, r#) -> longDiv (c + q) e r

        d = denominator rational

instance RealFrac Scientific where
    -- | The function 'properFraction' takes a Scientific number @s@
    -- and returns a pair @(n,f)@ such that @s = n+f@, and:
    --
    -- * @n@ is an integral number with the same sign as @s@; and
    --
    -- * @f@ is a fraction with the same type and sign as @s@,
    --   and with absolute value less than @1@.
    properFraction s@(Scientific d c e)
        | e < 0     = if dangerouslySmall c e
                      then (0, s)
                      else case c `quotRemInteger` magnitude (-e) of
                             (#q, r#) -> (fromInteger q, Scientific d r e)
        | otherwise = (toIntegral s, 0)
    {-# INLINE properFraction #-}

    -- | @'truncate' s@ returns the integer nearest @s@
    -- between zero and @s@
    truncate = whenFloating $ \c e ->
                 if dangerouslySmall c e
                 then 0
                 else fromInteger $ c `quotInteger` magnitude (-e)
    {-# INLINE truncate #-}

    -- | @'round' s@ returns the nearest integer to @s@;
    --   the even integer if @s@ is equidistant between two integers
    round = whenFloating $ \c e ->
              if dangerouslySmall c e
              then 0
              else let (#q, r#) = c `quotRemInteger` magnitude (-e)
                       n = fromInteger q
                       m | r < 0     = n - 1
                         | otherwise = n + 1
                       f = Scientific DisplayGeneric r e
                   in case signum $ coefficient $ abs f - 0.5 of
                        -1 -> n
                        0  -> if even n then n else m
                        1  -> m
                        _  -> error "round default defn: Bad value"
    {-# INLINE round #-}

    -- | @'ceiling' s@ returns the least integer not less than @s@
    ceiling = whenFloating $ \c e ->
                if dangerouslySmall c e
                then if c <= 0
                     then 0
                     else 1
                else case c `quotRemInteger` magnitude (-e) of
                       (#q, r#) | r <= 0    -> fromInteger q
                                | otherwise -> fromInteger (q + 1)
    {-# INLINE ceiling #-}

    -- | @'floor' s@ returns the greatest integer not greater than @s@
    floor = whenFloating $ \c e ->
              if dangerouslySmall c e
              then if c < 0
                   then -1
                   else 0
              else fromInteger (c `divInteger` magnitude (-e))
    {-# INLINE floor #-}


----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

-- | This function is used in the 'RealFrac' methods to guard against
-- computing a huge magnitude (-e) which could take up all space.
--
-- Think about parsing a scientific number from an untrusted
-- string. An attacker could supply 1e-1000000000. Lets say we want to
-- 'floor' that number to an 'Int'. When we naively try to floor it
-- using:
--
-- @
-- floor = whenFloating $ \c e ->
--           fromInteger (c `div` magnitude (-e))
-- @
--
-- We will compute the huge Integer: @magnitude 1000000000@. This
-- computation will quickly fill up all space and crash the program.
--
-- Note that for large /positive/ exponents there is no risk of a
-- space-leak since 'whenFloating' will compute:
--
-- @fromInteger c * magnitude e :: a@
--
-- where @a@ is the target type (Int in this example). So here the
-- space usage is bounded by the target type.
--
-- For large negative exponents we check if the exponent is smaller
-- than some limit (currently -324). In that case we know that the
-- scientific number is really small (unless the coefficient has many
-- digits) so we can immediately return -1 for negative scientific
-- numbers or 0 for positive numbers.
--
-- More precisely if @dangerouslySmall c e@ returns 'True' the
-- scientific number @s@ is guaranteed to be between:
-- @-0.1 > s < 0.1@.
--
-- Note that we avoid computing the number of decimal digits in c
-- (log10 c) if the exponent is not below the limit.
dangerouslySmall :: Integer -> Int -> Bool
dangerouslySmall c e = e < (-limit) && e < (-integerLog10' (abs c)) - 1
{-# INLINE dangerouslySmall #-}

limit :: Int
limit = maxExpt

positivize :: (Ord a, Num a, Num b) => (a -> b) -> (a -> b)
positivize f x | x < 0      = -(f (-x))
               | otherwise =    f   x
{-# INLINE positivize #-}

whenFloating :: (Num a) => (Integer -> Int -> a) -> Scientific -> a
whenFloating f s@(Scientific _ c e)
    | e < 0     = f c e
    | otherwise = toIntegral s
{-# INLINE whenFloating #-}

-- | Precondition: the 'Scientific' @s@ needs to be an integer:
-- @base10Exponent (normalize s) >= 0@
toIntegral :: (Num a) => Scientific -> a
toIntegral (Scientific _ c e) = fromInteger c * magnitude e
{-# INLINE toIntegral #-}


----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: V.Vector Integer
expts10 = runST $ do
    mv <- VM.unsafeNew maxExpt
    VM.unsafeWrite mv 0  1
    VM.unsafeWrite mv 1 10
    let go !ix
          | ix == maxExpt = V.unsafeFreeze mv
          | otherwise = do
              VM.unsafeWrite mv  ix        xx
              VM.unsafeWrite mv (ix+1) (10*xx)
              go (ix+2)
          where
            xx = x * x
            x  = V.unsafeIndex expts10 half
#if MIN_VERSION_base(4,5,0)
            half = ix `unsafeShiftR` 1
#else
            half = ix `shiftR` 1
#endif
    go 2

-- | @magnitude e == 10 ^ e@
magnitude :: (Num a) => Int -> a
magnitude e | e < maxExpt = cachedPow10 e
            | otherwise   = cachedPow10 hi * 10 ^ (e - hi)
    where
      cachedPow10 p = fromInteger (V.unsafeIndex expts10 p)

      hi = maxExpt - 1
{-# INLINE magnitude #-}


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
fromFloatDigits = positivize fromPositiveRealFloat
    where
      fromPositiveRealFloat r = go digits 0 0
        where
          (digits, e) = Numeric.floatToDigits 10 r

          go []     !c !n = Scientific DisplayGeneric c (e - n)
          go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)

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
toRealFloat = either id id . toBoundedRealFloat

-- | Preciser version of `toRealFloat`. If the 'base10Exponent' of the given
-- 'Scientific' is too big or too small to be represented in the target type,
-- Infinity or 0 will be returned as 'Left'.
toBoundedRealFloat :: forall a. (RealFloat a) => Scientific -> Either a a
toBoundedRealFloat s@(Scientific _ c e)
    | c == 0                                       = Right 0
    | e >  limit && e > hiLimit                    = Left  $ sign (1/0) -- Infinity
    | e < -limit && e < loLimit && e + d < loLimit = Left  $ sign 0
    | otherwise                                    = Right $ realToFrac s
  where
    (loLimit, hiLimit) = exponentLimits (undefined :: a)

    d = integerLog10' (abs c)

    sign x | c < 0     = -x
           | otherwise =  x

exponentLimits :: forall a. (RealFloat a) => a -> (Int, Int)
exponentLimits _ = (loLimit, hiLimit)
    where
      loLimit = floor   (fromIntegral lo     * log10Radix) -
                ceiling (fromIntegral digits * log10Radix)
      hiLimit = ceiling (fromIntegral hi     * log10Radix)

      log10Radix :: Double
      log10Radix = logBase 10 $ fromInteger radix

      radix    = floatRadix  (undefined :: a)
      digits   = floatDigits (undefined :: a)
      (lo, hi) = floatRange  (undefined :: a)

-- | Convert a `Scientific` to a bounded integer.
--
-- If the given `Scientific` doesn't fit in the target representation, it will
-- return `Nothing`.
--
-- This function also guards against computing huge Integer magnitudes (@10^e@)
-- that could fill up all space and crash your program.
toBoundedInteger :: forall i. (Integral i, Bounded i) => Scientific -> Maybe i
toBoundedInteger s
    | c == 0    = fromIntegerBounded 0
    | integral  = if dangerouslyBig
                  then Nothing
                  else fromIntegerBounded n
    | otherwise = Nothing
  where
    c = coefficient s

    integral = e >= 0 || e' >= 0

    e  = base10Exponent s
    e' = base10Exponent s'

    s' = normalize s

    dangerouslyBig = e > limit &&
                     e > integerLog10' (max (abs iMinBound) (abs iMaxBound))

    fromIntegerBounded :: Integer -> Maybe i
    fromIntegerBounded i
        | i < iMinBound || i > iMaxBound = Nothing
        | otherwise                      = Just $ fromInteger i

    iMinBound = toInteger (minBound :: i)
    iMaxBound = toInteger (maxBound :: i)

    -- This should not be evaluated if the given Scientific is dangerouslyBig
    -- since it could consume all space and crash the process:
    n :: Integer
    n = toIntegral s'

-- | @floatingOrInteger@ determines if the scientific is floating point
-- or integer. In case it's floating-point the scientific is converted
-- to the desired 'RealFloat' using 'toRealFloat'.
--
-- Also see: 'isFloating' or 'isInteger'.
floatingOrInteger :: (RealFloat r, Integral i) => Scientific -> Either r i
floatingOrInteger s
    | base10Exponent s  >= 0 = Right (toIntegral   s)
    | base10Exponent s' >= 0 = Right (toIntegral   s')
    | otherwise              = Left  (toRealFloat  s')
  where
    s' = normalize s


----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

-- | Return 'True' if the scientific is a floating point, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isFloating :: Scientific -> Bool
isFloating = not . isInteger

-- | Return 'True' if the scientific is an integer, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isInteger :: Scientific -> Bool
isInteger s = base10Exponent s  >= 0 ||
              base10Exponent s' >= 0
  where
    s' = normalize s


----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

instance Read Scientific where
    readPrec = Read.parens $ ReadPrec.lift (ReadP.skipSpaces >> scientificP)

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

scientificP :: ReadP Scientific
scientificP = do
  let positive = (('+' ==) <$> ReadP.satisfy isSign) `mplus` return True
  pos <- positive

  let step :: Num a => a -> Int -> a
      step a digit = a * 10 + fromIntegral digit
      {-# INLINE step #-}

  n <- foldDigits step 0

  let s = SP n 0
      fractional = foldDigits (\(SP a e) digit ->
                                 SP (step a digit) (e-1)) s

  SP coeff expnt <- (ReadP.satisfy (== '.') >> fractional)
                    ReadP.<++ return s

  let signedCoeff | pos       =   coeff
                  | otherwise = (-coeff)

      eP = do posE <- positive
              e <- foldDigits step 0
              if posE
                then return   e
                else return (-e)

      d = DisplayGeneric

  (ReadP.satisfy isE >>
           ((Scientific d signedCoeff . (expnt +)) <$> eP)) `mplus`
     return (Scientific d signedCoeff    expnt)


foldDigits :: (a -> Int -> a) -> a -> ReadP a
foldDigits f z = do
    c <- ReadP.satisfy isDecimal
    let digit = ord c - 48
        a = f z digit

    ReadP.look >>= go a
  where
    go !a [] = return a
    go !a (c:cs)
        | isDecimal c = do
            _ <- ReadP.get
            let digit = ord c - 48
            go (f a digit) cs
        | otherwise = return a

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

isSign :: Char -> Bool
isSign c = c == '-' || c == '+'
{-# INLINE isSign #-}

isE :: Char -> Bool
isE c = c == 'e' || c == 'E'
{-# INLINE isE #-}


----------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------

-- | Note that the rendering of a scientific number can be controlled
-- using the specified 'DisplayMode'.
instance Show Scientific where
    show s = case displayMode s of
               DisplayInteger  -> formatAsInteger s
               DisplayFixed    -> formatScientific Fixed    Nothing s
               DisplayGeneric  -> formatScientific Generic  Nothing s
               DisplayExponent -> formatScientific Exponent Nothing s

formatAsInteger :: Scientific -> String
formatAsInteger scntfc@(Scientific _ c _)
   | c < 0     = '-':doFmt (toDecimalDigits' (-scntfc))
   | otherwise =     doFmt (toDecimalDigits'   scntfc )
  where
    doFmt :: ([Int], Int, Int) -> String
    doFmt (is, n, se)
        | e <= 0    = "0"
        | otherwise = map intToDigit (take e is) ++
                      replicate se '0'
      where
        e = n + se

-- | Like 'show' but provides rendering options.
formatScientific :: FPFormat
                 -> Maybe Int  -- ^ Number of decimal places to render.
                 -> Scientific
                 -> String
formatScientific fmt decs scntfc@(Scientific _ c _)
   | c < 0     = '-':doFmt fmt (toDecimalDigits (-scntfc))
   | otherwise =     doFmt fmt (toDecimalDigits   scntfc )
  where
    doFmt :: FPFormat -> ([Int], Int) -> String
    doFmt format (is, e) =
      let ds = map intToDigit is in
      case format of
       Generic ->
        doFmt (if e < 0 || e > 7 then Exponent else Fixed)
              (is, e)
       Exponent ->
        case decs of
         Nothing ->
          let show_e' = show (e-1) in
          case ds of
            "0"     -> "0.0e0"
            [d]     -> d : ".0e" ++ show_e'
            (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
            []      -> error "formatScientific/doFmt/FFExponent: []"
         Just dec ->
          let dec' = max dec 1 in
          case is of
           [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
           _ ->
            let
             (ei,is') = roundTo (dec'+1) is
             (d:ds') = map intToDigit (if ei > 0 then init is' else is')
            in
            d:'.':ds' ++ 'e':show (e-1+ei)
       Fixed ->
        let
         mk0 ls = case ls of { "" -> "0" ; _ -> ls}
        in
        case decs of
         Nothing
            | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
            | otherwise ->
               let
                  f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                  f n s    ""  = f (n-1) ('0':s) ""
                  f n s (r:rs) = f (n-1) (r:s) rs
               in
                  f e "" ds
         Just dec ->
          let dec' = max dec 0 in
          if e >= 0 then
           let
            (ei,is') = roundTo (dec' + e) is
            (ls,rs)  = splitAt (e+ei) (map intToDigit is')
           in
           mk0 ls ++ (if null rs then "" else '.':rs)
          else
           let
            (ei,is') = roundTo dec' (replicate (-e) 0 ++ is)
            d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
           in
           d : (if null ds' then "" else '.':ds')

----------------------------------------------------------------------

roundTo :: Int -> [Int] -> (Int,[Int])
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  base = 10

  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

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
toDecimalDigits s = case toDecimalDigits' s of (is, n, e) -> (is, n + e)

-- | @toDecimalDigits'@ relates to 'toDecimalDigits' as follows:
--
-- @
-- toDecimalDigits s = (is, n + e)
--   where
--     (is, n, e) = toDecimalDigits' s
-- @
--
-- * @n@ is the length of the list of digits @is@.
--
-- * @e@ is the @'normalize'd@ 'base10Exponent' of @s@.
toDecimalDigits' :: Scientific -> ([Int], Int, Int)
toDecimalDigits' (Scientific _ 0  _)  = ([0], 1, 0)
toDecimalDigits' (Scientific _ c' e') =
    case normalizePositive c' e' of
      (c, e) -> case reverseAndLength $ digits c of
                  (is, n) -> (is, n, e)
  where
    digits :: Integer -> [Int]
    digits 0 = []
    digits i = case i `quotRemInteger` 10 of
                 (#q, r#) -> fromIntegral r : digits q

    reverseAndLength :: [a] -> ([a], Int)
    reverseAndLength l = rev l [] 0
      where
        rev []     a !m = (a, m)
        rev (x:xs) a !m = rev xs (x:a) (m+1)


----------------------------------------------------------------------
-- Normalization
----------------------------------------------------------------------

-- | Normalize a scientific number by dividing out powers of 10 from the
-- 'coefficient' and incrementing the 'base10Exponent' each time.
--
-- The 'DisplayMode' will remain unaffected.
--
-- You should rarely have a need for this function since scientific numbers are
-- automatically normalized when pretty-printed and in 'toDecimalDigits'.
normalize :: Scientific -> Scientific
normalize (Scientific d c e)
    | c > 0 = case normalizePositive   c  e of (c', e') -> Scientific d   c'  e'
    | c < 0 = case normalizePositive (-c) e of (c', e') -> Scientific d (-c') e'
    | otherwise {- c == 0 -} = Scientific d 0 0

normalizePositive :: Integer -> Int -> (Integer, Int)
normalizePositive c !e = case quotRemInteger c 10 of
                           (#c', 0#) -> normalizePositive c' (e+1)
                           _         -> (c, e)
