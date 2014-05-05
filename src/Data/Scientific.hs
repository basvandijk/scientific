{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

-- |
-- Module      :  Data.Scientific
-- Copyright   :  Bas van Dijk 2013
-- License     :  BSD3
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is designed to be imported qualified:
--
-- @import Data.Scientific as Scientific@
module Data.Scientific
    ( Scientific

    , scientific

    , coefficient
    , base10Exponent

      -- * Conversions
    , fromFloatDigits

      -- * Pretty printing
    , formatScientific
    , FPFormat(..)

    , toDecimalDigits
    ) where


----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad                (mplus)
import           Control.DeepSeq              (NFData)
import           Data.Array                   (Array, listArray, (!))
import           Data.Char                    (intToDigit, ord)
import           Data.Data                    (Data)
import           Data.Function                (on)
import           Data.Functor                 ((<$>))
import           Data.Hashable                (Hashable(..))
import           Data.Ratio                   ((%), numerator, denominator)
import           Data.Typeable                (Typeable)
import           Math.NumberTheory.Logarithms (integerLog10')
import           Numeric                      (floatToDigits)
import           Text.Read                    (readPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP     ( ReadP )
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
data Scientific = Scientific
    { coefficient    ::                !Integer
      -- ^ The coefficient of a scientific number.

    , base10Exponent :: {-# UNPACK #-} !Int
      -- ^ The base-10 exponent of a scientific number.
    } deriving (Typeable, Data)

-- | @scientific c e@ constructs a scientific number with
-- 'coefficient' @c@ and 'base10Exponent' @e@.
scientific :: Integer -> Int -> Scientific
scientific = Scientific
{-# INLINE scientific #-}


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance NFData Scientific

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

instance Num Scientific where
    Scientific c1 e1 + Scientific c2 e2
       | e1 < e2   = scientific (c1   + c2*l) e1
       | otherwise = scientific (c1*r + c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (+) #-}

    Scientific c1 e1 - Scientific c2 e2
       | e1 < e2   = scientific (c1   - c2*l) e1
       | otherwise = scientific (c1*r - c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (-) #-}

    Scientific c1 e1 * Scientific c2 e2 =
        scientific (c1 * c2) (e1 + e2)
    {-# INLINE (*) #-}

    abs (Scientific c e) = scientific (abs c) e
    {-# INLINE abs #-}

    negate (Scientific c e) = scientific (negate c) e
    {-# INLINE negate #-}

    signum (Scientific c _) = scientific (signum c) 0
    {-# INLINE signum #-}

    fromInteger i = scientific i 0
    {-# INLINE fromInteger #-}

instance Real Scientific where
    toRational (Scientific c e)
      | e < 0     =  c % magnitude (-e)
      | otherwise = (c * magnitude   e) % 1
    {-# INLINE toRational #-}

-- | /WARNING:/ 'recip' and '/' will diverge when their outputs have
-- an infinite decimal expansion. 'fromRational' will diverge when the
-- input 'Rational' has an infinite decimal expansion.
instance Fractional Scientific where
    recip = fromRational . recip . toRational
    {-# INLINE recip #-}

    fromRational rational = positivize (longDiv 0 0) (numerator rational)
      where
        -- Divide the numerator by the denominator using long division.
        longDiv :: Integer -> Int -> (Integer -> Scientific)
        longDiv !c !e  0 = scientific c e
        longDiv !c !e !n
                          -- TODO: Use a logarithm here!
            | n < d     = longDiv (c * 10) (e - 1) (n * 10)
            | otherwise = longDiv (c + q)   e      r
                            where
                              (q, r) = n `quotRem` d

        d = denominator rational

instance RealFrac Scientific where
    -- | The function 'properFraction' takes a Scientific number @s@
    -- and returns a pair @(n,f)@ such that @s = n+f@, and:
    --
    -- * @n@ is an integral number with the same sign as @s@; and
    --
    -- * @f@ is a fraction with the same type and sign as @s@,
    --   and with absolute value less than @1@.
    properFraction s@(Scientific c e)
        | e < 0     = if dangerouslySmall c e
                      then (0, s)
                      else let (q, r) = c `quotRem` magnitude (-e)
                           in (fromInteger q, scientific r e)
        | otherwise = (fromInteger c * magnitude e, 0)
    {-# INLINE properFraction #-}

    -- | @'truncate' s@ returns the integer nearest @s@
    -- between zero and @s@
    truncate = whenFloating $ \c e ->
                 if dangerouslySmall c e
                 then 0
                 else fromInteger $ c `quot` magnitude (-e)
    {-# INLINE truncate #-}

    -- | @'round' s@ returns the nearest integer to @s@;
    --   the even integer if @s@ is equidistant between two integers
    round = whenFloating $ \c e ->
              if dangerouslySmall c e
              then 0
              else let (q, r) = c `quotRem` magnitude (-e)
                       n = fromInteger q
                       m = if r < 0 then n - 1 else n + 1
                       f = scientific r e
                   in case signum $ coefficient $ abs f - 0.5 of
                        -1 -> n
                        0  -> if even n then n else m
                        1  -> m
                        _  -> error "round default defn: Bad value"
    {-# INLINE round #-}

    -- | @'ceiling' s@ returns the least integer not less than @s@
    ceiling = whenFloating $ \c e ->
                if dangerouslySmall c e
                then if c < 0
                     then 0
                     else 1
                else let (q, r) = c `quotRem` magnitude (-e)
                     in fromInteger $! if r > 0 then q + 1 else q
    {-# INLINE ceiling #-}

    -- | @'floor' s@ returns the greatest integer not greater than @s@
    floor = whenFloating $ \c e ->
              if dangerouslySmall c e
              then 0
              else fromInteger (c `div` magnitude (-e))
    {-# INLINE floor #-}


----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

maxExpt :: Int
maxExpt = 1100

expts10 :: Array Int Integer
expts10 = listArray (0, maxExpt) $ iterate (*10) 1

-- | @magnitude e == 10 ^ e@
magnitude :: (Num a) => Int -> a
magnitude e | e <= maxExpt = cachedPow10 e
            | otherwise    = cachedPow10 maxExpt * 10 ^ (e - maxExpt)
    where
      cachedPow10 p = fromInteger (expts10 ! p)
{-# INLINE magnitude #-}


----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

-- | A scientific value with a big negative exponent (e < (-limit)) is
-- considered dangerously small because if you evaluate its Integer
-- magnitude (10 ^ (-e)) it could take up a lot of space and
-- potentially crash your program.
--
-- However when the number of decimal digits in the coefficient is
-- larger than -e an attacker ...
dangerouslySmall :: Integer -> Int -> Bool
dangerouslySmall c e = e < (-limit) && e < (-integerLog10' (abs c)) - 1
    where
      limit :: Int
      limit = 20
{-# INLINE dangerouslySmall #-}

positivize :: (Ord a, Num a, Num b) => (a -> b) -> (a -> b)
positivize f x | x < 0      = -(f (-x))
               | otherwise =    f   x
{-# INLINE positivize #-}

whenFloating :: (Num a) => (Integer -> Int -> a) -> Scientific -> a
whenFloating f (Scientific c e)
    | e < 0     = f c e
    | otherwise = fromInteger c * magnitude e
{-# INLINE whenFloating #-}


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

-- | Exact conversion from a 'RealFloat' into a 'Scientific' number.
fromFloatDigits :: (RealFloat a) => a -> Scientific
fromFloatDigits rf
      -- integers are way more efficient to convert via Rational.
      -- We do pay the cost of always converting to Rational first though.
    | denominator rat == 1 = fromInteger $ numerator rat
    | otherwise = positivize fromNonNegRealFloat rf
    where
      rat = toRational rf

      fromNonNegRealFloat r = go digits 0 0
        where
          (digits, e) = floatToDigits 10 r

          go []     !c !n = scientific c (e - n)
          go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)


----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

instance Read Scientific where
    readPrec = ReadPrec.lift scientificP

scientificP :: ReadP Scientific
scientificP = do
  let positive = (('+' ==) <$> ReadP.satisfy isSign) `mplus` return True
  pos <- positive

  let step :: Num a => a -> Int -> a
      step a digit = a * 10 + fromIntegral digit

  n <- foldDigits step 0

  let s = Scientific n 0
      fractional = foldDigits (\(Scientific a e) digit ->
                                   scientific (step a digit) (e-1)) s

  Scientific coeff expnt <- (ReadP.satisfy (== '.') >> fractional)
                              `mplus` return s

  let signedCoeff | pos       =   coeff
                  | otherwise = (-coeff)

      eP = do posE <- positive
              e <- foldDigits step 0
              if posE
                then return   e
                else return (-e)

  (ReadP.satisfy isE >>
           ((scientific signedCoeff . (expnt +)) <$> eP)) `mplus`
     return (scientific signedCoeff    expnt)

foldDigits :: (a -> Int -> a) -> a -> ReadP a
foldDigits f z = ReadP.look >>= go z
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

instance Show Scientific where
    show = formatScientific Generic Nothing

-- | Like 'show' but provides rendering options.
formatScientific :: FPFormat
                 -> Maybe Int  -- ^ Number of decimal places to render.
                 -> Scientific
                 -> String
formatScientific fmt decs scntfc@(Scientific c _)
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

-- | Similar to 'floatToDigits', @toDecimalDigits@ takes a
-- non-negative 'Scientific' number, and returns a list of digits and
-- a base-10 exponent. In particular, if @x>=0@, and
--
-- > toDecimalDigits x = ([d1,d2,...,dn], e)
--
-- then
--
--      (1) @n >= 1@
--
--      (2) @x = 0.d1d2...dn * (10^^e)@
--
--      (3) @0 <= di <= 9@
toDecimalDigits :: Scientific -> ([Int], Int)
toDecimalDigits (Scientific 0 _) = ([0], 0)
toDecimalDigits (Scientific c e) = (is, n + e)
  where
    (is, n) = reverseAndLength $ digits c

    digits :: Integer -> [Int]
    digits 0 = []
    digits i = fromIntegral r : digits q
      where
        (q, r) = i `quotRem` 10

    reverseAndLength :: [a] -> ([a], Int)
    reverseAndLength l = rev l [] 0
      where
        rev []     a !m = (a, m)
        rev (x:xs) a !m = rev xs (x:a) (m+1)
