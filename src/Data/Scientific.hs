{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

-- TODO: The following extensions are needed for scientificBuilder:
{-# LANGUAGE CPP, MagicHash, OverloadedStrings #-}

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
    , toFractional
    , fromRealFloat

      -- * Pretty printing
    , FPFormat(..)

    , scientificBuilder
    , formatScientificBuilder
    , formatScientific

    , toDecimalDigits
    ) where

----------------------------------------------------------------------

import           Control.Monad       (mplus)
import           Control.DeepSeq     (NFData)
import           Data.Char           (intToDigit, ord)
import           Data.Function       (on)
import           Data.Functor        ((<$>))
import           Data.Hashable       (Hashable(..))
import           Data.Ratio          ((%), numerator, denominator)
import           Data.Typeable       (Typeable)
import           Foreign.C.Types     (CDouble, CFloat)
import           Numeric             (floatToDigits)
import           Text.Read           (readPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP     ( ReadP )

-- TODO: The following imports are needed for the scientificBuilder:
import Data.Text.Lazy.Builder       (Builder, fromString, singleton, fromText)
import Data.Text.Lazy.Builder.Int   (decimal)
import qualified Data.Text as T     (replicate)
import GHC.Base                     (Int(I#), Char(C#), chr#, ord#, (+#))
#if MIN_VERSION_base(4,5,0)
import Data.Monoid                  ((<>))
#else
import Data.Monoid                  (Monoid, mappend)
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
infixr 6 <>
#endif

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
    { coefficient    ::                !Integer -- ^ The coefficient of a scientific number.
    , base10Exponent :: {-# UNPACK #-} !Int     -- ^ The base-10 exponent of a scientific number.
    } deriving (Typeable)

-- | @scientific c e@ constructs a scientific number with
-- 'coefficient' @c@ and 'base10Exponent' @e@.
scientific :: Integer -> Int -> Scientific
scientific = Scientific
{-# INLINE scientific #-}

----------------------------------------------------------------------

instance NFData Scientific

instance Hashable Scientific where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Show Scientific where
    show = formatScientific Generic Nothing

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
      fractional = foldDigits (\(Scientific a e) digit -> scientific (step a digit) (e-1)) s

  Scientific coeff expnt <- (ReadP.satisfy (== '.') >> fractional) `mplus` return s

  let signedCoeff | pos       = coeff
                  | otherwise = negate coeff

      eP = do posE <- positive
              e <- foldDigits step 0
              if posE
                then return e
                else return $ negate e

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
           l = 10 ^ (e2 - e1)
           r = 10 ^ (e1 - e2)
    {-# INLINE (+) #-}

    Scientific c1 e1 - Scientific c2 e2
       | e1 < e2   = scientific (c1   - c2*l) e1
       | otherwise = scientific (c1*r - c2  ) e2
         where
           l = 10 ^ (e2 - e1)
           r = 10 ^ (e1 - e2)
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
      | e < 0     = c % (10 ^ negate e)
      | otherwise = (c * 10 ^ e) % 1
    {-# INLINE toRational #-}

-- | /WARNING:/ 'recip' and '/' will diverge when their outputs have
-- an infinite decimal expansion. 'fromRational' will diverge when the
-- input 'Rational' has an infinite decimal expansion.
instance Fractional Scientific where
    recip = fromRational . recip . toRational
    {-# INLINE recip #-}

    fromRational rational
        | numer < 0 = negate $ go (negate numer) 0 0
        | otherwise =          go         numer  0 0
      where
        numer = numerator   rational
        denom = denominator rational

        go :: Integer -> Integer -> Int -> Scientific
        go  0 !c !e     = scientific c e
        go !n !c !e
            | n < denom = go (n*10) (c * 10) (e-1) -- TODO: Use a logarithm here!
            | otherwise = go r      (c + q)   e
          where
            (q, r) = n `quotRem` denom
    {-# INLINE fromRational #-}

instance RealFrac Scientific where
    properFraction (Scientific c e)
        | e < 0     = let (q, r) = c `quotRem` (10 ^ negate e)
                      in (fromInteger q, scientific r e)
        | otherwise = (fromInteger c * 10 ^ e, 0)
    {-# INLINE properFraction #-}

    truncate = whenFloating $ \c e ->
                 fromInteger $ c `quot` (10 ^ negate e)
    {-# INLINE truncate #-}

    round = whenFloating $ \c e ->
      let m = c `quot` (10 ^ (negate e - 1))
          (n, r) = m `quotRem` 10
      in fromInteger $
           if c < 0
           then if r < (-5) || (r == (-5) && odd  n) then n-1 else n
           else if r <   5  || (r ==   5  && even n) then n   else n+1
    {-# INLINE round #-}

    ceiling = whenFloating $ \c e ->
                let (q, r) = c `quotRem` (10 ^ negate e)
                in fromInteger $! if r > 0 then q + 1 else q
    {-# INLINE ceiling #-}

    floor = whenFloating $ \c e ->
              fromInteger (c `div` (10 ^ negate e))
    {-# INLINE floor #-}

----------------------------------------------------------------------

whenFloating :: (Num a) => (Integer -> Int -> a) -> Scientific -> a
whenFloating f (Scientific c e)
    | e < 0     = f c e
    | otherwise = fromInteger c * 10 ^ e
{-# INLINE whenFloating #-}

----------------------------------------------------------------------

{-# RULES
"realToFrac/Scientific->Scientific" realToFrac = id :: Scientific -> Scientific #-}

-- | Efficient conversion from a 'Scientific' to a 'Fractional' number.
--
-- Note that this module provides rewrite RULES that convert
-- 'realToFrac' into 'toFractional' when going from a 'Scientific' to
-- either a 'Double', 'Float', 'CDouble' or 'CFloat' to avoid going
-- via 'Rational'.
--
-- So it's recommended to use 'realToFrac' to convert to a
-- 'Fractional' number. However, if you don't want to rely on these
-- RULES this function can be used.
toFractional :: (Fractional a) => Scientific -> a
toFractional = whenFloating $ \c e -> fromInteger c / 10 ^ negate e
{-# INLINE toFractional #-}

{-# RULES
"realToFrac/Scientific->Double"  realToFrac = toFractional :: Scientific -> Double
"realToFrac/Scientific->Float"   realToFrac = toFractional :: Scientific -> Float
"realToFrac/Scientific->CDouble" realToFrac = toFractional :: Scientific -> CDouble
"realToFrac/Scientific->CFloat"  realToFrac = toFractional :: Scientific -> CFloat #-}

-- | Efficient conversion from a 'RealFloat' into a 'Scientific'
-- number.
--
-- Note that this module provides rewrite RULES that convert
-- 'realToFrac' into 'fromRealFloat' when going from either a
-- 'Double', 'Float', 'CDouble' or 'CFloat' to a 'Scientific' to avoid
-- going via 'Rational'.
--
-- So it's recommended to use 'realToFrac' to convert 'Real' numbers
-- into 'Scientific'. However, if you don't want to rely on these
-- RULES this function can be used.
fromRealFloat :: (RealFloat a) => a -> Scientific
fromRealFloat rf
      -- integers are way more efficient to convert via Rational.
      -- We do pay the cost of always converting to Rational first though.
    | denominator rat == 1 = fromInteger $ numerator rat
    | rf < 0               = negate $ fromNonNegRealFloat $ negate rf
    | otherwise            =          fromNonNegRealFloat          rf
    where
      rat = toRational rf

      fromNonNegRealFloat r = go digits 0 0
        where
          (digits, e) = floatToDigits 10 r

          go []     !c !n = scientific c (e - n)
          go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)
{-# INLINE fromRealFloat #-}

{-# RULES
"realToFrac/Double->Scientific"  realToFrac = fromRealFloat :: Double  -> Scientific
"realToFrac/Float->Scientific"   realToFrac = fromRealFloat :: Float   -> Scientific
"realToFrac/CDouble->Scientific" realToFrac = fromRealFloat :: CDouble -> Scientific
"realToFrac/CFloat->Scientific"  realToFrac = fromRealFloat :: CFloat  -> Scientific #-}

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

----------------------------------------------------------------------

-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

-- | A @Text@ @Builder@ which renders a scientific number to full
-- precision, using standard decimal notation for arguments whose
-- absolute value lies between @0.1@ and @9,999,999@, and scientific
-- notation otherwise.
scientificBuilder :: Scientific -> Builder
scientificBuilder = formatScientificBuilder Generic Nothing

-- | Like 'scientificBuilder' but provides rendering options.
formatScientificBuilder :: FPFormat
                        -> Maybe Int  -- ^ Number of decimal places to render.
                        -> Scientific
                        -> Builder
formatScientificBuilder fmt decs scntfc@(Scientific c _)
   | c < 0 = singleton '-' <> doFmt fmt (toDecimalDigits (-scntfc))
   | otherwise =              doFmt fmt (toDecimalDigits   scntfc)
 where
  doFmt format (is, e) =
    let ds = map i2d is in
    case format of
     Generic ->
      doFmt (if e < 0 || e > 7 then Exponent else Fixed)
            (is,e)
     Exponent ->
      case decs of
       Nothing ->
        let show_e' = decimal (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> singleton d <> ".0e" <> show_e'
          (d:ds') -> singleton d <> singleton '.' <> fromString ds' <> singleton 'e' <> show_e'
          []      -> error "formatRealFloat/doFmt/Exponent: []"
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> "0." <> fromText (T.replicate dec' "0") <> "e0"
         _ ->
          let
           (ei,is') = roundTo (dec'+1) is
           (d:ds') = map i2d (if ei > 0 then init is' else is')
          in
          singleton d <> singleton '.' <> fromString ds' <> singleton 'e' <> decimal (e-1+ei)
     Fixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> fromString ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." <> fromText (T.replicate (-e) "0") <> fromString ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) <> singleton '.' <> mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map i2d is')
         in
         mk0 ls <> (if null rs then "" else singleton '.' <> fromString rs)
        else
         let
          (ei,is') = roundTo dec' (replicate (-e) 0 ++ is)
          d:ds' = map i2d (if ei > 0 then is' else 0:is')
         in
         singleton d <> (if null ds' then "" else singleton '.' <> fromString ds')

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))

----------------------------------------------------------------------

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
