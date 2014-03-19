{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Test.Tasty
import Test.Tasty.SmallCheck (testProperty)
import Test.SmallCheck
import Data.Scientific as Scientific
import Test.SmallCheck.Series -- (Serial, series, cons2)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.ByteString.Builder.Scientific as B
import Data.Text.Lazy.Builder.Scientific  as T

main :: IO ()
main = defaultMain $ testGroup "scientific"
  [ testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === s

    , testProperty "toDecimalDigits_laws"
                    toDecimalDigits_laws
    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific B.Generic Nothing s ==
          TL.unpack (TLB.toLazyText $ T.formatScientificBuilder B.Generic Nothing s)

      , testProperty "ByteString" $ \s ->
          formatScientific B.Generic Nothing s ==
          BLC8.unpack (B.toLazyByteString $ B.formatScientificBuilder B.Generic Nothing s)
      ]
    ]

  , testGroup "Num"
    [ testGroup "Equal to Rational"
      [ testProperty "fromInteger" $ \i -> fromInteger i === fromRational (fromInteger i)
      , testProperty "+"           $ bin (+)
      , testProperty "-"           $ bin (-)
      , testProperty "*"           $ bin (*)
      , testProperty "abs"         $ unary abs
      , testProperty "negate"      $ unary negate
      , testProperty "signum"      $ unary signum
      ]

    , testProperty "0 identity of +" $ \a -> a + 0 === a
    , testProperty "1 identity of *" $ \a -> 1 * a === a
    , testProperty "0 identity of *" $ \a -> 0 * a === 0

    , testProperty "associativity of +"         $ \a b c -> a + (b + c) === (a + b) + c
    , testProperty "commutativity of +"         $ \a b   -> a + b       === b + a
    , testProperty "distributivity of * over +" $ \a b c -> a * (b + c) === a * b + a * c

    , testProperty "subtracting the addition" $ \x y -> x + y - y === x

    , testProperty "+ and negate" $ \x -> x + negate x === 0
    , testProperty "- and negate" $ \x -> x - negate x === x + x

    , testProperty "abs . negate == id" $ over nonNegativeScientifics $ \x ->
                                            abs (negate x) === x
    ]

  , testGroup "Real"
    [ testProperty "fromRational . toRational == id" $ \x ->
        (fromRational . toRational) x === x
    ]

  , testGroup "RealFrac"
    [ testGroup "Equal to Rational"
      [ testProperty "properFraction" $ \x ->
          let (n1::Integer, f1::Scientific) = properFraction x
              (n2::Integer, f2::Rational)   = properFraction (toRational x)
          in (n1 == n2) && (f1 == fromRational f2)

      , testProperty "round" $ \(x::Scientific) ->
          (round x :: Integer) == round (toRational x)

      , testProperty "truncate" $ \(x::Scientific) ->
          (truncate x :: Integer) == truncate (toRational x)

      , testProperty "ceiling" $ \(x::Scientific) ->
          (ceiling x :: Integer) == ceiling (toRational x)

      , testProperty "floor" $ \(x::Scientific) ->
          (floor x :: Integer) == floor (toRational x)
      ]

    , testProperty "properFraction_laws" properFraction_laws

    , testProperty "round"    $ \s -> round    s == roundDefault    s
    , testProperty "truncate" $ \s -> truncate s == truncateDefault s
    , testProperty "ceiling"  $ \s -> ceiling  s == ceilingDefault  s
    , testProperty "floor"    $ \s -> floor    s == floorDefault    s
    ]

  , testGroup "Conversions"
    [ testProperty "fromRealFloat" $ \s ->
        Scientific.fromFloatDigits (realToFrac s :: Double) === s
    ]
  ]

-- | ('==') specialized to 'Scientific' so we don't have to put type
-- signatures everywhere.
(===) :: Scientific -> Scientific -> Bool
(===) = (==)
infix 4 ===

bin :: (forall a. Num a => a -> a -> a) -> Scientific -> Scientific -> Bool
bin op a b = toRational (a `op` b) == toRational a `op` toRational b

unary :: (forall a. Num a => a -> a) -> Scientific -> Bool
unary op a = toRational (op a) == op (toRational a)

toDecimalDigits_laws :: (Monad m) => Property m
toDecimalDigits_laws = over nonNegativeScientifics $ \x ->
  let (ds, e) = Scientific.toDecimalDigits x

      rule1 = length ds >= 1

      rule2 = toRational x == coeff * 10 ^^ e
      coeff = foldr (\di a -> a / 10 + fromIntegral di) 0 (0:ds)

      rule3 = all (\di -> 0 <= di && di <= 9) ds

  in rule1 && rule2 && rule3

properFraction_laws :: Scientific -> Bool
properFraction_laws x = fromInteger n + f === x        &&
                        (positive n == posX || n == 0) &&
                        (positive f == posX || f == 0) &&
                        abs f < 1
    where
      posX = positive x

      (n, f) = properFraction x :: (Integer, Scientific)

positive :: (Ord a, Num a) => a -> Bool
positive y = y >= 0

floorDefault :: Scientific -> Integer
floorDefault x = if r < 0 then n - 1 else n
                 where (n,r) = properFraction x

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > 0 then n + 1 else n
                   where (n,r) = properFraction x

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = properFraction x

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = properFraction x
                     m     = if r < 0 then n - 1 else n + 1
                 in case signum (abs r - 0.5) of
                      -1 -> n
                      0  -> if even n then n else m
                      1  -> m
                      _  -> error "round default defn: Bad value"

----------------------------------------------------------------------

instance (Monad m) => Serial m Scientific where
    series = scientifics

scientifics :: (Monad m) => Series m Scientific
scientifics = cons2 scientific

nonNegativeScientifics :: (Monad m) => Series m Scientific
nonNegativeScientifics = liftM getNonNegative series
