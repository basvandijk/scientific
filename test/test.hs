{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Int
import           Data.Word
import           Data.Scientific                    as Scientific
import           Test.Tasty
import           Test.Tasty.Runners.AntXML
import           Test.Tasty.HUnit                          (testCase, (@?=), Assertion)
import qualified Test.SmallCheck                    as SC
import qualified Test.SmallCheck.Series             as SC
import qualified Test.Tasty.SmallCheck              as SC  (testProperty)
import qualified Test.QuickCheck                    as QC
import qualified Test.Tasty.QuickCheck              as QC  (testProperty)
import qualified Data.Text.Lazy                     as TL  (unpack)
import qualified Data.Text.Lazy.Builder             as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Scientific  as T

#ifdef BYTESTRING_BUILDER
import qualified Data.ByteString.Lazy.Char8         as BLC8
import qualified Data.ByteString.Builder.Scientific as B

#if !MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Lazy.Builder       as B
#else
import qualified Data.ByteString.Builder            as B
#endif
#endif

main :: IO ()
main = testMain $ testGroup "scientific"
  [ smallQuick "normalization"
       (SC.over   normalizedScientificSeries $ \s ->
            s /= 0 SC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)
       (QC.forAll normalizedScientificGen    $ \s ->
            s /= 0 QC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)

  , testGroup "Parsing"
    [ testCase "reads \"\""        $ testReads ""        []
    , testCase "reads \"1.\""      $ testReads "1."      [(1.0, ".")]
    , testCase "reads \"1.2e\""    $ testReads "1.2e"    [(1.2, "e")]
    , testCase "reads \"(1.3 )\""  $ testReads "(1.3 )"  [(1.3, "")]
    , testCase "reads \"((1.3))\"" $ testReads "((1.3))" [(1.3, "")]
    , testCase "reads \" 1.3\""    $ testReads " 1.3"    [(1.3, "")]
    ]

  , testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === s

    , smallQuick "toDecimalDigits_laws"
        (SC.over   nonNegativeScientificSeries toDecimalDigits_laws)
        (QC.forAll nonNegativeScientificGen    toDecimalDigits_laws)

    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific (FloatingFormat Generic Nothing) s ==
          TL.unpack (TLB.toLazyText $
                       T.formatScientificBuilder (FloatingFormat Generic Nothing) s)

#ifdef BYTESTRING_BUILDER
      , testProperty "ByteString" $ \s ->
          formatScientific (FloatingFormat Generic Nothing) s ==
          BLC8.unpack (B.toLazyByteString $
                        B.formatScientificBuilder (FloatingFormat Generic Nothing) s)
#endif
      ]

    , testProperty "formatScientific_fromFloatDigits" $ \(d::Double) ->
        formatScientific (FloatingFormat Generic Nothing) (Scientific.fromFloatDigits d) ==
        show d

    -- , testProperty "formatScientific_realToFrac" $ \(d::Double) ->
    --     formatScientific B.Generic Nothing (realToFrac d :: Scientific) ==
    --     show d
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

    , smallQuick "abs . negate == id"
        (SC.over   nonNegativeScientificSeries $ \x -> abs (negate x) === x)
        (QC.forAll nonNegativeScientificGen    $ \x -> abs (negate x) === x)
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
    [ testGroup "Float"  $ conversionsProperties (undefined :: Float)
    , testGroup "Double" $ conversionsProperties (undefined :: Double)

    , testGroup "floatingOrInteger"
      [ testProperty "correct conversion" $ \s ->
            case floatingOrInteger s :: Either Double Int of
              Left  d -> d == toRealFloat s
              Right i -> i == fromInteger (coefficient s') * 10^(base10Exponent s')
                  where
                    s' = normalize s
      , testProperty "Integer == Right" $ \(i::Integer) ->
          (floatingOrInteger (fromInteger i) :: Either Double Integer) == Right i
      , smallQuick "Double == Left"
          (\(d::Double) -> genericIsFloating d SC.==>
             (floatingOrInteger (realToFrac d) :: Either Double Integer) == Left d)
          (\(d::Double) -> genericIsFloating d QC.==>
             (floatingOrInteger (realToFrac d) :: Either Double Integer) == Left d)
      ]
    , testGroup "toBoundedInteger"
      [ testGroup "correct conversion"
        [ testProperty "Int64"       $ toBoundedIntegerConversion (undefined :: Int64)
        , testProperty "Word64"      $ toBoundedIntegerConversion (undefined :: Word64)
        , testProperty "NegativeNum" $ toBoundedIntegerConversion (undefined :: NegativeInt)
        ]
      ]
    ]
  , testGroup "toBoundedRealFloat"
    [ testCase "0 * 10^1000 == 0" $
        toBoundedRealFloat (scientific 0 1000) @?= Right (0 :: Float)
    ]
  , testGroup "toBoundedInteger"
    [ testGroup "to Int64" $
      [ testCase "succ of maxBound" $
        let i = succ . fromIntegral $ (maxBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "pred of minBound" $
        let i = pred . fromIntegral $ (minBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "0 * 10^1000 == 0" $
          toBoundedInteger (scientific 0 1000) @?= Just (0 :: Int64)
      ]
    ]
  , testGroup "Predicates"
    [ testProperty "isFloating" $ \s -> isFloating s ==      genericIsFloating s
    , testProperty "isInteger"  $ \s -> isInteger  s == not (genericIsFloating s)
    ]
  , testGroup "IntegerFormat" $
    let formatInteger c e = formatScientific IntegerFormat $ scientific c e in
    [ QC.testProperty "large int - pad zeros" $ QC.forAll (QC.elements [0..10]) $ \e ->
        formatInteger 23456 e == "23456" ++ replicate e '0'
    , QC.testProperty "medium int - truncate" $ QC.forAll (QC.elements [(-4)..0]) $ \e ->
        formatInteger 23456 e == take (5 + e) "23456"
    , QC.testProperty "small values = 0 " $ QC.forAll (QC.elements [(-20)..(-5)]) $ \e ->
        formatInteger 23456 e == "0"
    ]
  ]

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients (antXMLRunner:defaultIngredients)

testReads :: String -> [(Scientific, String)] -> Assertion
testReads inp out = reads inp @?= out

genericIsFloating :: RealFrac a => a -> Bool
genericIsFloating a = fromInteger (floor a :: Integer) /= a

conversionsProperties :: forall realFloat.
                         ( RealFloat    realFloat
                         , QC.Arbitrary realFloat
                         , SC.Serial IO realFloat
                         , Show         realFloat
                         )
                      => realFloat -> [TestTree]
conversionsProperties _ =
  [
    -- testProperty "fromFloatDigits_1" $ \(d :: realFloat) ->
    --   Scientific.fromFloatDigits d === realToFrac d

    -- testProperty "fromFloatDigits_2" $ \(s :: Scientific) ->
    --   Scientific.fromFloatDigits (realToFrac s :: realFloat) == s

    testProperty "toRealFloat" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . realToFrac) d == d

  , testProperty "toRealFloat . fromFloatDigits == id" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . Scientific.fromFloatDigits) d == d

  -- , testProperty "fromFloatDigits . toRealFloat == id" $ \(s :: Scientific) ->
  --     Scientific.fromFloatDigits (Scientific.toRealFloat s :: realFloat) == s
  ]

toBoundedIntegerConversion
    :: forall i. (Integral i, Bounded i, Show i)
    => i -> Scientific -> Bool
toBoundedIntegerConversion _ s =
    case toBoundedInteger s :: Maybe i of
      Just i -> i == (fromIntegral $ (coefficient s') * 10^(base10Exponent s')) &&
                i >= minBound &&
                i <= maxBound
        where
          s' = normalize s
      Nothing -> isFloating s ||
                 s < fromIntegral (minBound :: i) ||
                 s > fromIntegral (maxBound :: i)

testProperty :: (SC.Testable IO test, QC.Testable test)
             => TestName -> test -> TestTree
testProperty n test = smallQuick n test test

smallQuick :: (SC.Testable IO smallCheck, QC.Testable quickCheck)
             => TestName -> smallCheck -> quickCheck -> TestTree
smallQuick n sc qc = testGroup n
                     [ SC.testProperty "smallcheck" sc
                     , QC.testProperty "quickcheck" qc
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

toDecimalDigits_laws :: Scientific -> Bool
toDecimalDigits_laws x =
  let (ds, e) = Scientific.toDecimalDigits x

      rule1 = n >= 1
      n     = length ds

      rule2 = toRational x == coeff * 10 ^^ e
      coeff = foldr (\di a -> a / 10 + fromIntegral di) 0 (0:ds)

      rule3 = all (\di -> 0 <= di && di <= 9) ds

      rule4 | n == 1    = True
            | otherwise = null $ takeWhile (==0) $ reverse ds

  in rule1 && rule2 && rule3 && rule4

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

newtype NegativeInt = NegativeInt Int
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded NegativeInt where
    minBound = -100
    maxBound = -10

----------------------------------------------------------------------
-- SmallCheck instances
----------------------------------------------------------------------

instance (Monad m) => SC.Serial m Scientific where
    series = scientifics

scientifics :: (Monad m) => SC.Series m Scientific
scientifics = SC.cons2 scientific

nonNegativeScientificSeries :: (Monad m) => SC.Series m Scientific
nonNegativeScientificSeries = liftM SC.getNonNegative SC.series

normalizedScientificSeries :: (Monad m) => SC.Series m Scientific
normalizedScientificSeries = liftM Scientific.normalize SC.series


----------------------------------------------------------------------
-- QuickCheck instances
----------------------------------------------------------------------

instance QC.Arbitrary Scientific where
    arbitrary = QC.frequency
      [ (70, scientific <$> QC.arbitrary
                        <*> intGen)
      , (20, scientific <$> QC.arbitrary
                        <*> bigIntGen)
      , (10, scientific <$> pure 0
                        <*> bigIntGen)
      ]

    shrink s = zipWith scientific (QC.shrink $ Scientific.coefficient s)
                                  (QC.shrink $ Scientific.base10Exponent s)

nonNegativeScientificGen :: QC.Gen Scientific
nonNegativeScientificGen =
    scientific <$> (QC.getNonNegative <$> QC.arbitrary)
               <*> intGen

normalizedScientificGen :: QC.Gen Scientific
normalizedScientificGen = Scientific.normalize <$> QC.arbitrary

bigIntGen :: QC.Gen Int
bigIntGen = QC.sized $ \size -> QC.resize (size * 1000) intGen

intGen :: QC.Gen Int
#if MIN_VERSION_QuickCheck(2,7,0)
intGen = QC.arbitrary
#else
intGen = QC.sized $ \n -> QC.choose (-n, n)
#endif
