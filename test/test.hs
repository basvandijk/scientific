{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Monad
import           Data.Int
import           Data.Word
import           Data.Scientific                    as Scientific
import           Test.Tasty
import           Test.Tasty.HUnit                          (testCase, (@?=), (@=?), Assertion, assertBool)
import           Test.QuickCheck                       (Property, (===), (.&&.))
import qualified Test.QuickCheck                    as QC
import           Test.Tasty.QuickCheck                 (testProperty)
import qualified Data.Binary                        as Binary (encode, decode)
import qualified Data.Text.Lazy                     as TL  (unpack)
import qualified Data.Text.Lazy.Builder             as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Scientific  as T
import           Numeric ( floatToDigits )

import qualified Data.ByteString.Lazy.Char8         as BLC8
import qualified Data.ByteString.Builder.Scientific as B
import qualified Data.ByteString.Builder            as B
import           Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = testMain $ testGroup "scientific"
  [ testGroup "DoS protection"
    [ testGroup "Eq"
      [ testCase "1e1000000" $ assertBool "" $ (read "1e1000000" :: Scientific) == (read "1e1000000" :: Scientific)
      , testCase "1e1000000 ineq" $ assertBool "" $ (read "1e1000000" :: Scientific) /= (read "1e1000002" :: Scientific)

      -- this also indirectly checks that 'read' is fast enough.
      , testCase "10...0" $ assertBool "" $
          (read "1e1000000" :: Scientific) ==
          (read ('1' : replicate 1000000 '0'))
      ]
    , testGroup "Ord"
      [ testCase "compare 1234e1000000 123e1000001" $
          compare (read "1234e1000000" :: Scientific) (read "123e1000001" :: Scientific) @?= GT

      , testCase "10...0" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "0"))
              @?= EQ
      , testCase "1...1" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "1"))
              @?= LT
      ]

    , testGroup "isInteger"
        [ testCase "1e1000000" $ True @=? isInteger (read "1e1000000" :: Scientific)
        , testCase "10...0e-1" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1" :: Scientific)
        , testCase "10...0e-10...0" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1000000" :: Scientific)
        , testCase "10...0e-20...0" $ False @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-2000000" :: Scientific)
        ]

    , testGroup "RealFrac"
      [ testGroup "floor"
        [ testCase "1e1000000"   $ (floor (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (floor (read "-1e-1000000" :: Scientific) :: Int) @?= (-1)
        , testCase "1e-1000000"  $ (floor (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "ceiling"
        [ testCase "1e1000000"   $ (ceiling (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (ceiling (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (ceiling (read "1e-1000000"  :: Scientific) :: Int) @?= 1
        ]
      , testGroup "round"
        [ testCase "1e1000000"   $ (round (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (round (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (round (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "truncate"
        [ testCase "1e1000000"   $ (truncate (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (truncate (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (truncate (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "properFracton"
        [ testCase "1e1000000"   $ properFraction (read "1e1000000" :: Scientific) @?= (0 :: Int, 0)
        , testCase "-1e-1000000" $ let s = read "-1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        , testCase "1e-1000000"  $ let s = read "1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        ]
      ]
    , testGroup "toRealFloat"
      [ testCase "1e1000000"  $ assertBool "Should be infinity!" $ isInfinite $
                                  (toRealFloat (read "1e1000000" :: Scientific) :: Double)
      , testCase "1e-1000000" $ (toRealFloat (read "1e-1000000" :: Scientific) :: Double) @?= 0
      ]
    , testGroup "toBoundedInteger"
      [ testCase "1e1000000"  $ (toBoundedInteger (read "1e1000000" :: Scientific) :: Maybe Int) @?= Nothing
      ]
    ]

  , testProperty "normalization"
       (QC.forAll normalizedScientificGen    $ \s ->
            s /= 0 QC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)

  , testGroup "Binary"
    [ testProperty "decode . encode == id" $ \s ->
        Binary.decode (Binary.encode s) === theSci s
    ]

  , testGroup "Parsing"
    [ testCase "reads \"\""        $ testReads ""        []
    , testCase "reads \"1.\""      $ testReads "1."      [(1.0, ".")]
    , testCase "reads \"1.2e\""    $ testReads "1.2e"    [(1.2, "e")]
    , testCase "reads \"(1.3 )\""  $ testReads "(1.3 )"  [(1.3, "")]
    , testCase "reads \"((1.3))\"" $ testReads "((1.3))" [(1.3, "")]
    , testCase "reads \" 1.3\""    $ testReads " 1.3"    [(1.3, "")]
    , testCase "read \" ( ((  -1.0e+3 ) ))\"" $ testRead " ( ((  -1.0e+3 ) ))" (-1000.0)
    , testCase "scientificP \"3\""       $ testScientificP "3"       [(3.0, "")]
    , testCase "scientificP \"3.0e2\""   $ testScientificP "3.0e2"   [(3.0, "e2"), (300.0, "")]
    , testCase "scientificP \"+3.0e+2\"" $ testScientificP "+3.0e+2" [(3.0, "e+2"), (300.0, "")]
    , testCase "scientificP \"-3.0e-2\"" $ testScientificP "-3.0e-2" [(-3.0, "e-2"), (-3.0e-2, "")]
    ]

  , testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === theSci s
    , testCase "show (Just 1)"    $ testShow (Just 1)    "Just 1.0"
    , testCase "show (Just 0)"    $ testShow (Just 0)    "Just 0.0"
    , testCase "show (Just (-1))" $ testShow (Just (-1)) "Just (-1.0)"

    , testGroup "toDecimalDigits"
      [ testProperty "laws"
          (QC.forAll nonNegativeScientificGen    toDecimalDigits_laws)

      , testProperty "== Numeric.floatToDigits"
          (toDecimalDigits_eq_floatToDigits . QC.getNonNegative)
      ]

    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          TL.unpack (TLB.toLazyText $
                       T.formatScientificBuilder Scientific.Generic Nothing s)

      , testProperty "ByteString" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          BLC8.unpack (B.toLazyByteString $
                        B.formatScientificBuilder Scientific.Generic Nothing s)
      ]

    , testProperty "formatScientific_fromFloatDigits" $ \(d::Double) ->
        formatScientific Scientific.Generic Nothing (Scientific.fromFloatDigits d) ==
        show d

    -- , testProperty "formatScientific_realToFrac" $ \(d::Double) ->
    --     formatScientific B.Generic Nothing (realToFrac d :: Scientific) ==
    --     show d
    ]

  , testGroup "Eq"
    [ testProperty "==" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        (s1 == s2) == (toRational s1 == toRational s2)
    , testProperty "s == s" $ \(s :: Scientific) -> s == s
    ]

  , testGroup "Ord"
    [ testProperty "compare" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        compare s1 s2 == compare (toRational s1) (toRational s2)
    ]

  , testGroup "Num"
    [ testGroup "Equal to Rational"
      [ testProperty "fromInteger" $ \i -> fromInteger i === theSci (fromRational (fromInteger i))
      , testProperty "+"           $ bin (+)
      , testProperty "-"           $ bin (-)
      , testProperty "*"           $ bin (*)
      , testProperty "abs"         $ unary abs
      , testProperty "negate"      $ unary negate
      , testProperty "signum"      $ unary signum
      ]

    , testProperty "0 identity of +" $ \a -> a + 0 === theSci a
    , testProperty "1 identity of *" $ \a -> 1 * a === theSci a
    , testProperty "0 identity of *" $ \a -> 0 * a === theSci 0

    , testProperty "associativity of +"         $ \a b c -> a + (b + c) === (a + b) + theSci c
    , testProperty "commutativity of +"         $ \a b   -> a + b       === b + theSci a
    , testProperty "distributivity of * over +" $ \a b c -> a * (b + c) === a * b + a * theSci c

    , testProperty "subtracting the addition" $ \x y -> x + y - y === theSci x

    , testProperty "+ and negate" $ \x -> theSci x + negate x === 0
    , testProperty "- and negate" $ \x -> theSci x - negate x === x + x

    , testProperty "abs . negate == id"
        (QC.forAll nonNegativeScientificGen    $ \x -> abs (negate x) === theSci x)
    ]

  , testGroup "Real"
    [ testProperty "fromRational . toRational == id" $ \x ->
        (fromRational . toRational) x === theSci x
    ]

  , testGroup "RealFrac"
    [ testGroup "Equal to Rational"
      [ testProperty "properFraction" $ \x ->
          let (n1::Integer, f1::Scientific) = properFraction x
              (n2::Integer, f2::Rational)   = properFraction (toRational x)
          in (n1 === n2) .&&. (f1 === fromRational f2)

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
    [ testProperty "fromRationalRepetend" $ \(l, r) -> r ==
        (case fromRationalRepetend (Just l) r of
          Left (s, rr) -> toRational s + rr
          Right (s, mbRepetend) ->
            case mbRepetend of
              Nothing       -> toRational s
              Just repetend -> toRationalRepetend s repetend)

    , testGroup "Float"  $ conversionsProperties (undefined :: Float)
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
      , testProperty "Double == Left"
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
  ]

-- used as type annotation
theSci :: Scientific -> Scientific
theSci = id

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients defaultIngredients

testReads :: String -> [(Scientific, String)] -> Assertion
testReads inp out = reads inp @?= out

testRead :: String -> Scientific -> Assertion
testRead inp out = read inp @?= out

testShow :: Maybe Scientific -> String -> Assertion
testShow inp out = show inp @?= out

testScientificP :: String -> [(Scientific, String)] -> Assertion
testScientificP inp out = readP_to_S Scientific.scientificP inp @?= out

genericIsFloating :: RealFrac a => a -> Bool
genericIsFloating a = fromInteger (floor a :: Integer) /= a

toDecimalDigits_eq_floatToDigits :: Double -> Bool
toDecimalDigits_eq_floatToDigits d =
    Scientific.toDecimalDigits (Scientific.fromFloatDigits d)
      == Numeric.floatToDigits 10 d

conversionsProperties :: forall realFloat.
                         ( RealFloat    realFloat
                         , QC.Arbitrary realFloat
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
    :: forall i. (Integral i, Bounded i)
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

properFraction_laws :: Scientific -> Property
properFraction_laws x = fromInteger n + f === x        .&&.
                        (positive n == posX || n == 0) .&&.
                        (positive f == posX || f == 0) .&&.
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
intGen = QC.arbitrary
