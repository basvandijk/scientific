module Main where

import Criterion.Main
import Data.Scientific

main :: IO ()
main = defaultMain
       [ bgroup "realToFrac"
         [ bgroup "Scientific->Double"
           [ sToD "pos"    pos
           , sToD "neg"    neg
           , sToD "int"    int
           , sToD "negInt" negInt
           ]
         , bgroup "Double->Scientific"
           [ dToS "pos"    pos
           , dToS "neg"    neg
           , dToS "int"    int
           , dToS "negInt" negInt
           ]
         ]
       , bgroup "floor"
         [ bench "floor"        (nf (floor :: Scientific -> Integer) $! pos)
         , bench "floorDefault" (nf floorDefault                     $! pos)
         ]
       , bgroup "ceiling"
         [ bench "ceiling"        (nf (ceiling :: Scientific -> Integer) $! pos)
         , bench "ceilingDefault" (nf ceilingDefault                     $! pos)
         ]
       , bgroup "truncate"
         [ bench "truncate"        (nf (truncate :: Scientific -> Integer) $! pos)
         , bench "truncateDefault" (nf truncateDefault                     $! pos)
         ]

       , bgroup "round"
         [ bench "round"        (nf (round :: Scientific -> Integer) $! pos)
         , bench "roundDefault" (nf roundDefault                     $! pos)
         ]

       , bgroup "toDecimalDigits"
         [ bench "big" (nf toDecimalDigits $! big)
         ]
       ]
    where
      pos :: Fractional a => a
      pos = 12345.12345

      neg :: Fractional a => a
      neg = -pos

      int :: Fractional a => a
      int = 12345

      negInt :: Fractional a => a
      negInt = -int

      big :: Scientific
      big = read $ "0." ++ concat (replicate 20 "0123456789")

realToFracStoD :: Scientific -> Double
realToFracStoD = fromRational . toRational
{-# INLINE realToFracStoD #-}

realToFracDtoS :: Double -> Scientific
realToFracDtoS = fromRational . toRational
{-# INLINE realToFracDtoS #-}


sToD :: String -> Scientific -> Benchmark
sToD name f = bgroup name
              [ bench "fromScientific" . nf (realToFrac     :: Scientific -> Double) $! f
              , bench "via Rational"   . nf (realToFracStoD :: Scientific -> Double) $! f
              ]

dToS :: String -> Double -> Benchmark
dToS name f = bgroup name
              [ bench "fromRealFloat"  . nf (realToFrac     :: Double -> Scientific) $! f
              , bench "via Rational"   . nf (realToFracDtoS :: Double -> Scientific) $! f
              ]

floorDefault :: Scientific -> Integer
floorDefault x = if r < 0 then n - 1 else n
                 where (n,r) = properFraction x
{-# INLINE floorDefault #-}

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > 0 then n + 1 else n
                   where (n,r) = properFraction x
{-# INLINE ceilingDefault #-}

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = properFraction x
{-# INLINE truncateDefault #-}

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = properFraction x
                     m     = if r < 0 then n - 1 else n + 1
                 in case signum (abs r - 0.5) of
                      -1 -> n
                      0  -> if even n then n else m
                      1  -> m
                      _  -> error "round default defn: Bad value"
{-# INLINE roundDefault #-}
