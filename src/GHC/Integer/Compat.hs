{-# LANGUAGE CPP #-}

module GHC.Integer.Compat (divInteger) where

#ifdef MIN_VERSION_integer_simple

#if MIN_VERSION_integer_simple(0,1,1)
import GHC.Integer (divInteger)
#else
divInteger :: Integer -> Integer -> Integer
divInteger = div
#endif

#else

#if MIN_VERSION_integer_gmp(0,5,1)
import GHC.Integer (divInteger)
#else
divInteger :: Integer -> Integer -> Integer
divInteger = div
#endif

#endif
