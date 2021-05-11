{-# LANGUAGE CPP #-}

module GHC.Integer.Compat (divInteger) where

#if MIN_VERSION_base(4,15,0)
import GHC.Integer (divInteger)
#else

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
#endif
