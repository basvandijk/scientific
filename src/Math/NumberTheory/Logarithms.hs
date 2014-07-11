{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-- | Integer logarithm, copied from Daniel Fischer's @arithmoi@
module Math.NumberTheory.Logarithms ( integerLog10' ) where

#if defined(INTEGER_SIMPLE) && __GLASGOW_HASKELL__ < 702
import GHC.Integer.Logarithms (integerLogBase#)
import GHC.Base (Int(I#))

-- | Only defined for positive inputs!
integerLog10' :: Integer -> Int
integerLog10' m = I# (integerLogBase# 10 m)

#else
import GHC.Base ( Int(I#), Word#, Int#
                , int2Word#, eqWord#, neWord#, (-#), and#, uncheckedShiftRL#

#if __GLASGOW_HASKELL__ >= 707
                , isTrue#
#endif
                )

#if __GLASGOW_HASKELL__ >= 702
import GHC.Integer.Logarithms (integerLog2#, wordLog2#)
#else
#include "MachDeps.h"

import GHC.Integer.GMP.Internals (Integer(S#, J#))

import GHC.Base ( indexWordArray#, uncheckedIShiftL#, indexInt8Array#
                , word2Int#, ByteArray#, newByteArray#, writeInt8Array#
                , (==#), (<#), (+#), (*#)
                , unsafeFreezeByteArray#, realWorld#
                )

#if (WORD_SIZE_IN_BITS != 32) && (WORD_SIZE_IN_BITS != 64)
#error Only word sizes 32 and 64 are supported.
#endif


#if WORD_SIZE_IN_BITS == 32

#define WSHIFT 5
#define MMASK 31

#else

#define WSHIFT 6
#define MMASK 63

#endif

-- | Calculate the integer base 2 logarithm of an 'Integer'.
--   The calculation is much more efficient than for the general case.
--
--   The argument must be strictly positive, that condition is /not/ checked.
integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i)
integerLog2# (J# s ba) = check (s -# 1#)
  where
    check i = case indexWordArray# ba i of
                0## -> check (i -# 1#)
                w   -> wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)

-- | This function calculates the integer base 2 logarithm of a 'Word#'.
--   @'wordLog2#' 0## = -1#@.
{-# INLINE wordLog2# #-}
wordLog2# :: Word# -> Int#
wordLog2# w =
  case leadingZeros of
   BA lz ->
    let zeros u = indexInt8Array# lz (word2Int# u) in
#if WORD_SIZE_IN_BITS == 64
    case uncheckedShiftRL# w 56# of
     a ->
      if a `neWord#` 0##
       then 64# -# zeros a
       else
        case uncheckedShiftRL# w 48# of
         b ->
          if b `neWord#` 0##
           then 56# -# zeros b
           else
            case uncheckedShiftRL# w 40# of
             c ->
              if c `neWord#` 0##
               then 48# -# zeros c
               else
                case uncheckedShiftRL# w 32# of
                 d ->
                  if d `neWord#` 0##
                   then 40# -# zeros d
                   else
#endif
                    case uncheckedShiftRL# w 24# of
                     e ->
                      if e `neWord#` 0##
                       then 32# -# zeros e
                       else
                        case uncheckedShiftRL# w 16# of
                         f ->
                          if f `neWord#` 0##
                           then 24# -# zeros f
                           else
                            case uncheckedShiftRL# w 8# of
                             g ->
                              if g `neWord#` 0##
                               then 16# -# zeros g
                               else 8# -# zeros w

-- Lookup table
data BA = BA ByteArray#

leadingZeros :: BA
leadingZeros =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 9# s1 of
                s2 ->
                  let fillA lim val idx st =
                        if idx ==# 256#
                          then st
                          else if idx <# lim
                                then case writeInt8Array# mba idx val st of
                                        nx -> fillA lim val (idx +# 1#) nx
                                else fillA (2# *# lim) (val -# 1#) idx st
                  in case fillA 2# 8# 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b
#endif

-- | Only defined for positive inputs!
integerLog10' :: Integer -> Int
integerLog10' n
  | n < 10      = 0
  | n < 100     = 1
  | otherwise   = ex + integerLog10' (n `quot` integerPower 10 ex)
    where
      ln = I# (integerLog2# n)
      -- u/v is a good approximation of log 2/log 10
      u  = 1936274
      v  = 6432163
      -- so ex is a good approximation to integerLogBase 10 n
      ex = fromInteger ((u * fromIntegral ln) `quot` v)

-- | Power of an 'Integer' by the left-to-right repeated squaring algorithm.
--   This needs two multiplications in each step while the right-to-left
--   algorithm needs only one multiplication for 0-bits, but here the
--   two factors always have approximately the same size, which on average
--   gains a bit when the result is large.
--
--   For small results, it is unlikely to be any faster than '(^)', quite
--   possibly slower (though the difference shouldn't be large), and for
--   exponents with few bits set, the same holds. But for exponents with
--   many bits set, the speedup can be significant.
--
--   /Warning:/ No check for the negativity of the exponent is performed,
--   a negative exponent is interpreted as a large positive exponent.
integerPower :: Integer -> Int -> Integer
integerPower b (I# e#) = power b (int2Word# e#)

power :: Integer -> Word# -> Integer
power b w#
  | isTrue# (w# `eqWord#` 0##) = 1
  | isTrue# (w# `eqWord#` 1##) = b
  | otherwise           = go (wordLog2# w# -# 1#) b (b*b)
    where
      go 0# l h = if isTrue# ((w# `and#` 1##) `eqWord#` 0##) then l*l else (l*h)
      go i# l h
        | w# `hasBit#` i#   = go (i# -# 1#) (l*h) (h*h)
        | otherwise         = go (i# -# 1#) (l*l) (l*h)

-- | A raw version of testBit for 'Word#'.
hasBit# :: Word# -> Int# -> Bool
hasBit# w# i# = isTrue# (((w# `uncheckedShiftRL#` i#) `and#` 1##) `neWord#` 0##)

#if __GLASGOW_HASKELL__ < 707
isTrue# :: Bool -> Bool
isTrue# = id
#endif
#endif
