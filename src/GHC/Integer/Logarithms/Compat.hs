{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

module GHC.Integer.Logarithms.Compat
    ( integerLog2#
    , wordLog2#
    ) where

#if __GLASGOW_HASKELL__ >= 702
import GHC.Integer.Logarithms (integerLog2#, wordLog2#)
#else
#include "MachDeps.h"

import GHC.Integer.GMP.Internals (Integer(S#, J#))

import GHC.Base ( indexWordArray#, uncheckedIShiftL#, indexInt8Array#
                , word2Int#, ByteArray#, newByteArray#, writeInt8Array#
                , (==#), (<#), (+#), (*#)
                , unsafeFreezeByteArray#, realWorld#
                , neWord#, (-#), uncheckedShiftRL#
                , Int#, Word#, int2Word#
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
