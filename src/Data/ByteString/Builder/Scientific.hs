{-# LANGUAGE CPP, OverloadedStrings #-}

module Data.ByteString.Builder.Scientific
    ( scientificBuilder
    , formatScientificBuilder
    , FPFormat(..)
    ) where

import           Data.Scientific   (Scientific)
import qualified Data.Scientific as Scientific

import Data.Text.Lazy.Builder.RealFloat (FPFormat(..))

import           Data.List (genericReplicate, genericSplitAt)
import qualified Data.ByteString.Char8 as BC8
import           Data.ByteString.Builder (Builder, string8, char8, integerDec)
import           Data.ByteString.Builder.Extra (byteStringCopy)

import Utils (roundTo, i2d)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid                  (mempty)
#endif

#if MIN_VERSION_base(4,5,0)
import Data.Monoid                  ((<>))
#else
import Data.Monoid                  (Monoid, mappend)
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
infixr 6 <>
#endif


-- | A @ByteString@ @Builder@ which renders a scientific number to full
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
formatScientificBuilder fmt decs scntfc
   | scntfc < 0 = char8 '-' <> doFmt fmt (Scientific.toDecimalDigits (-scntfc))
   | otherwise  =              doFmt fmt (Scientific.toDecimalDigits   scntfc)
 where
  doFmt :: FPFormat -> ([Int], Integer) -> Builder
  doFmt format (is, e) =
    let ds = map i2d is in
    case format of
     Generic ->
      doFmt (if e < 0 || e > 7 then Exponent else Fixed)
            (is,e)
     Exponent ->
      case decs of
       Nothing ->
        let show_e' = integerDec (e-1) in
        case ds of
          "0"     -> byteStringCopy "0.0e0"
          [d]     -> char8 d <> byteStringCopy ".0e" <> show_e'
          (d:ds') -> char8 d <> char8 '.' <> string8 ds' <> char8 'e' <> show_e'
          []      -> error $ "Data.ByteString.Builder.Scientific.formatScientificBuilder" ++
                             "/doFmt/Exponent: []"
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> byteStringCopy "0." <>
                byteStringCopy (BC8.replicate dec' '0') <>
                byteStringCopy "e0"
         _ ->
          let
           (ei,is') = roundTo (toInteger dec'+1) is
          in case map i2d (if ei > 0 then init is' else is') of
               [] -> error "The impossible happened"
               d:ds' ->
                 char8 d <> char8 '.' <> string8 ds' <>
                 char8 'e' <> integerDec (e - 1 + toInteger ei)
     Fixed ->
      let
       mk0 ls = case ls of { "" -> char8 '0' ; _ -> string8 ls}
      in
      case decs of
       Nothing
          | e <= 0    -> byteStringCopy "0." <>
                         string8 (genericReplicate (-e) '0') <>
                         string8 ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) <> char8 '.' <> mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = toInteger $ max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo (dec' + e) is
          (ls,rs)  = genericSplitAt (e + toInteger ei) (map i2d is')
         in
         mk0 ls <> (if null rs then mempty else char8 '.' <> string8 rs)
        else
         let
          (ei,is') = roundTo dec' (genericReplicate (-e) 0 ++ is)
         in case map i2d (if ei > 0 then is' else 0:is') of
              [] -> error "The impossible happened"
              d:ds' -> char8 d <> (if null ds' then mempty else char8 '.' <> string8 ds')
