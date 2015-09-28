{ mkDerivation, base, binary, bytestring, containers, deepseq, ghc-prim
, hashable, integer-gmp, QuickCheck, smallcheck, stdenv, tasty
, tasty-ant-xml, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, vector
}:
mkDerivation {
  pname = "scientific";
  version = "0.3.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq ghc-prim hashable integer-gmp
    text vector
  ];
  testHaskellDepends = [
    base bytestring QuickCheck smallcheck tasty tasty-ant-xml
    tasty-hunit tasty-quickcheck tasty-smallcheck text
  ];
  homepage = "https://github.com/basvandijk/scientific";
  description = "Numbers represented using scientific notation";
  license = stdenv.lib.licenses.bsd3;
}
