{ mkDerivation, base, binary, bytestring, containers, criterion
, deepseq, hashable, integer-gmp, integer-logarithms, primitive
, QuickCheck, smallcheck, stdenv, tasty, tasty-ant-xml, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, template-haskell, text
}:
mkDerivation {
  pname = "scientific";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq hashable integer-gmp
    integer-logarithms primitive template-haskell text
  ];
  testHaskellDepends = [
    base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
    tasty-hunit tasty-quickcheck tasty-smallcheck text
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/basvandijk/scientific";
  description = "Numbers represented using scientific notation";
  license = stdenv.lib.licenses.bsd3;
}
