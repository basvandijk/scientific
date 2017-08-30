{ mkDerivation, base, binary, bytestring, containers, deepseq
, hashable, integer-gmp, integer-logarithms, QuickCheck, smallcheck, stdenv, tasty
, tasty-ant-xml, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, primitive, criterion
}:
mkDerivation {
  pname = "scientific";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq hashable integer-gmp
    integer-logarithms text primitive
  ];
  testHaskellDepends = [
    base bytestring QuickCheck smallcheck tasty tasty-ant-xml
    tasty-hunit tasty-quickcheck tasty-smallcheck text

    criterion
  ];
  homepage = "https://github.com/basvandijk/scientific";
  description = "Numbers represented using scientific notation";
  license = stdenv.lib.licenses.bsd3;
}
