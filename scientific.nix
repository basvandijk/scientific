{ cabal, deepseq, hashable, QuickCheck, smallcheck, tasty
, tastyAntXml, tastyHunit, tastyQuickcheck, tastySmallcheck, text
}:

cabal.mkDerivation (self: {
  pname = "scientific";
  version = "0.3.3.2";
  src = ./.;
  buildDepends = [ deepseq hashable text ];
  testDepends = [
    QuickCheck smallcheck tasty tastyAntXml tastyHunit tastyQuickcheck
    tastySmallcheck text
  ];
  meta = {
    homepage = "https://github.com/basvandijk/scientific";
    description = "Numbers represented using scientific notation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
