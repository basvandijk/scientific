let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      scientific = self.callPackage ./scientific.nix {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.scientific.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
       ] ++ hs.scientific.propagatedNativeBuildInputs
         ++ hs.scientific.extraBuildInputs)))
     ];
   }
