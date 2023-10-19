{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

in
    haskellPackages.shellFor {
      packages = hpkgs: [
        (hpkgs.callPackage (import ./scientific.nix) {})
      ];
      nativeBuildInputs = [
        pkgs.haskell-language-server
        pkgs.cabal-install
      ];
    }
