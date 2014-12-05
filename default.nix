let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./scientific.nix {}
