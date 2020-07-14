let
  pkgs = import <unstable> {};

in
  (pkgs.haskellPackages.callPackage ./default.nix {}).env
