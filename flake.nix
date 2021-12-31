{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:

  let

    packages = nixpkgs.legacyPackages.x86_64-linux;
    derivation = packages.haskellPackages.callCabal2nix "agda-unused" self {};

  in

  {
    defaultPackage.x86_64-linux = derivation;

    devShell.x86_64-linux = derivation.env.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [
        packages.cabal-install
        packages.haskellPackages.Agda
      ];
    });
  };
}

