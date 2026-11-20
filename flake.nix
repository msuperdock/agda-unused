{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }:

  let

    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    agda-unused = pkgs.haskellPackages.callCabal2nix "agda-unused" self { };

    packages = agda-unused.env.nativeBuildInputs ++ [
      pkgs.cabal-install
      pkgs.haskellPackages.Agda
    ];

  in

  {
    packages.${system}.default = agda-unused;

    devShells.${system}.default = pkgs.mkShell {
      inherit packages;
    };

    # jail.nix permissions; see https://git.sr.ht/~alexdavid/jail.nix
    jail = combinators: [
      (combinators.add-pkg-deps packages)
    ];
  };
}
