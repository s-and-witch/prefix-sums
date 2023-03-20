# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override (old:
          { overrides = (self: super:
            { ghcWithPackages = super.ghcWithPackages.override (_:
              { useLLVM = true;
              });
            });
          });

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "prefix-sums";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {};

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = map (f: f.overrideAttrs (_: _:{useLLVM = true;})) (map (__getAttr "env") (__attrValues self.packages.${system}));
        };
        devShell = self.devShells.${system}.default;
      });
}
