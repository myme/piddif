{
  description = "Piddif nix flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in {
      overlay = (final: prev: {
        piddif = pkgs.haskell.lib.compose.justStaticExecutables
          final.haskellPackages.piddif;
        haskellPackages = prev.haskellPackages // {
          piddif =
            let srcs = prev.nix-gitignore.gitignoreSourcePure ./.gitignore ./.;
            in prev.haskellPackages.callCabal2nix "piddif" srcs { };
        };
      });

      packages.${system} = {
        default = pkgs.piddif;
        image = pkgs.dockerTools.buildLayeredImage {
          name = "piddif";
          tag = "latest";
          contents = pkgs.piddif;
          config = {
            Cmd = [ "piddif-server" "--host" "0.0.0.0" "--port" "8000" ];
            ExposedPorts = { "8000/tcp" = { }; };
          };
        };
      };

      devShell.${system} = pkgs.haskellPackages.shellFor {
        withHoogle = true;
        packages = _: [ pkgs.haskellPackages.piddif ];
        buildInputs =
          (with pkgs; [ cabal-install haskell-language-server hlint ])
          ++ (with pkgs.haskellPackages; [ ghcid ormolu ]);
      };
    };
}
