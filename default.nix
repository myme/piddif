{ haskellPackages, nix-gitignore }:

let
  srcs = nix-gitignore.gitignoreSourcePure ./.gitignore ./.;

in
  haskellPackages.callCabal2nix "piddif" srcs {}
