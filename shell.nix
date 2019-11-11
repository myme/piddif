{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
  drv ? pkgs.callPackage ./default.nix {},
}:
haskellPackages.shellFor {
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal2nix
    cabal-install
    hlint
  ]) ++ (with haskellPackages; [
    ghcid
  ]);
}
