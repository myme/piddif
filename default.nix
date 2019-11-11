{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:
haskellPackages.mkDerivation {
  pname = "piddif";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    blaze-html
    blaze-markup
    file-embed
    optparse-applicative
    pandoc
    process
    temporary
    text
  ];
  license = pkgs.stdenv.lib.licenses.mit;
}
