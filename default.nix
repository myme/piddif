{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

in haskellPackages.mkDerivation {
  pname = "piddif";
  version = "0.1.0.0";
  src = gitignore ./.;
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
  testDepends = with haskellPackages; [
    hspec
  ];
  license = pkgs.stdenv.lib.licenses.mit;
}
