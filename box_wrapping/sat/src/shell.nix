let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ (pkgs.haskell.lib.dontCheck mios) vector mtl lens ]))
  ];
}
