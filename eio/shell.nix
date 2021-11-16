{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    opam
    pkgconfig
    curl
  ];
  shellHook = ''
    export OPAMROOT=$PWD/.opam-root
    eval $(opam env --switch=4.12.0+domains+effects)
  '';
}
