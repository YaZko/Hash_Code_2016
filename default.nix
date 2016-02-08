with import <nixpkgs> {};

stdenv.mkDerivation {

  name = "Hash2016";
  src = ./.;

  buildInputs = with ocamlPackages_4_01_0; [
    ocaml findlib merlin ocaml_batteries
  ];

}
