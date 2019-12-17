{ stdenv, ocaml, findlib, dune }:

stdenv.mkDerivation rec {
  pname = "dune-configurator";

  inherit (dune) version src buildInputs buildFlags dontAddPrefix installPhase;

  meta = {
    homepage = "https://dune.build/";
    description = "Helper library for gathering system configuration.";
    maintainers = [ stdenv.lib.maintainers.vbgl stdenv.lib.maintainers.marsam ];
    license = stdenv.lib.licenses.mit;
    inherit (ocaml.meta) platforms;
  };
}
