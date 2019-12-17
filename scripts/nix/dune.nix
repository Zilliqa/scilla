{ stdenv, fetchurl, ocaml, findlib, opaline, ncurses }:

stdenv.mkDerivation rec {
  pname = "dune";
  version = "2.0.0";
  src = fetchurl {
    url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
    sha256 = "1xmkqiqg3rqqkr6qx1ajba9fxlqk4kzf4223zlv24nkpcf13p6cz";
  };

  buildInputs = [ ocaml findlib ncurses ];

  buildFlags = "release";

  dontAddPrefix = true;

  installPhase = ''
    runHook preInstall
    ${opaline}/bin/opaline -prefix $out -libdir $OCAMLFIND_DESTDIR
    runHook postInstall
  '';

  meta = {
    homepage = "https://dune.build/";
    description = "A composable build system";
    maintainers = [ stdenv.lib.maintainers.vbgl stdenv.lib.maintainers.marsam ];
    license = stdenv.lib.licenses.mit;
    inherit (ocaml.meta) platforms;
  };
}
