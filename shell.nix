{ compiler ? "4.06" }:

with import <nixpkgs> { };

let
  ver = with lib.strings; concatStringsSep "_" (splitString "." compiler);
  opkgs = pkgs.ocaml-ng."ocamlPackages_${ver}";
  ppx_deriving_protobuf = opkgs.callPackage ./scripts/nix/ppx_deriving_protobuf.nix { };
  ocaml-protoc = opkgs.callPackage ./scripts/nix/ocaml-protoc.nix { inherit ppx_deriving_protobuf; };
  rpclib = opkgs.callPackage ./scripts/nix/rpclib.nix { };
  ppx_deriving_rpc = opkgs.callPackage ./scripts/nix/ppx_deriving_rpc.nix { inherit rpclib; };
  systemPkgs = [
    m4 cmake gmp procps opam gcc ncurses boost openssl
    zlib secp256k1 libffi pkgconfig pcre patdiff
  ];
  ocamlPkgs = with opkgs; [
    ocaml dune ocamlbuild findlib utop merlin ocp-indent ocp-index
    base core core_bench
    textutils ppx_sexp_conv
    ppx_deriving ppx_tools_versioned
    bisect_ppx fileutils hex stdint batteries zarith cryptokit atdgen
    bitstring ctypes angstrom ounit expect_test_helpers patience_diff
    ocaml_pcre yojson menhir secp256k1 rpclib re2 ppx_let
    ppx_deriving_protobuf ocaml-protoc ppx_deriving_rpc result rresult xmlm
  ];
  packages = systemPkgs ++ ocamlPkgs;
  mkpath = p: "${p}/lib/ocaml/${opkgs.ocaml.version}/site-lib";
  paths = builtins.concatStringsSep ":" (map mkpath ocamlPkgs);
  siteLisp = "share/emacs/site-lisp";
in
with opkgs; pkgs.mkShell {
  buildInputs = packages;
  shellHook = with opkgs; ''
    export OCAML_TOPLEVEL_PATH="${mkpath findlib}"
    export UTOP_SITE_LISP="${utop}/${siteLisp}";
    export MERLIN_SITE_LISP="${merlin}/${siteLisp}";
    export OCP_INDENT_SITE_LISP="${ocp-indent}/${siteLisp}";
  '';
}
