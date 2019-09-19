with import <nixpkgs> { };

let
  opkgs = pkgs.ocamlPackages;
  ppx_deriving_protobuf = opkgs.callPackage ./scripts/nix/ppx_deriving_protobuf.nix { };
  ocaml-protoc = opkgs.callPackage ./scripts/nix/ocaml-protoc.nix { inherit ppx_deriving_protobuf; };
  rpclib = opkgs.callPackage ./scripts/nix/rpclib.nix { };
  ppx_deriving_rpc = opkgs.callPackage ./scripts/nix/ppx_deriving_rpc.nix { inherit rpclib; };
  ocamlVersion = opkgs.ocaml.version;
  systemPkgs = [
    ocaml opam gcc dune ncurses boost openssl
    zlib secp256k1 libffi pkgconfig pcre patdiff
  ];
  ocamlPkgs = with opkgs; [
    base core core_bench core_profiler ppx_deriving ppx_tools_versioned
    ppx_sexp_conv bisect_ppx fileutils hex stdint batteries cryptokit
    bitstring ctypes findlib utop angstrom ounit expect_test_helpers patience_diff
    ocaml_pcre merlin ocp-indent ocp-index yojson menhir secp256k1 rpclib
    ppx_deriving_protobuf ocaml-protoc ppx_deriving_rpc result rresult xmlm
  ];
  packages = systemPkgs ++ ocamlPkgs;
  mkpath = p: "${p}/lib/ocaml/${ocamlVersion}/site-lib";
  paths = builtins.concatStringsSep ":" (map mkpath ocamlPkgs);
  siteLisp = "share/emacs/site-lisp";
in pkgs.mkShell {
  buildInputs = packages;
  shellHook = with opkgs; ''
    export OCAML_TOPLEVEL_PATH="${mkpath findlib}"
  '';
}
