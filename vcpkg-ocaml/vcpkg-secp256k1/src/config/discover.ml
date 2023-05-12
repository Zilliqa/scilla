open Base
open Stdio
module C = Configurator.V1

let () =
  C.main ~name:"secp256k1" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lsecp256k1"; "-lsecp256k1_precomputed"]
      ; cflags = []
      }
    in
    let conf = default
    in
    let vcpkg_cmd =
      "python3 -c 'import os, sys; print(os.path.realpath(os.environ[\"SCILLA_REPO_ROOT\"]" ^^ "+\"/%s\"))'"
    in
    let vcpkg_triplet_path =
      input_line
        (Unix.open_process_in
           (Printf.sprintf vcpkg_cmd "scripts/vcpkg_triplet.sh"))
    in
    let vcpkg_triplet =
      input_line (Unix.open_process_in vcpkg_triplet_path)
    in
    let vcpkg_include_dir =
      input_line
        (Unix.open_process_in
           (Printf.sprintf vcpkg_cmd
              ("vcpkg_installed/" ^ vcpkg_triplet ^ "/include")))
    in
    let vcpkg_lib_dir =
      input_line
        (Unix.open_process_in
           (Printf.sprintf vcpkg_cmd
              ("vcpkg_installed/" ^ vcpkg_triplet ^ "/lib")))
    in
    let c_flags = List.append conf.cflags [ "-I" ^ vcpkg_include_dir ] in
    let clib_flags = List.append conf.libs [ "-L" ^ vcpkg_lib_dir ] in

    C.Flags.write_sexp "c_flags.sexp" c_flags;
    C.Flags.write_sexp "c_library_flags.sexp" clib_flags)

