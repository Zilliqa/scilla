module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
  let default : C.Pkg_config.package_conf =
    { libs   = []
    ; cflags = []
    }
  in
  let conf =
    match C.Pkg_config.get c with
    | None -> default
    | Some pc ->
        match (C.Pkg_config.query pc ~package:"openssl") with
        | None -> default
        | Some deps -> deps
  in

  (* This file runs in _build/default/src/cpp. 
   * libff is installed in _build/libff/install. *)
  let libff_dir = Sys.getcwd() ^ "/../../../libff/install" in
  let libff_include_dir = libff_dir ^ "/include" in
  let libff_lib_dir = libff_dir ^ "/lib" in
  if not (Sys.file_exists libff_include_dir) || not (Sys.file_exists libff_lib_dir)
  then
    (Printf.eprintf "Not found: libff. Please run scripts/libff.sh";
    exit 1)
  else
    let cflags' = conf.cflags @ ["-I"; libff_include_dir] in
    let libs' = conf.libs @ ["-L" ^ libff_lib_dir] in
    C.Flags.write_sexp "c_flags.sexp"         cflags';
    C.Flags.write_sexp "c_library_flags.sexp" libs'
  )
