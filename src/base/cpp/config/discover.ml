module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let default : C.Pkg_config.package_conf = { libs = []; cflags = [] } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc -> (
            match C.Pkg_config.query pc ~package:"openssl" with
            | None -> default
            | Some deps -> deps )
      in

      let lflags =
        if Sys.os_type = "Unix" then
          let ic = Unix.open_process_in "uname" in
          let uname = input_line ic in
          let () = close_in ic in
          (* macOS requires -keep_dwarf_unwind for exceptions to work. *)
          if uname = "Darwin" then [ "-cclib"; "-Wl,-keep_dwarf_unwind" ]
          else []
        else []
      in

      (* This file runs in _build/default/src/base/cpp.
       * libff is installed in deps/libff/install. *)
      let libff_dir = Sys.getcwd () ^ "/../../../../../deps/libff/install" in
      let libff_include_dir = libff_dir ^ "/include" in
      let libff_lib_dir = libff_dir ^ "/lib" in
      if
        (not (Sys.file_exists libff_include_dir))
        || not (Sys.file_exists libff_lib_dir)
      then (
        Printf.eprintf "Not found: libff. Please run scripts/libff.sh";
        exit 1 )
      else
        let cflags' = conf.cflags @ [ "-I"; libff_include_dir ] in
        let libs' = conf.libs @ [ "-L" ^ libff_lib_dir ] in
        C.Flags.write_sexp "c_flags.sexp" cflags';
        C.Flags.write_sexp "c_library_flags.sexp" libs';
        C.Flags.write_sexp "library_flags.sexp" lflags)
