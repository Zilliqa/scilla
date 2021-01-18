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

      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs;
      C.Flags.write_sexp "library_flags.sexp" lflags)
