open List

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
            | Some deps -> deps)
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

      (* 
         Find the absolute path to vcpkg's library.
         Unfortunately, %{project_root} in the dune file always returns a relative path and in order
         for -L to work properly, we need to give an absolute path.
         The 'pwd' below returns a sub-directory of the _build directory so currently, we rely
         on the 'realpath' utility & and the build directory location.
      *)
      let ic = Unix.open_process_in "pwd" in
      let cmd = "realpath " ^ input_line ic ^ "/../../../../../vcpkg_installed/x64-linux-dynamic/lib" in
      let ic = Unix.open_process_in cmd in
      let vcpkg_lib_dir = input_line ic in
      let clib_flags = List.append conf.libs [ "-L" ^ vcpkg_lib_dir ]

      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" clib_flags;
      C.Flags.write_sexp "library_flags.sexp" lflags)
