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
          Find the absolute path to vcpkg's libraries & headers.
          Unfortunately, %{project_root} in the dune file always returns a relative path and in order
          for -L to work properly, we need to give an absolute path.
          We rely on a simple python program (in vcpkg_cmd) to do so portably.
      *)
      let vcpkg_cmd =
        "python3 -c 'import os, sys; print(os.path.realpath(\"../../../../../"
        ^^ "%s" ^^ "\"))'"
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
      C.Flags.write_sexp "c_library_flags.sexp" clib_flags;
      C.Flags.write_sexp "library_flags.sexp" lflags)
