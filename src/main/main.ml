open Printf

let () =
  let foo z =
    for i = 1 to Array.length Sys.argv - 1 do
      printf "[%i] %d\n" i (Sys.argv.(i) |> int_of_string)
    done 
  in foo 5

 
