(* FIXME use omacro latex_macros package *)

open Latex_macros

let _ = 
  Tjr_file.read_file "tjr_earley.md" |> fun s ->
  run_macros macros s |> print_endline

;;

