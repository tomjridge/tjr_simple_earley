(** Simple test of Earley functionality. *)

open Tjr_simple_earley
open Tjr_simple_earley.Prelude
open Spec_types
open Profiler

(* simple test ------------------------------------------------------ *)

module Base_types = struct
  type nt = string
  type tm = string
end

module Internal = Earley_simple.Make(Base_types)
open Internal

let main () = 
  (* Example input; use command line argument *)
  let input = !Params.input in
  let input_length = String.length input in
  let grammar = !Params.grammar in
  let grammar_etc = Examples.get_grammar_etc_by_name ~name:grammar ~input ~input_length in
  let initial_nt = (Examples.get_grammar_by_name grammar).initial_nt in
  run_earley_parser
    ~grammar_etc
    ~initial_nt
  |> fun state -> 
  Printf.printf "%d nt_items produced (%s)\n%!" state.count __FILE__;
  Log.log @@ lazy (profiler.print_summary ())

