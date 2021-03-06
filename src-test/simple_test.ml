(** Simple test of Earley functionality. *)


(* simple test ------------------------------------------------------ *)

module Base_types = struct
  type nt = string
  type tm = string
end

module Internal = Earley_simple.Make(Base_types)
open Internal
(* open Internal.Derived_types *)

let main () = 
  (* Example input; use command line argument *)
  let input = !Test_params.input in
  let input_length = String.length input in
  let grammar = !Test_params.grammar in
  let g1,initial_nt = 
    Examples_without_actions.get_grammar_by_name grammar in
  let g2 = g1 |> Earley_intf.simple_to_input_dependent_grammar in
  let parse_tm ~tm ~input ~pos = 
    match Misc.string_matches_at ~string:input.input ~sub:tm ~pos with
    | true -> [pos+(String.length tm)]
    | false -> []
  in
  run_earley_parser
    ~grammar:g2
    ~parse_tm:{parse_tm}
    ~input:{input;input_length}
    ~initial_nt
  |> fun state -> 
  Printf.printf "%d nt_items produced (%s)\n%!" state.count __FILE__
  (* Prelude.log @@ lazy (profiler.print_summary ()) FIXME *)

