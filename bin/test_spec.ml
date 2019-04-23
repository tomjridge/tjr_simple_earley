open Tjr_simple_earley
open Prelude
open Spec_common

module Internal = Earley_spec.Make(A)
open Internal

open Spec_types

let main () = 
  let grammar = !Params.grammar in
  let initial_nt = grammar.initial_nt in
  let expand_nt,expand_tm = grammar_to_expand grammar in
  earley_spec ~expand_nt ~expand_tm ~initial_nt
  |> fun { count; items; _ } -> 
  Log.log @@ lazy (
    Printf.printf "%s: %d nt_items produced\n%!"
      __FILE__
      count);
  Log.log @@ lazy (
    Lazy.force items
    |> filter_sort_items
    |> List.iter (fun itm -> itm |> itm_to_string |> print_endline))

