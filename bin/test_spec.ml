open Tjr_simple_earley
open Prelude
open Spec_common

module Internal = Earley_spec.Internal_with_inefficient_spec_state(A)
open Internal

open Spec_types

let main () = 
  let grammar = !Params.grammar in
  let initial_nt = grammar.initial_nt in
  let expand_nt,expand_tm = grammar_to_expand grammar in
  earley_spec ~expand_nt ~expand_tm ~initial_nt
  |> fun { count; items; _ } -> 
  Printf.printf "%d nt_items produced (%s)\n%!"
    count
    __FILE__;
  Log.log @@ lazy (    
    let filename = "/tmp/spec.items" in
    Lazy.force items
    |> filter_sort_items
    |> List.map (fun itm -> itm |> itm_to_string)
    |> String.concat "\n"
    |> fun text -> ExtLib.output_file ~filename ~text;
    Printf.printf "Spec items written to %s (%s)\n%!" filename __FILE__
  )

