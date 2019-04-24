(** This is almost a direct copy of test_spec *)
open Tjr_simple_earley
open Prelude
open Spec_common

module Internal = Earley_unstaged.Make(A)
open Internal

open Spec_types

let main () = 
  let grammar = !Params.grammar in
  let initial_nt = grammar.initial_nt in
  let expand_nt,expand_tm = grammar_to_expand grammar in
  earley_unstaged ~expand_nt ~expand_tm ~initial_nt
  |> fun { count; items; _ } -> 
  Printf.printf "%d nt_items produced (%s)\n%!"
    count __FILE__;
  Log.log @@ lazy (
    (* don't print if > 1000 items *)
    match count > 1000 with
    | true -> ()
    | false -> 
      let filename = "/tmp/unstaged.items" in
      Lazy.force items
      |> filter_sort_items
      |> List.map (fun itm -> itm |> itm_to_string)
      |> String.concat "\n"
      |> fun text -> ExtLib.output_file ~filename ~text;
      Printf.printf "Spec items written to %s (%s)\n%!" filename __FILE__
  );
  Log.log @@ lazy (profiler.print_summary ())
