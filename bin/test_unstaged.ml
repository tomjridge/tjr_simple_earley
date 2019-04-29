(** This is almost a direct copy of test_spec *)
open Tjr_simple_earley
open Prelude
open Spec_common

module Internal = Earley_unstaged.Make(Nt_tm)
open Internal

let main () = 
  let grammar,initial_nt = Examples.get_grammar_by_name !Params.grammar in
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
      |> Misc.rev_filter_map (function 
        |Internal.Extended_items.Nt_item itm -> Some itm 
        | _ -> None)
      |> List.sort (fun itm1 itm2 -> 
        let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
        Pervasives.compare (f itm1) (f itm2))
      |> List.map (fun itm -> itm |> itm_to_string)
      |> String.concat "\n"
      |> fun text -> ExtLib.output_file ~filename ~text;
      Printf.printf "Spec items written to %s (%s)\n%!" filename __FILE__
  );
  Log.log @@ lazy (profiler.print_summary ())
