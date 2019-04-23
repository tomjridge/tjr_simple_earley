open Tjr_simple_earley
open Spec_common

module Internal = Earley_spec.Make(A)
open Internal

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
    |> Misc.rev_filter_map (function
        | Nt_item x -> Some x
        | _ -> None)
    |> List.sort (fun itm1 itm2 -> 
        let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
        Pervasives.compare (f itm1) (f itm2))
    |> List.iter (fun itm -> itm |> itm_to_string |> print_endline))

