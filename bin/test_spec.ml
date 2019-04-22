module Make() = struct

  open Tjr_simple_earley
  open Spec_common

  module Internal = Earley_spec.Make(A)
  open Internal

  let grammar = !Params.grammar

  let initial_nt = grammar.initial_nt

  let expand_nt,expand_tm = grammar_to_expand grammar

  let _ = 
    earley_spec ~expand_nt ~expand_tm ~initial_nt
    |> Misc.rev_filter_map (function (Nt_item itm) -> Some itm | _ -> None)
    |> fun itms -> 
    let _ = 
      Printf.printf "%s: input length %d; %d nt_items produced\n%!"
        __FILE__
        !Params.input_length
        (List.length itms)
    in
    itms
    |> List.sort (fun itm1 itm2 -> 
        let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
        Pervasives.compare (f itm1) (f itm2))
    |> List.iter (fun itm -> itm |> itm_to_string |> print_endline)

end
