(** This is almost a direct copy of test_spec *)


module Make() = struct
  open Tjr_simple_earley
  open Spec_common

  module Internal = Earley_unstaged.Make(A)
  open Internal

  let grammar = !Params.grammar

  let initial_nt = grammar.initial_nt

  let expand_nt,expand_tm = grammar_to_expand grammar



  let _ = 
    (* Earley_unstaged._mark_ref := profiler.mark; *)
    earley_unstaged ~expand_nt ~expand_tm ~initial_nt
    |> Misc.rev_filter_map (function (Nt_item itm) -> Some itm | _ -> None)
    |> fun itms -> 
    let len_itms = List.length itms in
    Log.log @@ lazy (
      Printf.printf "%s: %d nt_items produced\n%!"
        __FILE__
        len_itms);
    begin
      (* don't print if > 1000 items *)
      match len_itms > 1000 with
      | true -> ()
      | false -> 
        Log.log @@ lazy (
          itms
          |> List.sort (fun itm1 itm2 -> 
              let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
              Pervasives.compare (f itm1) (f itm2))
          |> List.iter (fun itm -> itm |> itm_to_string |> print_endline))
    end;
    Log.log @@ lazy (profiler.print_summary ())

end
