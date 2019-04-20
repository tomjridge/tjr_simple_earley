(** This is almost a direct copy of test_spec *)


module Make() = struct
  open Tjr_simple_earley
  open Spec_common

  module Internal = Earley_unstaged.Make(A)
  open Internal


  let _ = 
    earley_unstaged ~expand_nt ~expand_tm ~initial_nt:"E"
    |> Misc.rev_filter_map (function (Nt_item itm) -> Some itm | _ -> None)
    |> fun itms -> 
    let len_itms = List.length itms in
    let _ = 
      Printf.printf "%s: input length %d; %d nt_items produced\n%!"
        __FILE__
        !Params.input_length
        len_itms
    in
    (* don't print if > 1000 items *)
    match len_itms > 1000 with
    | true -> ()
    | false -> 
      itms
      |> List.sort (fun itm1 itm2 -> 
          let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
          Pervasives.compare (f itm1) (f itm2))
      |> List.iter (fun itm -> itm |> itm_to_string |> print_endline)

end
