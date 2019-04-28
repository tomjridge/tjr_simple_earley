(** Simple test of Earley functionality. *)

open Tjr_simple_earley
open Tjr_simple_earley.Prelude
(* open Spec_types *)
open Profiler

(* simple test ------------------------------------------------------ *)

module Base_types = struct
  type nt = string
  type tm = string
end

module Internal = Earley_simple.Make(Base_types)
open Internal
open Internal.Derived_types

let main () = 
  (* Example input; use command line argument *)
  let input = !Params.input in
  let input_length = String.length input in
  let grammar = !Params.grammar in
  let grammar_etc = 
    Examples.get_grammar_by_name grammar |> fun g ->
    let tbl = Hashtbl.create 100 in
    g.rules |> List.iter (fun (nt,rhs) -> 
        let rhss = Hashtbl.find_opt tbl nt |> function
          | None -> (Hashtbl.add tbl nt []; [])
          | Some rhss -> rhss
        in
        Hashtbl.replace tbl nt (rhs::rhss));
    Hashtbl.filter_map_inplace (fun _ rhss -> Some(List.rev rhss)) tbl;
    let new_items ~nt ~input ~pos = Hashtbl.find_opt tbl nt |> function
      | None -> []
      | Some rhss -> rhss |> List.map (fun rhs -> {nt;i_=pos;k_=pos;bs=rhs})
    in
    let parse_tm ~tm ~input ~pos ~input_length = 
        match Misc.string_matches_at ~string:input ~sub:tm ~pos with
        | true -> [pos+(String.length tm)]
        | false -> []
    in
    { new_items; parse_tm; input; input_length }
  in
  let initial_nt = (Examples.get_grammar_by_name grammar).initial_nt in
  run_earley_parser
    ~grammar_etc
    ~initial_nt
  |> fun state -> 
  Printf.printf "%d nt_items produced (%s)\n%!" state.count __FILE__;
  Log.log @@ lazy (profiler.print_summary ())

