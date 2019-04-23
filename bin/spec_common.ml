(** Factor out common code from test_spec.ml and test_unstaged.ml *)

open Tjr_simple_earley

module A = struct

  type nt = string
  type tm = string

  type sym = Nt of nt | Tm of tm

  type nt_item = { nt:nt; i_:int; k_:int; bs: sym list }

  type sym_item = { i_:int; sym:sym; j_:int }

  type sym_at_k = { sym:sym; k_:int } 

  type item = 
    | Nt_item of nt_item
    | Sym_item of sym_item
    | Sym_at_k of sym_at_k

(*
  let tbl = Hashtbl.create 100

  let note_item (itm:nt_item) : unit = 
    Hashtbl.replace tbl itm ()
*)

end
include A

(*
let _E,_1,eps = Nt "E",Tm "1", Tm "eps"

let expand_nt (nt,i) =
  assert(nt="E");
  [ [_E;_E;_E]; [_1]; [eps] ]
  |> List.map (fun rhs -> { nt; i_=i;k_=i; bs=rhs })

*)

let sym_to_string = function Nt x -> x | Tm x -> x 
let syms_to_string xs = 
  String.concat ";" (List.map sym_to_string xs) 
  |> fun s -> "["^s^"]"
let itm_to_string {nt;i_;k_;bs} = Printf.sprintf "%s -> %3d,%3d,%s"
    nt
    i_
    k_
    (syms_to_string bs)

let grammar_to_expand grammar = 
  let grammar = 
    let to_sym s = if Examples.is_nt s then Nt s else Tm s in
    let Examples.{ rules; _ } = grammar in
    rules |> List.map (fun (nt,rhs) -> (nt,List.map to_sym rhs))
  in
  let expand_nt (nt,i) =
    grammar |> Misc.rev_filter_map (fun (nt',bs) -> match nt'=nt with
        | true -> Some { nt; i_=i;k_=i; bs }
        | false -> None)
  in
  let expand_tm (tm,i) = 
    let input = !Params.input in
    match Misc.string_matches_at ~string:input ~sub:tm ~pos:i with
    | true -> [i+(String.length tm)]
    | false -> []
  in
  (expand_nt,expand_tm)



let now =
  Core.(fun () ->
      Time_stamp_counter.(now () |> to_int63)
      |>Int63.to_int_exn)

let profiler = Tjr_profile.make_string_profiler ~now

(* let _ = Earley_spec._mark_ref := profiler.mark *)

