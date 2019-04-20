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

let _E,_1,eps = Nt "E",Tm "1", Tm "eps"

let expand_nt (nt,i) =
  assert(nt="E");
  [ [_E;_E;_E]; [_1]; [eps] ]
  |> List.map (fun rhs -> { nt; i_=i;k_=i; bs=rhs })

let len = Sys.argv.(1) |> int_of_string

let input = String.make len '1'

let expand_tm (tm,i) = 
  assert(tm="1" || tm="eps");
  match tm with
  | _ when tm="1" -> (
      if i<String.length input && String.get input i = '1' then [i+1] else [])
  | _ when tm="eps" -> [i]

let sym_to_string = function Nt x -> x | Tm x -> x 
let syms_to_string xs = 
  String.concat ";" (List.map sym_to_string xs) 
  |> fun s -> "["^s^"]"
let itm_to_string {nt;i_;k_;bs} = Printf.sprintf "%s -> %3d,%3d,%s"
    nt
    i_
    k_
    (syms_to_string bs)
