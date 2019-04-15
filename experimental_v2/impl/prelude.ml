type i_t = int  
type k_t = int
type j_t = int

type nt
type tm
type sym

type nt_item  

type nt_item_set

type item_ops = {
  sym_case: 'a. nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a;
  _NT: nt -> sym;
  dot_nt: nt_item -> nt;
  dot_i: nt_item -> i_t;
  dot_k: nt_item -> k_t;
  dot_bs: nt_item -> sym list;
  cut: nt_item -> j_t -> nt_item;
  elements : nt_item_set -> nt_item list
}

(* when we increase k, we need to alter the state significantly;
   this reveals the structure of the state *)

type todo_gt_k  (* int -> nt_item_set *)
type bitms_lt_k  (* int -> bitms_at_k *)
type bitms_at_k  (* nt -> nt_item_set *)

(* following are per k *)
type ixk_done  (* int*nt set *)
type ktjs  (* tm -> j list option *)

type state = {
  todo: nt_item list;
  todo_done: nt_item_set;
  todo_gt_k: todo_gt_k;
  bitms_lt_k: bitms_lt_k;
  bitms_at_k: bitms_at_k;
  ixk_done: ixk_done;
  ktjs:ktjs
}

type state_ops = {
  todo_gt_k_find: int -> todo_gt_k -> nt_item_set;
  update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k;
  empty_bitms_at_k: bitms_at_k;
  empty_ixk_done: ixk_done;
  empty_ktjs: ktjs;
}

(*
type 'a m
type monad_ops = {
  bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m;
  return: 'a. 'a -> 'a m
}
*)
open Tjr_monad.Imperative
type 'a m = ('a,imperative) Tjr_monad.Types.m
type monad_ops = {
  bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m;
  return: 'a. 'a -> 'a m
}


type atomic_operations = {
  get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
  get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
  add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
  pop_todo: unit -> nt_item option m;
  add_todos_at_k: nt_item list -> unit m;
  add_todos_gt_k: nt_item list -> unit m;
  add_ixk_done: int*nt -> unit m;
  mem_ixk_done: int*nt -> bool m;
  find_ktjs: tm -> int list option m;
  add_ktjs: tm -> int list -> unit m;
  with_state: (state -> state) -> unit m;
}
