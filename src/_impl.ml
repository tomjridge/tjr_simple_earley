(** Implement types for arguments to Earley.Make functor *)

open Tjr_monad.Imperative
type 'a m = ('a,imperative) Tjr_monad.Types.m
let return = monad_ops.return
let bind = monad_ops.bind  

type i_t = int
type k_t = int
type j_t = int

(* NOTE we need to be able to distinguish nonterminals from terminals *)
type nt = int (* even, say *)
type tm = int (* odd, say *)
type sym = int
let even x = (x mod 2 = 0)
let sym_case ~nt ~tm sym = 
  if even sym then nt sym else tm sym

let _NT: nt -> sym = fun x -> x

type nt_item = { nt:nt; i_:i_t; k_:k_t; bs:sym list }


let dot_nt nitm = nitm.nt
let dot_i nitm = nitm.i_
let dot_k nitm = nitm.k_
let dot_bs nitm = nitm.bs

let cut : nt_item -> j_t -> nt_item = 
  fun bitm j0 -> 
  { bitm with k_=j0; bs=(List.tl bitm.bs)}
type cut = nt_item -> j_t -> nt_item


type nt_item_ops = {
  dot_nt: nt_item -> nt;
  dot_i: nt_item -> i_t;
  dot_k: nt_item -> k_t;
  dot_bs: nt_item -> sym list;
}

module Set_nt_item = Set.Make(
  struct 
    type t = nt_item 
    let compare : t -> t -> int = Pervasives.compare 
  end)
type nt_item_set = Set_nt_item.t
let elements = Set_nt_item.elements


module Todo_gt_k = Tjr_map.Map_int
type todo_gt_k = nt_item_set Todo_gt_k.t

module Bitms_at_k = Map.Make(
  struct 
    type t = nt
    let compare: t -> t -> int = Pervasives.compare 
  end)
type bitms_at_k = nt_item_set Bitms_at_k.t

module Bitms_lt_k = Map.Make(
  struct 
    type t = int 
    let compare: t -> t -> int = Pervasives.compare 
  end)
type bitms_lt_k = bitms_at_k Bitms_lt_k.t

module Ixk_done = Set.Make(
  struct 
    type t = int * nt 
    let compare: t -> t -> int = Pervasives.compare 
  end)
type ixk_done = Ixk_done.t

module Ktjs = Map.Make(
  struct
    type t = tm                   
    let compare: t -> t -> int = Pervasives.compare 
  end)
type ktjs = j_t list option Ktjs.t

(*
type state = {
  todo: nt_item list;
  todo_done: nt_item_set;
  todo_gt_k: todo_gt_k;
  bitms_lt_k: bitms_lt_k;
  bitms_at_k: bitms_at_k;
  ixk_done: ixk_done;
  ktjs:ktjs
}
*)

let todo_gt_k_find k t = 
  try 
    Todo_gt_k.find k t
  with _ -> Set_nt_item.empty

let update_bitms_lt_k : int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k =
  fun i at lt ->
  Bitms_lt_k.add i at lt

let empty_bitms_at_k = Bitms_at_k.empty

let empty_ixk_done = Ixk_done.empty

let empty_ktjs = Ktjs.empty      


module Earley_ = Earley.Make(struct 
    type i_t = int
    type k_t = int
    type j_t = int
    type nonrec nt = nt
    type nonrec tm = tm
    type nonrec sym = sym
    type nonrec nt_item = nt_item
    type nonrec nt_item_set = nt_item_set
    type nonrec todo_gt_k = todo_gt_k
    type nonrec bitms_lt_k = bitms_lt_k
    type nonrec bitms_at_k = bitms_at_k
    type nonrec ixk_done = ixk_done
    type nonrec ktjs = ktjs
    type nonrec 'a m = 'a m
  end)

let make_earley_parser,run_earley_parser = Earley_.(make_earley_parser,run_earley_parser)

let _ = make_earley_parser
open Earley_

let to_m = Tjr_monad.Imperative.to_m

let make_atomic_ops ~state = 
  let s = state in
  let get_bitms_at_k = fun nt -> 
    to_m (
      try
        Bitms_at_k.find nt !s.bitms_at_k |> elements
      with _ -> [])
  in
  let get_bitms_lt_k = fun (i,x) ->
    (
      let bitms = 
        try
          Bitms_lt_k.find i !s.bitms_lt_k
        with _ -> Bitms_at_k.empty
      in
      try 
        Bitms_at_k.find x bitms
      with _ -> Set_nt_item.empty)
    |> elements
    |> to_m
  in
  let add_bitm_at_k = fun itm nt ->
    to_m (
      let s_ = !s in
      let itms = 
        try Bitms_at_k.find nt s_.bitms_at_k with _ -> Set_nt_item.empty
      in
      let itms = Set_nt_item.add itm itms in
      s:={s_ with bitms_at_k=Bitms_at_k.add nt itms s_.bitms_at_k};
      ())      
  in
  let pop_todo = fun () ->
    to_m (
      match !s.todo with
      | [] -> None
      | x::xs -> 
        s:={!s with todo=xs};
        Some x)
  in
  let add_todos_at_k itms = 
    to_m (
      let todo_done = !s.todo_done in
      let itms = itms |> List.filter (fun itm -> not (Set_nt_item.mem itm todo_done)) in
      s:={!s with todo=itms@ !s.todo; 
                  todo_done=Set_nt_item.union (Set_nt_item.of_list itms) (!s.todo_done)};
      ())
  in
  let add_todos_gt_k itms = 
    to_m (
      itms |> List.iter (fun itm ->
          let todo_gt_k = !s.todo_gt_k in
          let k' = itm.k_ in 
          let set = 
            try 
              Todo_gt_k.find k' todo_gt_k
            with _ -> Set_nt_item.empty
          in
          let set = Set_nt_item.add itm set in
          let todo_gt_k = Todo_gt_k.add k' set todo_gt_k in
          s:={!s with todo_gt_k});
      ())
  in
  let add_ixk_done (i,x) =
    to_m (
      s:={!s with ixk_done=Ixk_done.add (i,x) !s.ixk_done};
      ())
  in
  let mem_ixk_done (i,x) =
    to_m (
      Ixk_done.mem (i,x) !s.ixk_done)
  in
  let find_ktjs tm = 
    to_m (
      try
        Ktjs.find tm !s.ktjs
      with _ -> None)
  in
  let add_ktjs tm ints = 
    to_m (
      s:={!s with ktjs=Ktjs.add tm (Some ints) !s.ktjs})
  in
  let with_state f =
    to_m (
      s:=f !s)
  in
  let at_ops = {
    get_bitms_at_k;
    get_bitms_lt_k;
    add_bitm_at_k;
    pop_todo;
    add_todos_at_k;
    add_todos_gt_k;
    add_ixk_done;
    mem_ixk_done;
    find_ktjs;
    add_ktjs;
    with_state
  }
  in 
  at_ops

let earley_parser ~state = 
  make_earley_parser
    ~monad_ops:{bind; return}
    ~item_ops:{
      sym_case;
      _NT;
      dot_nt;
      dot_i;
      dot_k;
      dot_bs;
      cut;
      elements}
    ~state_ops:{
      todo_gt_k_find;
      update_bitms_lt_k;
      empty_bitms_at_k;
      empty_ixk_done;
      empty_ktjs}
    ~at_ops:(make_atomic_ops ~state)


let empty_state = {
  todo=[];
  todo_done=Set_nt_item.empty;
  todo_gt_k=Todo_gt_k.empty;
  bitms_lt_k=Bitms_lt_k.empty;
  bitms_at_k=empty_bitms_at_k;
  ixk_done=empty_ixk_done;
  ktjs=empty_ktjs
}

let run_earley_parser ~state = 
  earley_parser ~state |> fun earley_parser -> run_earley_parser ~earley_parser

let _ :
state:Earley_.state ref ->
new_items:(nt:sym -> input:'a -> k:sym -> nt_item list) ->
input:'a ->
parse_tm:(tm:sym -> input:'a -> k:sym -> input_length:sym -> sym list) ->
input_length:sym -> unit m
= run_earley_parser

