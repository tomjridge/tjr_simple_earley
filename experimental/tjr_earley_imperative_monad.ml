(* an experiment to see whether the imperative code (represented using
   a monad) is easier to read; probably it is *)

module type M_ = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
end


module type S_ = sig  
  type i_t = int  
  type k_t = int
  type j_t = int

  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  type nt_item  

  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> i_t
  val dot_k: nt_item -> k_t
  val dot_bs: nt_item -> sym list

  val cut: nt_item -> j_t -> nt_item

  type nt_item_set
  val elements : nt_item_set -> nt_item list
end


module type STATE = sig  
  type nt_item
  type nt_item_set

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

  val todo_gt_k_find: int -> todo_gt_k -> nt_item_set
  val update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k
  val empty_bitms_at_k: bitms_at_k
  val empty_ixk_done: ixk_done
  val empty_ktjs: ktjs
end


module Make = functor 
  (S:S_)
  (State:STATE with type nt_item = S.nt_item and type nt_item_set = S.nt_item_set)
  (M:M_) -> 
struct
  (*:np:*)
  open S
  open M

  type atomic_operations = {
    get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
    get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
    add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
    pop_todo: unit -> nt_item option m;
    add_todos: nt_item list -> unit m;
    add_ixk_done: int*nt -> unit m;
    mem_ixk_done: int*nt -> bool m;
    find_ktjs: tm -> int list option m;
    add_ktjs: tm -> int list -> unit m;
    with_state: (State.state -> State.state) -> unit m;
  }

  let is_finished nitm = nitm|>dot_bs = [] 


  let run_earley ~at_ops ~new_items ~input ~parse_tm ~input_length = 
    begin

      let { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo; add_todos; add_ixk_done; mem_ixk_done; find_ktjs; add_ktjs; with_state } = at_ops in

      let step_at_k k nitm = 
        let get_bitms (i,x) =
          if i=k then get_bitms_at_k x else
            get_bitms_lt_k (i,x)
        in
       
        match is_finished nitm with
        | true -> (
            let (i,x) = (nitm|>dot_i,nitm|>dot_nt) in
            mem_ixk_done (i,x) >>= fun already_done ->
            match already_done with
            | true -> return ()
            | false -> (
                add_ixk_done (i,x) >>= fun _ ->
                get_bitms (i,x) >>= fun bitms ->
                add_todos (List.map (fun bitm -> cut bitm k) bitms)))
        | false -> (
            let bitm = nitm in
            let s = List.hd (bitm|>dot_bs) in
            s |> sym_case
              ~nt:(fun _Y -> 
                  get_bitms_at_k _Y >>= fun bitms ->
                  let bitms_empty = bitms=[] in
                  add_bitm_at_k bitm _Y >>= fun _ ->
                  match bitms_empty with
                  | false -> (
                      mem_ixk_done (k,_Y) >>= function
                      | true -> add_todos [cut bitm k]
                      | false -> return ())
                  | true -> (
                      let itms = new_items ~nt:_Y ~input ~k in
                      add_todos itms))
              ~tm:(fun tm ->
                  find_ktjs tm >>= fun ktjs ->
                  (match ktjs with 
                   | None -> 
                     (* we need to process kT *)
                     let js = parse_tm ~tm ~input ~k ~input_length in
                     add_ktjs tm js >>= fun _ ->
                     return js
                   | Some js -> return js) >>= fun js ->
                  add_todos (List.map (fun j -> cut bitm j) js)))
      in


      (* FIXME monad syntax may make this easier to read *)
      let rec loop_at_k k = 
        pop_todo () >>= function
        | None -> return ()
        | Some itm -> step_at_k k itm >>= fun _ -> loop_at_k k
      in

      let rec loop k = 
        match k > input_length with  
        (* correct? FIXME don't we have to go one further? *)
        | true -> return ()
        | false -> 
          (* process items *)
          loop_at_k k >>= fun _ ->
          let k' = k+1 in
          (* 
           todo and todo_done are updated with todo_gt_k[k'];
           bitms_lt_k is updated: bitms_lt_k[k]=bitms_at_k
           bitms_at_k is reset;
           ixk_done and ktjs are reset *)
          let open State in
          with_state (fun s ->
              let todo' = todo_gt_k_find k' s.todo_gt_k in
              { todo=elements todo';
                todo_done=todo';
                todo_gt_k=s.todo_gt_k;
                bitms_lt_k=(update_bitms_lt_k k s.bitms_at_k s.bitms_lt_k);
                bitms_at_k=empty_bitms_at_k;
                ixk_done=empty_ixk_done;
                ktjs=empty_ktjs;
              }) >>= fun _ ->
          loop k'
      in


      loop 0
    end (* run_earley *)

end


(* example instance ------------------------------------------------- *)

open Tjr_monad
open Tjr_monad.Monad


module M = struct
  open Tjr_monad.Imperative_instance
  type 'a m = ('a,imperative) Monad.m
  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind  
end

module X1 = (M:M_)

(* Implement the signature required by the Earley code *)
module S = struct
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

  (* Implement the accessor functions by using simple arithmetic *)
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
end  

module X2 = (S:S_)


module State = struct
  type nt_item = S.nt_item
  type nt_item_set = S.nt_item_set

  module Todo_gt_k = Tjr_map.Map_int
  type todo_gt_k = nt_item_set Todo_gt_k.t

  module Bitms_at_k = Map.Make(
    struct 
      type t = S.nt
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
      type t = int * S.nt 
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type ixk_done = Ixk_done.t

  module Ktjs = Map.Make(
    struct
      type t = S.tm                   
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type ktjs = S.j_t list option Ktjs.t

  type state = {
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: todo_gt_k;
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    ixk_done: ixk_done;
    ktjs:ktjs
  }


  let todo_gt_k_find k t = 
    try 
      Todo_gt_k.find k t
    with _ -> S.Set_nt_item.empty

  let update_bitms_lt_k : int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k =
    fun i at lt ->
    Bitms_lt_k.add i at lt

  let empty_bitms_at_k = Bitms_at_k.empty

  let empty_ixk_done = Ixk_done.empty

  let empty_ktjs = Ktjs.empty      
end

module X3 = (State:STATE)

module Earley = Make (S)(State)(M)

open M
open S
open State

open Earley

let to_m = Imperative_instance.to_m
let run_earley ~init_state =
  let s = ref init_state in
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
  let add_todos itms = 
    to_m (
      s:={!s with todo=itms@ !s.todo};
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
    add_todos;
    add_ixk_done;
    mem_ixk_done;
    find_ktjs;
    add_ktjs;
    with_state
  }
  in

  run_earley ~at_ops

let _ :
init_state:State.state ->
new_items:(nt:S.nt -> input:'a -> k:S.j_t -> S.nt_item list) ->
input:'a ->
parse_tm:(tm:S.tm -> input:'a -> k:S.j_t -> input_length:S.j_t -> S.j_t list) ->
input_length:S.j_t -> unit M.m
= run_earley


