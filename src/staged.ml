(* TODO:

   use imperative hashmaps as set and map implementations *)

module X = functor (S:
sig

type i_t
type k_t = int
type j_t = int
type nt
type tm
type sym
type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;
    k: k_t;
    bs: sym list
  }

type nt_item_set

type 'a map_nt
type 'a map_int
type 'a map_tm

type 'a map_nt_ops = {
  find: nt -> 'a map_nt -> 'a;
  add: nt -> 'a -> 'a map_nt -> 'a map_nt;
}


type 'a map_int_ops = {
  find: int -> 'a map_int -> 'a;
  add: int -> 'a -> 'a map_int -> 'a map_int;
}

type 'a map_tm_ops = {
  find: tm -> 'a map_tm -> 'a;
  add: tm -> 'a -> 'a map_tm -> 'a map_tm;
}

val map_tm_: 'a map_tm_ops

val map_int_ : 'a map_int_ops

val map_nt_ : 'a map_nt_ops

(* val map_nt_find: nt -> 'a map_nt -> 'a  *)

type nt_item_set_ops = {
  empty: nt_item_set;
  add: nt_item -> nt_item_set -> nt_item_set;
  mem: nt_item -> nt_item_set -> bool;
}

val nt_item_set_ : nt_item_set_ops

end) -> struct

open S
open Profile

type ixk = (i_t * nt)  (* i X k *)

module Ixk_set =
  Set.Make(
  struct
    type t = ixk
    let compare: t -> t -> int = Pervasives.compare
  end)

type ixk_set = Ixk_set.t

type bitms_at_k = nt_item_set map_nt  (* bitms blocked at k,X *)

type bitms_lt_k = (nt_item_set map_nt) option array


module Blocked_map = struct
  type t = bitms_lt_k
  let empty : int -> t = fun len -> Array.make (len+2) None
  let find : (k_t * nt) -> bitms_lt_k -> nt_item_set = (
    fun (k,nt) t -> 
      let map = dest_Some (Array.get t k) in
      map_nt_.find nt map 
  )
  let add: t -> k_t -> bitms_at_k -> t = (
    fun s0 k bitms -> Array.set s0 k (Some(bitms)); s0)

end

type state_t = {
  k: int;
  todo: nt_item list;
  todo_done: nt_item_set;
  todo_gt_k: nt_item_set map_int;
  ixk_done: ixk_set;  (* i X k *)
  ktjs: int list option map_tm;  (* k T j *)
  bitms_lt_k: bitms_lt_k;
  bitms_at_k: bitms_at_k;
  all_done: nt_item_set list;
}

let wrap find k m default = (
  try find k m with Not_found -> default)

let bitms: state_t -> (k_t * nt) -> nt_item_set = (
  fun s0 (k,x) ->
    match (k=s0.k) with
    | true -> (wrap map_nt_.find x s0.bitms_at_k nt_item_set_.empty)
    | false -> (wrap Blocked_map.find (k,x) s0.bitms_lt_k nt_item_set_.empty))

(* nt_item blocked on nt at k *)
let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
  fun nitm nt s0 ->
    { s0 with
      bitms_at_k = (
        let m = s0.bitms_at_k in
        let s = wrap map_nt_.find nt m nt_item_set_.empty in
        let s' = nt_item_set_.add nitm s in
        let m' = map_nt_.add nt s' m in
        m' ) }



let pop_todo s0 = (
  match s0.todo with
  | x::xs -> (x,{s0 with todo=xs})
  | _ -> (failwith "pop_todo"))


let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)

(* k is the current stage *)
(* FIXME avoid cost of double lookup by using new ocaml sets with boolean rv *)
let add_todo: nt_item -> state_t -> state_t = 
  fun nitm s0 ->
    let k = s0.k in
    match nitm.k > k with
    | true -> (
        let nitms =
          wrap map_int_.find nitm.k s0.todo_gt_k nt_item_set_.empty
        in
        let nitms = nt_item_set_.add nitm nitms in
        { s0 with
          todo_gt_k=(map_int_.add nitm.k nitms s0.todo_gt_k)})
    | false -> (
        match nt_item_set_.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with
              todo=(nitm::s0.todo);
              todo_done=nt_item_set_.add nitm s0.todo_done}))

let add_ixk_done: ixk -> state_t -> state_t =
  fun ix s0 ->
    { s0 with ixk_done=(Ixk_set.add ix s0.ixk_done)}


let mem_ixk_done: ixk -> state_t -> bool =
  fun ix s0 -> Ixk_set.mem ix s0.ixk_done 


let find_ktjs: tm -> state_t -> int list option =
  fun t s0 ->
    wrap map_tm_.find t s0.ktjs None


let counter = ref 0

let step_k: ctxt_t -> state_t -> state_t = (
  fun c0 s0 ->
    let _ = debug_endline "XXXstep_k" in
    let _ = assert(log P.ab) in
    let _ = (
      counter:=1 + !counter; 
      if (!counter mod 1000 = 0) then Gc.full_major() else () )
    in
    let _ = assert(log P.ac) in
    let k = s0.k in    
    let bitms = bitms s0 in
    let (nitm,s0) = pop_todo s0 in
    (* let _ = print_endline (nitm_to_string nitm) in *)
    let complete = nitm.bs = [] in
    let _ = assert(log P.bc) in
    match complete with
    | true -> (
        (*{|adoc ----|}*)
        (*{|adoc |}*)
        (*{|adoc We have a complete item `(i,X,k)`.|}*)
        (*{|adoc |}*)
        (*{|adoc [source,ocaml]|}*)
        (*{|adoc ----|}*)
        let (i,x) = (nitm.i,nitm.nt) in
        (* possible NEW COMPLETE (i,X,k) *)
        let already_done = mem_ixk_done (i,x) s0 in
        let _ = assert(log P.cd) in
        match already_done with
        | true -> (
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc If the item has already been processed, don't do anything.|}*)
            (*{|adoc Otherwise, add to `ixk` map.|}*)
            (*{|adoc |}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let _ = debug_endline "already_done" in
            s0)
        | false -> (
            let _ = debug_endline "not already_done" in
            let s0 = add_ixk_done (i,x) s0 in
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc Then we need to process the complete item against the blocked items.|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let bitms = bitms (i,x) in
        (*
        let fm bitm s1 = (add_todo (cut bitm k) s1) in
        let r = nt_item_set_.fold fm bitms s0 in
           *)
            (* following is a fold using mutable state FIXME while_not_nil? *)
            let r = (
              (* FIXME possible optimization if we work with Y -> {h} as i
                 X bs *)
              let s1 = ref s0 in
              let f bitm = (s1:=add_todo (cut bitm k) (!s1)) in
              let _ = nt_item_set_.iter f bitms in
              !s1
            )
            in
            let _ = assert(log P.de) in
            r
          ))
    | false -> (
        (*{|adoc ----|}*)
        (*{|adoc |}*)
        (*{|adoc Otherwise, we have an item `(X -> i as k (S bs'))` blocked on `k,S`.|}*)
        (*{|adoc |}*)
        (*{|adoc [source,ocaml]|}*)
        (*{|adoc ----|}*)
        (* NEW BLOCKED X -> i as k (S bs') on k S*)
        let bitm = nitm in
        let s = List.hd bitm.bs in
        match s with
        | NT y -> (
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc If `S` is a nonterminal `y`...|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let _ = assert(log P.ef) in
            (* have we already processed k Y? *)
            let bitms = bitms (k,y) in
            let bitms_empty = nt_item_set_.is_empty bitms in
            (* record blocked FIXME we may already have processed k Y *)
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc record the new blocked item.|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let s0 = add_bitm_at_k bitm y s0 in
            let _ = assert(log P.fg) in
            match bitms_empty with
            | false -> (
                (*{|adoc ----|}*)
                (*{|adoc |}*)
                (*{|adoc If we have already processed `k,Y` (ie, this is not the first time we have seen nonterminal `Y` at stage `k`)...|}*)
                (*{|adoc |}*)
                (*{|adoc [source,ocaml]|}*)
                (*{|adoc ----|}*)
                let _ = debug_endline "not bitms_empty" in
                (* already processed k Y, so no need to expand; but may have complete item kYk *)
                (*{|adoc ----|}*)
                (*{|adoc |}*)
                (*{|adoc then we don't need to expand `Y`, but we may have to consider the|}*)
                (*{|adoc interaction of `bitm` with the complete item `(k,Y,k)`: if the|}*)
                (*{|adoc complete item has been found, we need to cut it against the blocked|}*)
                (*{|adoc item.|}*)
                (*{|adoc |}*)
                (*{|adoc [source,ocaml]|}*)
                (*{|adoc ----|}*)
                match (mem_ixk_done (k,y) s0) with
                | true -> (add_todo (cut bitm k) s0)
                | false -> s0)
            | true -> (
                (*{|adoc ----|}*)
                (*{|adoc |}*)
                (*{|adoc If we haven't processed nt `Y` yet...|}*)
                (*{|adoc |}*)
                (*{|adoc [source,ocaml]|}*)
                (*{|adoc ----|}*)
                let _ = debug_endline "bitms_empty" in
                (* need to process Y *)
                (* do we have an item (k,Y,k) ? no *)
                let _ = assert (mem_ixk_done (k,y) s0 = false) in
                (* expand y *)
                (*{|adoc ----|}*)
                (*{|adoc |}*)
                (*{|adoc expand `Y`, and add new items. Note that there cannot be any complete|}*)
                (*{|adoc items `(k,Y,k)` because this is the first time we are processing `Y`|}*)
                (*{|adoc at this stage.|}*)
                (*{|adoc |}*)
                (*{|adoc [source,ocaml]|}*)
                (*{|adoc ----|}*)
                let new_itms = c0.g0.nt_items_for_nt y (c0.i0.str,k) in
                let fn s1 nitm = add_todo nitm s1 in
                let r = List.fold_left fn s0 new_itms in
                let _ = assert(log P.gh) in
                r
              ) )
        | TM t -> (
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc If `S` is a nonterminal `T`...|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            (* have we already processed k T ? *)
            let ktjs = find_ktjs t s0 in
            let _ = assert(log P.hi) in
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc (we need to bind `js` to the indexes resulting from a terminal parse|}*)
            (*{|adoc of `T` at this stage)|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let (js,s0) = (
              match ktjs with
              | None -> (
                  (*{|adoc ----|}*)
                  (*{|adoc |}*)
                  (*{|adoc if we have not already dealt with `T` at the current stage...|}*)
                  (*{|adoc |}*)
                  (*{|adoc [source,ocaml]|}*)
                  (*{|adoc ----|}*)
                  let _ = debug_endline "ktjs None" in
                  (* process k T *)
                  let _ = debug_endline "processing k T" in
                  let p = c0.g0.p_of_tm t in
                  (*{|adoc ----|}*)
                  (*{|adoc |}*)
                  (*{|adoc we use the terminal parser to get a list of indexes `j`, and update the `ktjs` map.|}*)
                  (*{|adoc |}*)
                  (*{|adoc [source,ocaml]|}*)
                  (*{|adoc ----|}*)
                  let js = p (c0.i0.str,k,c0.i0.len) in
                  let s0 = { s0 with ktjs=(map_tm_.add t (Some js) s0.ktjs) } in
                  (js,s0))
              | Some js -> (
                  (*{|adoc ----|}*)
                  (*{|adoc |}*)
                  (*{|adoc if we have already dealt with `T` at the current stage...|}*)
                  (*{|adoc |}*)
                  (*{|adoc [source,ocaml]|}*)
                  (*{|adoc ----|}*)
                  let _ = debug_endline "ktjs Some" in
                  (js,s0)))
            in
            let _ = assert(log P.ij) in
            (* process blocked; there is only one item blocked at this point *)
            (*{|adoc ----|}*)
            (*{|adoc |}*)
            (*{|adoc Now we have `js`, corresponding to a set of complete items. We need to|}*)
            (*{|adoc process these against the relevant blocked items. Note that we only|}*)
            (*{|adoc parse a terminal once at stage `k`, and this occurs when we meet the|}*)
            (*{|adoc first corresponding blocked item. Thus, there is only one blocked item|}*)
            (*{|adoc at this point.|}*)
            (*{|adoc |}*)
            (*{|adoc [source,ocaml]|}*)
            (*{|adoc ----|}*)
            let fo s1 j = add_todo (cut bitm j) s1 in
            let r = List.fold_left fo s0 js in
            let _ = assert(log P.jk) in
            r
          )
      )            
)


(* loop at k *)
let loop_k: ctxt_t -> state_t -> state_t = 
  fun ctxt -> (
      let step_k = step_k ctxt in
      let rec f s0 = (
        match s0.todo with
        | [] -> s0
        | _ -> (f (step_k s0)))
      in
      f)


(* outer loop: repeatedly process items at stage k, then move to stage
   k+1 *)
let loop: ctxt_t -> state_t -> state_t = (
  fun c0 -> (
      let loop_k = loop_k c0 in
      let rec f s0 = (
        match s0.k > c0.i0.len + 1 with
        | true -> s0
        | false -> (
            (* process items *)
            let s0 = loop_k s0 in
            let old_k = s0.k in
            let k = s0.k+1 in
            let todo =
              wrap map_int_.find k s0.todo_gt_k nt_item_set_.empty
            in
            let todo_done = todo in
            let todo = nt_item_set_.elements todo in
            let todo_gt_k = (
              (* keep debug into around *)
              match !debug with 
              | true -> s0.todo_gt_k | false -> map_int_.remove k s0.todo_gt_k) 
            in
            let ixk_done = Ixk_set.empty in
            let ktjs = map_tm_.empty in
            let bitms_lt_k = (
              (* FIXME the following hints that bitms_lt_k should be a
                 map from k to a map from nt to ... since bitms_at_k is a
                 map from nt *)
              let b_init = s0.bitms_lt_k in
              Blocked_map.add b_init old_k s0.bitms_at_k)
            in
            let bitms_at_k = map_nt_.empty in
            let all_done =  s0.todo_done::s0.all_done in
            let s1 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done}
            in
            f s1
          )
      )
      in
      f
    )
)

(* construct initial context, apply loop *)
let staged: ctxt_t -> nt -> state_t = (
  fun c0 nt ->
    let (i,k) = (0,0) in
    let init = {nt;i;as_=[];k;bs=[NT nt]} in (* this is a dummy item to get things going *)
    let todo = [init] in  
    let todo_done = nt_item_set_.empty in
    let todo_gt_k = map_int_.empty in
    let ixk_done = Ixk_set.empty in
    let ktjs = map_tm_.empty in
    let bitms_lt_k = Blocked_map.empty c0.i0.len in
    let bitms_at_k = map_nt_.empty in
    let all_done = [] in
    let s0 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done} in
    loop c0 s0)

end
