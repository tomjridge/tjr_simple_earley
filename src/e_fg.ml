(** Refine Earley by adding more items. *)

open E_common

type ixk_t = (i_t * nt)  (* i X k *)

module Ixk_set =
  Set.Make(
  struct
    type t = ixk_t
    let compare: t -> t -> int = Pervasives.compare
  end)

module Map_tm =
  Map.Make(
  struct
    type t = tm
    let compare: t -> t -> int = Pervasives.compare
  end)

module Map_nt =
  Map.Make(
  struct
    type t = nt
    let compare: t -> t -> int = Pervasives.compare
  end)

module Blocked_map =
  Map.Make(
  struct
    type t = (k_t * nt)
    let compare: t -> t -> int = Pervasives.compare
  end)

(* state at k *)
type state_t = {
  todo: nt_item list;
  todo_done: Nt_item_set.t;
  todo_gt_k: unit;
  ixk_done: Ixk_set.t;  (* i X k *)
  ktjs: int list option Map_tm.t;  (* k T j *)
  bitms_lt_k: Nt_item_set.t Blocked_map.t;
  bitms_at_k: Nt_item_set.t Map_nt.t;  (* bitms blocked at k,X *)
}

(* k0 is current stage *)
let bitms: k_t -> state_t -> (k_t * nt) -> Nt_item_set.t = (
  fun k0 s0 (k,x) ->
    match (k=k0) with
    | true -> (Map_nt.find x s0.bitms_at_k)
    | false -> (Blocked_map.find (k,x) s0.bitms_lt_k))

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
let add_todo: k_t -> nt_item -> state_t -> state_t = 
  fun k nitm s0 ->
    match nitm.k > k with
    | true -> s0  (* FIXME *)
    | false -> (
        match Nt_item_set.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with
              todo=(nitm::s0.todo);
              todo_done=Nt_item_set.add nitm s0.todo_done}))
      
let add_ixk_done: ixk_t -> state_t -> state_t =
  fun ix s0 ->
    { s0 with ixk_done=(Ixk_set.add ix s0.ixk_done)}

let mem_ixk_done: ixk_t -> state_t -> bool =
  fun ix s0 -> Ixk_set.mem ix s0.ixk_done 

(* nt_item blocked on nt at k *)
let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
  fun nitm nt s0 ->
    { s0 with bitms_at_k = (
          let m = s0.bitms_at_k in
          let s = Map_nt.find nt m in
          let s' = Nt_item_set.add nitm s in
          let m' = Map_nt.add nt s' m in
          m' ) }

let find_ktjs: tm -> state_t -> int list option =
  fun t s0 ->
    try
      Map_tm.find t s0.ktjs
    with _ -> None

let step_k: ctxt_t -> k_t -> state_t -> state_t = (
  fun c0 k s0 ->       
let bitms = bitms k s0 in
let (nitm,s0) = pop_todo s0 in
let complete = nitm.bs = [] in
match complete with
| true -> (
    let (i,x) = (nitm.i,nitm.nt) in
    (* possible NEW COMPLETE (i,X,k) *)
    let already_done = mem_ixk_done (i,x) s0 in
    match already_done with
    | true -> s0
    | false -> (
        let s0 = add_ixk_done (i,x) s0 in
        let fm bitm s1 = (add_todo k (cut bitm k) s1) in
        let bitms = bitms (i,x) in
        Nt_item_set.fold fm bitms s0))
| false -> (
    (* NEW BLOCKED X -> i as k (S bs') on k S*)
    let bitm = nitm in
    let s = List.hd bitm.bs in
    match s with
    | NT y -> (
        (* have we already processed k Y? *)
        let bitms = bitms (k,y) in
        let already_done = Nt_item_set.is_empty bitms in
        match already_done with
        | true -> s0
        | false -> (
            (* record blocked FIXME we may already have processed k Y *)
            let s0 = add_bitm_at_k bitm y s0 in
            (* do we have an item (k,Y,k) ? *)
            let complete = Ixk_set.mem (k,y) s0.ixk_done in
            let s0 = (match complete with
                | true -> (add_todo k (cut bitm k) s0)
                | false -> s0)
            in
            (* expand y *)
            let new_itms = c0.g0.nt_items_for_nt y (c0.i0.str,k) in
            let fn s1 nitm = add_todo k nitm s1 in
            List.fold_left fn s0 new_itms) )
    | TM t -> (
        (* have we already processed k T ? *)
        let ktjs = find_ktjs t s0 in
        match ktjs with
        | None -> (
            (* process k T *)
            let p = c0.g0.p_of_tm t in
            let js = p (c0.i0.str,k,c0.i0.len) in
            let s0 = { s0 with ktjs=(Map_tm.add t (Some js) s0.ktjs) } in
            (* process blocked *)
            let fo s1 j = add_todo k (cut bitm j) s1 in
            List.fold_left fo s0 js)
        | Some js -> (
            (* process blocked *)
            let fo s1 j = add_todo k (cut bitm j) s1 in
            List.fold_left fo s0 js)
      )            
  )
)
