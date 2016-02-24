(* Earley, nt_items only, unstaged, O(n^3) *)

open E_common
open E_common.Unstaged


module Blocked_map =
  Map.Make(
  struct
    type t = b_key_t
    let compare: t -> t -> int = Pervasives.compare
  end)

module Complete_map =
  Map.Make(
  struct
    type t = c_key_t
    let compare: t -> t -> int = Pervasives.compare
  end)


(* l:gh *)
type cm_t = Int_set.t option Complete_map.t
type bm_t = Nt_item_set.t Blocked_map.t

type state_t = {
  todo_done: Nt_item_set.t;
  todo: nt_item list;
  blocked: bm_t;
  complete: cm_t
}

(* l:hi *)
let add_todo: nt_item -> state_t -> state_t = (
  fun itm s0 -> (
      match (Nt_item_set.mem itm s0.todo_done) with
      | true -> s0
      | false -> {s0 with
                  todo_done=(Nt_item_set.add itm s0.todo_done);
                  todo=(itm::s0.todo) }
    )
)

(* l:ij *)
let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)

let citm_to_key = (fun citm -> (citm.k,citm.sym))

let c_add: citm_t -> cm_t -> cm_t = (
  fun citm cm -> (
      let key = citm_to_key citm in
      (*  invariant: anything in the map is Some(...) *)
      let s = (find_with_default (Some Int_set.empty) Complete_map.find key cm) |> dest_Some in
      let s' = Int_set.add citm.j s in
      let cm' = Complete_map.add key (Some s') cm in
      cm'
    )
)


let bitm_to_key = (fun (bitm:bitm_t) -> (bitm.k,List.hd bitm.bs))

let b_add: bitm_t -> bm_t -> bm_t = (
  fun bitm bm -> (
      let key = bitm_to_key bitm in
      let s = find_with_default Nt_item_set.empty Blocked_map.find key bm in
      let s' = Nt_item_set.add bitm s in
      let bm' = Blocked_map.add key s' bm in
      bm'
    )        
)

(* l:ja *)
let process_citms key citms s0 = (
  let f5 s1 citm = 
    { s1 with complete=(c_add citm s1.complete) } in
  let s0 = List.fold_left f5 s0 citms in
  (* cut citm against blocked *)
  let bitms = find_with_default Nt_item_set.empty Blocked_map.find key s0.blocked in
  let f8 s1 citm = (
    let f6 bitm s1 = (let nitm = cut bitm citm.j in add_todo nitm s1) in
    let s1 = Nt_item_set.fold f6 bitms s1 in
    s1)
  in
  let s0 = List.fold_left f8 s0 citms in
  s0
)

(* l:jk *)
let step: ctxt_t -> state_t -> state_t = (
  fun c0 s0 -> (
      match s0.todo with
      | [] -> s0  (* finished *)
      | nitm::rest -> (
          (* process itm *)
          let s0 = { s0 with todo=rest } in
          (* l:jp *)
          let complete = (nitm.bs = []) in
          match complete with
          | true -> (
              let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
              let citm : citm_t = {k;sym;j} in
              let key = citm_to_key citm in
              process_citms key [citm] s0
            )
          | false -> (  (* l:kl *)
              (* blocked, so process next sym *)
              let bitm = nitm in
              let (k,sym) = (bitm.k,List.hd nitm.bs) in
              let key = (k,sym) in
              (* record bitm *)
              let s0 = { s0 with blocked=(b_add bitm s0.blocked) } in
              (* process blocked against complete items *)
              let f2 j s1 = (let nitm = cut bitm j in add_todo nitm s1) in
              let js = (find_with_default (Some Int_set.empty) Complete_map.find key s0.complete) |> dest_Some in
              let s0 = Int_set.fold f2 js s0 in
              (* now look at symbol we are blocked on *)  (* l:lm *)
              match sym with
              | NT nt -> (
                  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
                  let f3 s1 nitm = (add_todo nitm s1) in
                  let s0 = List.fold_left f3 s0 nitms in
                  s0
                )
              | TM tm -> (
                  (* l:mn *)
                  (* FIXME optimization: if key already in complete map, don't process again *)
                  let k = nitm.k in
                  let p = c0.g0.p_of_tm tm in
                  let js = p (c0.i0.str,k,c0.i0.len) in
                  let citms = List.map (fun j -> {k;sym;j}) js in
                  let key = (k,sym) in
                  process_citms key citms s0
                )))))




(* l:no *)
let rec earley' ctxt s0 = (
  if s0.todo = [] then s0 else earley' ctxt (step ctxt s0))

(* l:op *)
let earley c0 nt = (
  let (i,k) = (0,0) in
  let todo = [{nt;i;as_=[];k;bs=[NT nt]}] in
  let todo_done = Nt_item_set.of_list todo in
  let blocked = Blocked_map.empty in
  let complete = Complete_map.empty in
  let s0 = {todo; todo_done; blocked; complete} in
  earley' c0 s0
)


(* l:pq *)

let _ = str := (String.make 200 '1')  
let _ = earley (c0 ()) e'
let _ = print_endline "Finished"


(* sample timings: 2.436s for a string of length 200 *)


