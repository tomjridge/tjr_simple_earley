(** An abstract model of Earley parsing. *)

(** Specification of Earley's algorithm *)

open E_common

type spec_item_t = nt_item

module Spec_t = Nt_item_set

type spec_t = Spec_t.t

let spec_to_bitms: spec_t -> b_key_t -> bitm_t list = (
  fun s0 key -> 
    (Spec_t.elements s0)
    |> List.map (function
        | nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat)


let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)

let new_items : ctxt_t -> spec_t -> spec_item_t -> spec_item_t list = (
  fun c0 s0 nitm -> (
      let complete = (nitm.bs = []) in
      match complete with
      | true -> (
          let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
          (* let citm : citm_t = {k;sym;j} in *)
          let key = (k,sym) in
          let bitms = spec_to_bitms s0 key in
          let f bitm = cut bitm j in
          List.map f bitms)
      | false -> (
          (* blocked, so process next sym *)
          let bitm = nitm in
          let (k,sym) = (bitm.k,List.hd nitm.bs) in
          (* now look at symbol we are blocked on *)
          match sym with
          | NT nt -> (
              let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
              nitms)
          | TM tm -> (
              (* parse tm and complete with item *)
              let k = bitm.k in
              let p = c0.g0.p_of_tm tm in
              let js = p (c0.i0.str,k,c0.i0.len) in
              let f j = cut bitm j in
              List.map f js)
        )        
    )
)


(* repeatedly apply step till no change *)
let rec spec' c0 s0 = (
  let new_itms = (
    (Spec_t.elements s0)
    |> List.map (new_items c0 s0)
    |> List.concat
    |> Spec_t.of_list)
  in
  let s1 = Spec_t.union s0 new_itms in
  if Spec_t.equal s1 s0 then s0 else spec' c0 s1)

(* construct initial context, apply spec' *)
let spec c0 nt = (
  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,0) in
  let s0 = Spec_t.of_list nitms in
  spec' c0 s0
)

let spec_rs = (spec (c0 ()) e') |> Spec_t.elements

