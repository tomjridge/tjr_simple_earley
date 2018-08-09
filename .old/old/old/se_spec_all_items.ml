(* refine spec by adding more items. *)

open Se_common
open Se_common.Unstaged

type spec_item_t = NTITM of nt_item | CITM of citm_t | SITM of sym_item

module Spec_t = struct
  include 
  Set.Make(
  struct
    type t = spec_item_t
    let compare: t -> t -> int = Pervasives.compare
  end)

  (* for < 4.02.0 *)
  let of_list: elt list -> t = (
    fun xs -> 
      List.fold_left (fun a b -> add b a) empty xs
  )  

end

    
type spec_t = Spec_t.t

let spec_to_bitms: spec_t -> b_key_t -> bitm_t list = (
  fun s0 key -> 
    (Spec_t.elements s0)
    |> List.map (function
        | NTITM nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat)


let new_items: ctxt_t -> spec_t -> spec_item_t -> spec_item_t list = (
  fun c0 s0 itm -> (
      match itm with
      | NTITM nitm -> (
          let complete = (nitm.bs = []) in
          match complete with
          | true -> (
              let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
              let citm : citm_t = {k;sym;j} in
              [CITM citm])
          | false -> (
              (* blocked, so process next sym *)
              let (k,sym) = (nitm.k,List.hd nitm.bs) in
              [SITM{k;sym}]))
      | CITM citm -> (
          let key = citm_to_key citm in
          let j = citm.j in
          let bitms = spec_to_bitms s0 key in
          let f bitm = (cut bitm j) |> (fun x -> NTITM x) in
          List.map f bitms)
      | SITM sitm -> (
          let (k,sym) = (sitm.k,sitm.sym) in
          match sym with
          | NT nt -> (
              c0.g0.nt_items_for_nt nt (c0.i0.str,k)
              |> List.map (fun x -> NTITM x))
          | TM tm -> (
              let p = c0.g0.p_of_tm tm in
              let js = p (c0.i0.str,k,c0.i0.len) in
              let f j = CITM {k;sym;j} in
              List.map f js))
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
let se_spec_all_items c0 nt = (
  let (i,k) = (0,0) in
  let init = {nt;i;as_=[];k;bs=[NT nt]} in
  let nitms = [NTITM init] in
  let s0 = Spec_t.of_list nitms in
  let s1 = spec' c0 s0 in
  Spec_t.remove (NTITM init) s1)

