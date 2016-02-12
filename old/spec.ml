open Simple_earley

(** Specification of Earley's algorithm *)

type spec_item_t = CITM of citm_t | PITM of item (* pending *)

let is_CITM x = (match x with CITM _ -> true | _ -> false)
let dest_CITM x = (
  match x with CITM citm -> citm | _ -> failwith "dest_CITM")

let is_PITM x = (match x with PITM _ -> true | _ -> false)
let dest_PITM x = (
  match x with PITM itm -> itm | _ -> failwith "dest_PITM")

module Spec_t =
  Set.Make(
  struct
    type t = spec_item_t
    let compare: t -> t -> int = Pervasives.compare
  end)

type spec_t = Spec_t.t

let spec_to_bitms: spec_t -> b_key_t -> bitm_t list = (
  fun s0 key -> 
    (Spec_t.elements s0)
    |> List.map (function
        | PITM (NTITM nitm) when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat)


let new_items : spec_t -> spec_item_t -> spec_item_t list = (
  fun s0 itm -> (
      match itm with
      | CITM citm -> (
          let j = citm.j in
          let key = (citm.k,citm.sym) in
          let bitms = spec_to_bitms s0 key in
          let f8 bitm = (let nitm = cut bitm j in PITM(NTITM nitm)) in
          List.map f8 bitms)
      | PITM(NTITM nitm) -> (
          let complete = (nitm.bs = []) in
          match complete with
          | true -> (
              let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
              let citm : citm_t = {k;sym;j} in
              [CITM citm])
          | false -> (  
              (* blocked, so process next sym *)
              let bitm = nitm in
              let (k,sym) = (bitm.k,List.hd nitm.bs) in
              (* now look at symbol we are blocked on *)
              match sym with
              | NT nt -> (
                  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
                  let f3 nitm = (PITM (NTITM nitm)) in
                  List.map f3 nitms)
              | TM tm -> [PITM (TMITM({k;tm}))]
            )
        )
      | PITM(TMITM titm) -> (
          let tm = titm.tm in
          let k = titm.k in
          let sym = TM tm in
          let p = c0.g0.p_of_tm tm in
          let js = p (c0.i0.str,titm.k,c0.i0.len) in
          (* update complete items *)
          let f5 j = (CITM {k;sym;j}) in
          List.map f5 js)
    )
)

let rec spec' ctxt s0 = (
  let new_itms = (
    (Spec_t.elements s0)
    |> List.map (new_items s0)
    |> List.concat
    |> Spec_t.of_list)
  in
  let s1 = Spec_t.union s0 new_itms in
  if Spec_t.equal s1 s0 then s0 else spec' ctxt s1)

let spec c0 nt = (
  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,0) in
  let todo = List.map (fun x -> PITM(NTITM x)) nitms in
  let s0 = Spec_t.of_list todo in
  spec' c0 s0
)

let spec_rs = spec c0 e'

let _ = (
  let nitms = (
    (Spec_t.elements spec_rs)
    |> (List.map (function
        | PITM(NTITM nitm) -> [nitm]
        | _ -> []))
    |> List.concat)
  in
  assert(nitms = earley_rs)
)
