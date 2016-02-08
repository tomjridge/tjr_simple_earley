type k_t = int
type i_t = int
type j_t = int


(** Symbols *)

type nt = int
type tm = int
type sym = NT of nt | TM of tm


(** Items *)

type tm_item = {
  k: k_t;
  tm: tm
}

type nt_item = {
  nt: nt;
  i: i_t;
  as_: sym list;
  k: k_t;
  bs: sym list
}

(* complete item *)
type citm_t = {
  k: k_t;
  sym: sym;
  j: j_t 
}

type item =   (* items that are being processed *)
  | NTITM of nt_item
  | TMITM of tm_item 


type string_t
type substring_t = (i_t * string_t * j_t)


type grammar_t = {
  nt_items_for_nt: nt -> (string_t * int) -> nt_item list;
  p_of_tm: tm -> substring_t -> k_t list
}

type input_t = {
  str: string_t;
  len: int;
}

type ctxt_t = grammar_t * input_t

type b_key_t = k_t * sym

type c_key_t = k_t * sym

module Int_set = 
  Set.Make(
  struct
    type t = int
    let compare: t -> t -> int = Pervasives.compare
  end)


module Nt_item_set = 
  Set.Make(
  struct
    type t = nt_item
    let compare: t -> t -> int = Pervasives.compare
  end)

module Item_set =
  Set.Make(
  struct
    type t = item
    let compare: t -> t -> int = Pervasives.compare
  end)

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



type cm_t = Int_set.t Complete_map.t
type bm_t = Nt_item_set.t Blocked_map.t

type state_t = {
  todo_done: Item_set.t;
  todo: item list;
  blocked: bm_t;
  complete: cm_t
}

let add_todo: item -> state_t -> state_t = (
  fun itm s0 -> (
      match (Item_set.mem itm s0.todo_done) with
      | true -> s0
      | false -> {s0 with
                  todo_done=(Item_set.add itm s0.todo_done);
                  todo=(itm::s0.todo) }
    )
)

let cut: nt_item -> j_t -> state_t -> state_t = (
  fun bitm j0 s0 -> (
      let itm = NTITM({bitm with k=j0}) in
      let s0 = add_todo itm s0 in
      s0
    )
)

let citm_to_key = (fun citm -> (citm.k,citm.sym))

let c_add: citm_t -> cm_t -> cm_t = (
  fun citm cm -> (
      let key = citm_to_key citm in
      let s = try Complete_map.find key cm with Not_found -> Int_set.empty in
      let s' = Int_set.add citm.j s in
      let cm' = Complete_map.add key s' cm in
      cm'
    )
)

let step: ctxt_t -> state_t -> state_t = (
  fun c0 ->
    let (g0,i0) = c0 in
    fun s0 -> (
        match s0.todo with
        | [] -> s0  (* finished *)
        | itm::rest -> (
            (* process itm *)
            let s0 = { s0 with todo=rest } in
            match itm with
            | NTITM nitm -> (
                let complete = (nitm.bs = []) in
                match complete with
                | true -> (
                    let citm : citm_t = {
                      k = nitm.i;
                      sym = NT(nitm.nt);
                      j = nitm.k
                    } in
                    let key = (citm.k,citm.sym) in
                    (* record citm *)
                    let s0 = { s0 with complete=(c_add citm s0.complete) } in
                    (* process against blocked items *)
                    let bitms = try Blocked_map.find key s0.blocked with Not_found -> Nt_item_set.empty in
                    let f1 bitm s1 = (cut bitm citm.j s1) in
                    let s0 = Nt_item_set.fold f1 bitms s0 in
                    s0
                  )
                | false -> s0
              )
          )
      )
)


