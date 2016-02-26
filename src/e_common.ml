let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let find_with_default d f k m = try (f k m) with Not_found -> d

(** Common type definitions *)

type k_t = int
type i_t = int
type j_t = int


(** Symbols *)

type nt = int
type tm = int
type sym = NT of nt | TM of tm


(** Items *)


(* l:bc *)
type nt_item = {
  nt: nt;
  i: i_t;
  as_: sym list;
  k: k_t;
  bs: sym list
}


(* l:bh *)
type tm_item = {
  k: k_t;
  tm: tm
}


(* l:bm *)
type sym_item = {
  k: k_t;
  sym: sym
}

(* l:cd *)
(* complete item *)
type citm_t = {
  k: k_t;
  sym: sym;
  j: j_t 
}




(* l:de *)
type string_t
type substring_t = (string_t * i_t * j_t)

let string_to_string_t: string -> string_t = (fun s -> Obj.magic s)
let string_t_to_string: string_t -> string = (fun s -> Obj.magic s)

(* l:ef *)
type grammar_t = {
  nt_items_for_nt: nt -> (string_t * int) -> nt_item list;
  p_of_tm: tm -> substring_t -> k_t list
}

type input_t = {
  str: string_t;
  len: int;
}

type ctxt_t = {
  g0: grammar_t;
  i0: input_t
}


let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)



module Unstaged = struct

  type bitm_t = nt_item  (* bs <> [] *)

  type b_key_t = (k_t * sym)

  let bitm_to_key: bitm_t -> b_key_t = (
    fun bitm -> (bitm.k,List.hd bitm.bs))

  type c_key_t = (k_t * sym)
                 
  let citm_to_key: citm_t -> c_key_t = (
    fun citm -> (citm.k,citm.sym))

end





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


module Sym_item_set = 
  Set.Make(
  struct
    type t = sym_item
    let compare: t -> t -> int = Pervasives.compare
  end)


module Nt_set = 
  Set.Make(
  struct
    type t = nt
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



module Int_map = 
  Map.Make(
  struct
    type t = int
    let compare: t -> t -> int = Pervasives.compare
  end)




