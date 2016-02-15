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

type bitm_t = nt_item  (* bs <> [] *)

type b_key_t = (k_t * sym)

let bitm_to_key: bitm_t -> b_key_t = (
  fun bitm -> (bitm.k,List.hd bitm.bs))

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

type c_key_t = (k_t * sym)

let citm_to_key: citm_t -> c_key_t = (
  fun citm -> (citm.k,citm.sym))


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





(** Example E -> E E E | "1" | eps *)

let e' = 1
let e = NT e'
let _1 = TM 2
let eps = TM 3
    
let parse_eps = (fun (s,i,j) -> if i<=j then [i] else [])

let parse_1 = (fun (s,i,j) ->
    (* this terminal parser requires to know string_t *)
    let (s:string) = string_t_to_string s in  
    if i < j && i < String.length s && String.get s i = '1' then 
      [i+1]
    else
      [])

let p_of_tm = (fun tm -> 
    if TM tm=eps then parse_eps
    else if TM tm=_1 then parse_1
    else failwith "p_of_tm: p8t")

  
let g = [
  (e',[e;e;e]);
  (e',[_1]);
  (e',[eps])]

let nt_items_for_nt=(fun nt (s,i) ->
    let _ = assert(nt=e') in
    let as_ = [] in
    let k = i in
    [{nt;i;as_;k;bs=[e;e;e]};
     {nt;i;as_;k;bs=[_1]};
     {nt;i;as_;k;bs=[eps]}])

let g0 = {nt_items_for_nt; p_of_tm}

let str = ref (String.make 10 '1')

let i0 () = (
  let str = !str in
  let len = String.length str in
  let str : string_t = string_to_string_t str in
  { str; len })

let c0 () = {g0;i0=(i0 ())}

