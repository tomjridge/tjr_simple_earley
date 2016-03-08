type k_t = int
type i_t = int
type j_t = int


(** Symbols *)

type nt = int
type tm = int
type sym = NT of nt | TM of tm


(** Items *)

(* l:ba *)
type tm_item = {
  k: k_t;
  tm: tm
}

(* l:bc *)
type nt_item = {
  nt: nt;
  i: i_t;
  as_: sym list;
  k: k_t;
  bs: sym list
}

type bitm_t = nt_item  (* bs <> [] *)

(* l:cd *)
(* complete item *)
type citm_t = {
  k: k_t;
  sym: sym;
  j: j_t 
}

type item =   (* items that are being processed *)
  | NTITM of nt_item
  | TMITM of tm_item 


let is_NTITM x = (match x with NTITM _ -> true | _ -> false)
let dest_NTITM x = (match x with NTITM x -> x | _ -> failwith "dest_NTITM")

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

(* l:fg *)
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

module Item_set = struct
  include
  Set.Make(
  struct
    type t = item
    let compare: t -> t -> int = Pervasives.compare
  end)
    
  (* for < 4.02.0 *)
  let of_list: elt list -> t = (
    fun xs -> 
      List.fold_left (fun a b -> add b a) empty xs
  )
end

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
type cm_t = Int_set.t Complete_map.t
type bm_t = Nt_item_set.t Blocked_map.t

type state_t = {
  todo_done: Item_set.t;
  todo: item list;
  blocked: bm_t;
  complete: cm_t
}

(* l:hi *)
let add_todo: item -> state_t -> state_t = (
  fun itm s0 -> (
      match (Item_set.mem itm s0.todo_done) with
      | true -> s0
      | false -> {s0 with
                  todo_done=(Item_set.add itm s0.todo_done);
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
      let s = try Complete_map.find key cm with Not_found -> Int_set.empty in
      let s' = Int_set.add citm.j s in
      let cm' = Complete_map.add key s' cm in
      cm'
    )
)


let bitm_to_key = (fun (bitm:bitm_t) -> (bitm.k,List.hd bitm.bs))

let b_add: bitm_t -> bm_t -> bm_t = (
  fun bitm bm -> (
      let key = bitm_to_key bitm in
      let s = try Blocked_map.find key bm with Not_found -> Nt_item_set.empty in
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
  let bitms = try Blocked_map.find key s0.blocked with Not_found -> Nt_item_set.empty in
  let f8 s1 citm = (
    let f6 bitm s1 = (let nitm = cut bitm citm.j in add_todo (NTITM nitm) s1) in
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
| itm::rest -> (
    (* process itm *)
    let s0 = { s0 with todo=rest } in
    match itm with
    | NTITM nitm -> (  (* l:jp *)
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
            let f2 j s1 = (let nitm = cut bitm j in add_todo (NTITM nitm) s1) in
            let js = try Complete_map.find key s0.complete with Not_found -> Int_set.empty in
            let s0 = Int_set.fold f2 js s0 in
            (* now look at symbol we are blocked on *)  (* l:lm *)
            match sym with
            | NT nt -> (
                let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
                let f3 s1 nitm = (add_todo (NTITM nitm) s1) in
                let s0 = List.fold_left f3 s0 nitms in
                s0
              )
            | TM tm -> (add_todo (TMITM({k;tm})) s0)
          )
      )  (* NTITM *)
    | TMITM titm -> (  (* l:mn *)
        let tm = titm.tm in
        let k = titm.k in
        let sym = TM tm in
        let p = c0.g0.p_of_tm tm in
        let js = p (c0.i0.str,titm.k,c0.i0.len) in
        let citms = List.map (fun j -> {k;sym;j}) js in
        let key = (k,sym) in
        process_citms key citms s0
      )  (* TMITM *)
  )))


(* l:no *)
let rec earley' ctxt s0 = (
   if s0.todo = [] then s0 else earley' ctxt (step ctxt s0))

(* l:op *)
let earley c0 nt = (
  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,0) in
  let todo = List.map (fun x -> NTITM x) nitms in
  let todo_done = Item_set.of_list todo in
  let blocked = Blocked_map.empty in
  let complete = Complete_map.empty in
  let s0 = {todo; todo_done; blocked; complete} in
  earley' c0 s0
)


(* l:pq *)
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

let str = String.make 10 '1'

let i0 = (
  let len = String.length str in
  let str : string_t = string_to_string_t str in
  { str; len })

let c0 = {g0;i0}

let earley_as_list c0 e' = (
  earley c0 e' |> (fun x -> Item_set.elements x.todo_done
                          |> List.filter is_NTITM |> List.map dest_NTITM))

let earley_rs: nt_item list = earley_as_list c0 e'

let _ = print_endline "Finished"

(* let earley_rs = List.filter (fun (x:nt_item) -> x.k=100) rs *)

(* sample timings: 2.8s for a string of length 200 *)


