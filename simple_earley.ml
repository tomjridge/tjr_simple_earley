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

type bitm_t = nt_item  (* bs <> [] *)

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

type string_t
type substring_t = (string_t * i_t * j_t)

let string_to_string_t: string -> string_t = (fun s -> Obj.magic s)
let string_t_to_string: string_t -> string = (fun s -> Obj.magic s)

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
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let itm = NTITM({bitm with k;as_;bs}) in
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

let step: ctxt_t -> state_t -> state_t = (
  fun c0 ->
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
                    let key = citm_to_key citm in
                    (* record citm *)
                    let s0 = { s0 with complete=(c_add citm s0.complete) } in
                    (* process against blocked items *)
                    let bitms = try Blocked_map.find key s0.blocked with Not_found -> Nt_item_set.empty in
                    let f1 bitm s1 = (cut bitm citm.j s1) in
                    let s0 = Nt_item_set.fold f1 bitms s0 in
                    s0
                  )
                | false -> (
                    (* blocked, so process next sym *)
                    let bitm = nitm in
                    let (k,sym) = (bitm.k,List.hd nitm.bs) in
                    let key = (k,sym) in
                    (* record bitm *)
                    let s0 = { s0 with blocked=(b_add bitm s0.blocked) } in
                    (* process blocked against complete items *)
                    let f2 j s1 = (cut bitm j s1) in
                    let js = try Complete_map.find key s0.complete with Not_found -> Int_set.empty in
                    let s0 = Int_set.fold f2 js s0 in
                    (* now look at symbol we are blocked on *)
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
            | TMITM titm -> (
                let tm = titm.tm in
                let k = titm.k in
                let sym = TM tm in
                let p = c0.g0.p_of_tm tm in
                let js = p (c0.i0.str,titm.k,c0.i0.len) in
                (* update complete items *)
                let f5 s1 j = 
                  { s1 with complete=(c_add {k;sym;j} s1.complete) } in
                let s0 = List.fold_left f5 s0 js in
                (* cut citm against blocked *)
                let key = (k,sym) in
                let bitms = try Blocked_map.find key s0.blocked with Not_found -> Nt_item_set.empty in
                let f8 s1 j = (
                  let f6 bitm s1 = cut bitm j s1 in
                  let s1 = Nt_item_set.fold f6 bitms s1 in
                  s1)
                in
                let s0 = List.fold_left f8 s0 js in
                s0
              )  (* TMITM *)
          )
      )
)


let rec earley' ctxt s0 = (
   if s0.todo = [] then s0 else earley' ctxt (step ctxt s0))

let earley c0 nt = (
  let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,0) in
  let todo = List.map (fun x -> NTITM x) nitms in
  let todo_done = Item_set.of_list todo in
  let blocked = Blocked_map.empty in
  let complete = Complete_map.empty in
  let s0 = {todo; todo_done; blocked; complete} in
  earley' c0 s0
)


(* example *)

let e' = 1
let e = NT e'
let _1 = TM 2
let eps = TM 3
    
let parse_eps = (fun (s,i,j) -> if i<=j then [i] else [])

let parse_1 = (fun (s,i,j) ->
    (* this terminal parser requires to know string_t *)
    let (s:string) = Obj.magic s in  
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

let i0 = (
  let str = String.make 200 '1' in
  let len = String.length str in
  let str : string_t = string_to_string_t str in
  { str; len })

let c0 = {g0;i0}

let earley_as_list c0 e' = (
  earley c0 e' |> (fun x -> Item_set.elements x.todo_done
                          |> List.filter is_NTITM |> List.map dest_NTITM))

let rs = earley_as_list c0 e'

(* let rs = List.filter (fun (x:nt_item) -> x.k=100) rs *)

(* sample timings: 2.8s for a string of length 200 *)
