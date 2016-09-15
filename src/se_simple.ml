(*{|scala object se_simple {|}*)
(*{|scala |}*)
(*{|scala import se_common._|}*)
(*{|scala |}*)
(*{|scala val adoc = s"""|}*)
(*{|adoc == Simple Earley parsing (file: se_simple.ml)|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
(* Earley, nt_items only, unstaged, O(n^3) *)

open Se_common
open Se_common.Unstaged

let comp = Pervasives.compare

module Blocked_map =
  Map.Make(struct type t = b_key_t;; let compare: t -> t -> int = comp end)

module Complete_map =
  Map.Make(struct type t = c_key_t;; let compare: t -> t -> int = comp end)


(* use an option so that we can potentially optimize terminal parsing - see below *)
type cm_t = Int_set.t option Complete_map.t
type bm_t = Nt_item_set.t Blocked_map.t

(* add some defaults *)

let cm_find k m = try Complete_map.find k m with Not_found -> Some(Int_set.empty)
let bm_find k m = try Blocked_map.find k m with Not_found -> Nt_item_set.empty

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The state of the Earley algorithm is represented as a record with the|}*)
(*{|adoc following fields:|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc * `todo_done` is the set of all items that are pending, or have|}*)
(*{|adoc   already been processed|}*)
(*{|adoc * `todo` is the list of all items that are pending|}*)
(*{|adoc * `blocked` is the blocked map|}*)
(*{|adoc * `complete` is the complete map|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

type state_t = {
    todo_done: Nt_item_set.t;
    todo: nt_item list;
    blocked: bm_t;
    complete: cm_t
  }

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We add an item to the state by adding it to `todo` and|}*)
(*{|adoc `todo_done`. If the item is already in `todo_done` we leave the state|}*)
(*{|adoc unchanged.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let add_todo: nt_item -> state_t -> state_t = (
    fun itm s0 -> (
      match (Nt_item_set.mem itm s0.todo_done) with
      | true -> s0
      | false -> {s0 with
                   todo_done=(Nt_item_set.add itm s0.todo_done);
                   todo=(itm::s0.todo) }    )  )

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Now we implement the core Earley step. This takes a blocked|}*)
(*{|adoc item ${nitm(x,i,as,k,s"S $bs")} and a complete item ${citm(k,s,j)} and|}*)
(*{|adoc forms a new item of the form ${nitm(x,i,s"$as S",j,bs)}. Note that the|}*)
(*{|adoc latexmath:[$as] field is stored in "reverse" order (to make this|}*)
(*{|adoc operation more efficient).|}*)
(*{|adoc |}*)
(*{|adoc Notation: latexmath:[S $bs] is the list latexmath:[$bs] with latexmath:[S] cons'ed on. latexmath:[$as S] is the|}*)
(*{|adoc list latexmath:[$as] with latexmath:[S] joined on the end.|}*)
(*{|adoc |}*)
(*{|adoc ----|}*)
(*{|adoc X -> i,as,k,(S bs)   k S j|}*)
(*{|adoc ------------------------- cut|}*)
(*{|adoc X -> i,(as S),j,bs|}*)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let cut: nt_item -> j_t -> nt_item = (
    fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm ))

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We then give definitions for adding a complete item to the complete|}*)
(*{|adoc map, and a blocked item to the blocked map.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let citm_to_key = (fun citm -> (citm.k,citm.sym))

let c_add: citm_t -> cm_t -> cm_t = (
    fun citm cm -> (
      let key = citm_to_key citm in
      (*  invariant: anything in the map is Some(...); FIXME so why have option type? *)
      let s = (cm_find key cm) |> dest_Some in
      let s' = Int_set.add citm.j s in
      let cm' = Complete_map.add key (Some s') cm in
      cm' ))

let bitm_to_key = (fun (bitm:bitm_t) -> (bitm.k,List.hd bitm.bs))

let b_add: bitm_t -> bm_t -> bm_t = (
    fun bitm bm -> (
      let key = bitm_to_key bitm in
      let s = bm_find key bm in
      let s' = Nt_item_set.add bitm s in
      let bm' = Blocked_map.add key s' bm in
      bm' ))

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We pull out some common code `process_citms` to process a list of complete|}*)
(*{|adoc items, all of which have a given key. Processing involves adding each|}*)
(*{|adoc `citm` to the complete map, and cutting each item against the relevant|}*)
(*{|adoc blocked items.|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

(* process citms; update complete map; cut against blocked items *)

let process_citms key citms s0 = (
    let f5 citm s1 = 
      { s1 with complete=(c_add citm s1.complete) } in
    let s0 = while_not_nil citms f5 s0 in
    (* cut citm against blocked *)
    let bitms = bm_find key s0.blocked in
    let f8 citm s1 = (
        let f6 bitm s1 = (let nitm = cut bitm citm.j in add_todo nitm s1) in
        let s1 = Nt_item_set.fold f6 bitms s1 in
        s1)
    in
    let s0 = while_not_nil citms f8 s0 in
    s0 )

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Now we reach the core `step` part of Earley's algorithm. The|}*)
(*{|adoc full algorithm repeatedly applies `step` to an initial state until|}*)
(*{|adoc there are no further `todo` items.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let step: ctxt_t -> state_t -> state_t = (
    fun c0 s0 -> (
      match s0.todo with
      | [] -> s0  (* finished *)
      | nitm::rest -> (
        (* process itm *)
        let s0 = { s0 with todo=rest } in
        let complete = (nitm.bs = []) in
(*{|adoc |}*)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We are processing an nt item. This item may be complete. If|}*)
(*{|adoc so, via `process_citms` we record it in the complete map, and process|}*)
(*{|adoc it against any blocked items with the same key.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc |}*)
        match complete with
        | true -> (
          let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
          let citm : citm_t = {k;sym;j} in
          let key = citm_to_key citm in
          process_citms key [citm] s0
        )
        | false -> (

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The nt item is not complete. So we record it in the blocked|}*)
(*{|adoc map. |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

          (* blocked, so process next sym *)
          let bitm = nitm in
          let (k,sym) = (bitm.k,List.hd nitm.bs) in
          let key = (k,sym) in
          (* record bitm *)
          let s0 = { s0 with blocked=(b_add bitm s0.blocked) } in
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We then try to progress the item by cutting it with all the|}*)
(*{|adoc current complete items with the same key. |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
          (* process blocked against complete items *)
          let f2 j s1 = (let nitm = cut bitm j in add_todo nitm s1) in
          let js = (cm_find key s0.complete) |> dest_Some in
          let s0 = Int_set.fold f2 js s0 in
          (* now look at symbol we are blocked on *)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc It may be that we have yet|}*)
(*{|adoc to process all or any of the relevant complete items. So we also have|}*)
(*{|adoc to look at the symbol the nt item is blocked on, and manufacture more|}*)
(*{|adoc items. |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

          match sym with
          | NT nt -> (
            let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
            let f3 nitm s1 = (add_todo nitm s1) in
            let s0 = while_not_nil nitms f3 s0 in
            s0
          )
          | TM tm -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The nt item was blocked on a terminal `tm` (or latexmath:[T]). We use `p_of_tm` to|}*)
(*{|adoc determine which substrings of the input can be parsed as the terminal|}*)
(*{|adoc latexmath:[T]. This gives us complete items of the form ${citm(k,t,j)}. For each|}*)
(*{|adoc `citm` we then update the complete map and process against blocked|}*)
(*{|adoc items, using `process_citms`.|}*)
(*{|adoc |}*)
(*{|adoc There is a possible optimization here: if the key is already in the|}*)
(*{|adoc complete map, we don't need to process it again. This is why we use an|}*)
(*{|adoc option for the complete map codomain. For simplicity we don't|}*)
(*{|adoc incorporate this optimization.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let k = nitm.k in
            let p = c0.g0.p_of_tm tm in
            let js = p (c0.i0.str,k,c0.i0.len) in
            let citms = List.map (fun j -> {k;sym;j}) js in
            let key = (k,sym) in
            process_citms key citms s0 )))))

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc That concludes the explanation of the core of the algorithm.|}*)
(*{|adoc |}*)
(*{|adoc Next we repeatedly apply the step function in a loop until there|}*)
(*{|adoc are no more items to do.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let rec earley' ctxt s0 = (
    if s0.todo = [] then s0 else earley' ctxt (step ctxt s0))

let se_simple c0 nt = (
    let (i,k) = (0,0) in
    let init = {nt;i;as_=[];k;bs=[NT nt]} in
    let todo = [init] in
    let todo_done = Nt_item_set.empty in
    let blocked = Blocked_map.empty in
    let complete = Complete_map.empty in
    let s0 = {todo; todo_done; blocked; complete} in
    let s1 = earley' c0 s0 in
    s1)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc === Complexity|}*)
(*{|adoc |}*)
(*{|adoc We assume that there is a constant latexmath:[c] such that each invocation of|}*)
(*{|adoc `p_of_tm` produces at most latexmath:[c * n] results.|}*)
(*{|adoc |}*)
(*{|adoc As implemented, the algorithm is latexmath:[O(n^{3}\\ log\\ n)] because the sets and|}*)
(*{|adoc maps use OCaml's default sets and maps, which are implemented as|}*)
(*{|adoc binary trees. However, clearly given an input and a grammar, there are|}*)
(*{|adoc only a finite number of items that can be in any of the sets or|}*)
(*{|adoc maps. Thus, we can enumerate these items, and use the enumeration to|}*)
(*{|adoc implement e.g. a set as an array. This would give the latexmath:[O(n^3)] desired|}*)
(*{|adoc complexity.|}*)
(*{|adoc |}*)
(*{|scala """|}*)
(*{|scala |}*)
(*{|scala val adoc2 = adoc.lines.map(s => |}*)
(*{|scala   if (s.startsWith("//adoc ")) s.substring(7) |}*)
(*{|scala   else if (s.startsWith("//ml ")) s.substring(5)|}*)
(*{|scala   else s).mkString("\n")|}*)
(*{|scala |}*)
(*{|scala // write doc to README.scala.adoc|}*)
(*{|scala |}*)
(*{|scala |}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala import java.io._|}*)
(*{|scala new PrintWriter("se_simple.pp.adoc") { write(se_simple.adoc2); close }|}*)
(*{|scala |}*)
