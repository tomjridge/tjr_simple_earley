(*{|scala object se_staged {|}*)
(*{|scala |}*)
(*{|scala import se_common._|}*)
(*{|scala |}*)
(*{|scala val adoc = s"""|}*)
(* staged version of Earley ---------------------------------------- *)
(*{|adoc |}*)
(*{|adoc == Staged version of Earley (file: se_staged.ml)|}*)
(*{|adoc |}*)
(*{|adoc This version processes all items at index latexmath:[k] (the current|}*)
(*{|adoc stage) before moving on to latexmath:[k+1] (the next stage).|}*)
(*{|adoc |}*)
(*{|adoc ////|}*)
(*{|adoc |}*)

(* TODO:

   use imperative hashmaps as set and map implementations *)

open Se_common
open Se_common.Unstaged
open Se_profile
(*{|adoc |}*)
(*{|adoc We have a map of nonterminal parses from index latexmath:[i] to index latexmath:[k].|}*)
(*{|adoc |}*)

type ixk_t = (i_t * nt)  (* i X k *)


module Ixk_set =
  Set.Make(
  struct
    type t = ixk_t
    let compare: t -> t -> int = Pervasives.compare
  end)


(*{|adoc |}*)
(*{|adoc We need to know which blocked items are present at the current stage,|}*)
(*{|adoc and at previous stages. Note for previous stages, we use an imperative array.|}*)
(*{|adoc |}*)
type bitms_at_k_t = Nt_item_set.t Map_nt.t  (* bitms blocked at k,X *)

type bitms_lt_k_t = (Nt_item_set.t Map_nt.t) option array


(*{|adoc |}*)
(*{|adoc FIXME the blocked map is slightly complicated.|}*)
(*{|adoc |}*)
module Blocked_map = struct
  type t = bitms_lt_k_t
  let empty : int -> t = fun len -> Array.make (len+2) None
  let find : (k_t * nt) -> bitms_lt_k_t -> Nt_item_set.t = (
    fun (k,nt) t -> 
      let map = dest_Some (Array.get t k) in
      Map_nt.find nt map 
  )
  let add: t -> k_t -> bitms_at_k_t -> t = (
    fun s0 k bitms -> Array.set s0 k (Some(bitms)); s0)

end

(*{|adoc |}*)
(*{|adoc The state at the current stage. |}*)
(*{|adoc |}*)
(* state at k *)
type state_t = {
  k: int;
  todo: nt_item list;
  todo_done: Nt_item_set.t;
  todo_gt_k: Nt_item_set.t Map_int.t;
  ixk_done: Ixk_set.t;  (* i X k *)
  ktjs: int list option Map_tm.t;  (* k T j *)
  bitms_lt_k: bitms_lt_k_t;
  bitms_at_k: bitms_at_k_t;
  all_done: Nt_item_set.t list;
}


(*{|adoc |}*)
(*{|adoc FIXME replace the following with hand-written find functions like `se_simple`|}*)
(*{|adoc |}*)
let wrap find k m default = (
  try find k m with Not_found -> default)

(*{|adoc |}*)
(*{|adoc Function to retrieve the blocked items at a given index, and add a new blocked item.|}*)
(*{|adoc |}*)

let bitms: state_t -> (k_t * nt) -> Nt_item_set.t = (
  fun s0 (k,x) ->
    match (k=s0.k) with
    | true -> (wrap Map_nt.find x s0.bitms_at_k Nt_item_set.empty)
    | false -> (wrap Blocked_map.find (k,x) s0.bitms_lt_k Nt_item_set.empty))

(* nt_item blocked on nt at k *)
let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
  fun nitm nt s0 ->
    { s0 with
      bitms_at_k = (
        let m = s0.bitms_at_k in
        let s = wrap Map_nt.find nt m Nt_item_set.empty in
        let s' = Nt_item_set.add nitm s in
        let m' = Map_nt.add nt s' m in
        m' ) }



let pop_todo s0 = (
  match s0.todo with
  | x::xs -> (x,{s0 with todo=xs})
  | _ -> (failwith "pop_todo"))


(*{|adoc |}*)
(*{|adoc FIXME from `se_common`?|}*)
(*{|adoc |}*)
let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)


(*{|adoc |}*)
(*{|adoc Add a todo item:|}*)
(*{|adoc |}*)
(* k is the current stage *)
(* FIXME avoid cost of double lookup by using new ocaml sets with boolean rv *)
let add_todo: nt_item -> state_t -> state_t = 
  fun nitm s0 ->
    let k = s0.k in
    match nitm.k > k with
    | true -> (
        let nitms =
          wrap Map_int.find nitm.k s0.todo_gt_k Nt_item_set.empty
        in
        let nitms = Nt_item_set.add nitm nitms in
        { s0 with
          todo_gt_k=(Map_int.add nitm.k nitms s0.todo_gt_k)})
    | false -> (
        match Nt_item_set.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with
              todo=(nitm::s0.todo);
              todo_done=Nt_item_set.add nitm s0.todo_done}))

(*{|adoc |}*)
(*{|adoc `ixk` functions:|}*)
(*{|adoc |}*)

let add_ixk_done: ixk_t -> state_t -> state_t =
  fun ix s0 ->
    { s0 with ixk_done=(Ixk_set.add ix s0.ixk_done)}


let mem_ixk_done: ixk_t -> state_t -> bool =
  fun ix s0 -> Ixk_set.mem ix s0.ixk_done 

(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc `ktjs` function:|}*)
(*{|adoc |}*)


let find_ktjs: tm -> state_t -> int list option =
  fun t s0 ->
    wrap Map_tm.find t s0.ktjs None


(*{|adoc |}*)
(*{|adoc The counter is for profiling.|}*)
(*{|adoc |}*)
let counter = ref 0

(*{|adoc |}*)
(*{|adoc ////|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
let step_k: ctxt_t -> state_t -> state_t = (
  fun c0 s0 ->
    let _ = debug_endline "XXXstep_k" in
    let _ = assert(log P.ab) in
    let _ = (
      counter:=1 + !counter; 
      if (!counter mod 1000 = 0) then Gc.full_major() else () )
    in
    let _ = assert(log P.ac) in
    let k = s0.k in    
    let bitms = bitms s0 in
    let (nitm,s0) = pop_todo s0 in
    (* let _ = print_endline (nitm_to_string nitm) in *)
    let complete = nitm.bs = [] in
    let _ = assert(log P.bc) in
    match complete with
    | true -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc We have a complete item `(i,X,k)`.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
        let (i,x) = (nitm.i,nitm.nt) in
        (* possible NEW COMPLETE (i,X,k) *)
        let already_done = mem_ixk_done (i,x) s0 in
        let _ = assert(log P.cd) in
        match already_done with
        | true -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc If the item has already been processed, don't do anything.|}*)
(*{|adoc Otherwise, add to `ixk` map.|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let _ = debug_endline "already_done" in
            s0)
        | false -> (
            let _ = debug_endline "not already_done" in
            let s0 = add_ixk_done (i,x) s0 in
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Then we need to process the complete item against the blocked items.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let bitms = bitms (i,x) in
        (*
        let fm bitm s1 = (add_todo (cut bitm k) s1) in
        let r = Nt_item_set.fold fm bitms s0 in
           *)
            (* following is a fold using mutable state FIXME while_not_nil? *)
            let r = (
              (* FIXME possible optimization if we work with Y -> {h} as i
                 X bs *)
              let s1 = ref s0 in
              let f bitm = (s1:=add_todo (cut bitm k) (!s1)) in
              let _ = Nt_item_set.iter f bitms in
              !s1
            )
            in
            let _ = assert(log P.de) in
            r
          ))
    | false -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Otherwise, we have an item `(X -> i as k (S bs'))` blocked on `k,S`.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
        (* NEW BLOCKED X -> i as k (S bs') on k S*)
        let bitm = nitm in
        let s = List.hd bitm.bs in
        match s with
        | NT y -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc If `S` is a nonterminal `y`...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let _ = assert(log P.ef) in
            (* have we already processed k Y? *)
            let bitms = bitms (k,y) in
            let bitms_empty = Nt_item_set.is_empty bitms in
            (* record blocked FIXME we may already have processed k Y *)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc record the new blocked item.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let s0 = add_bitm_at_k bitm y s0 in
            let _ = assert(log P.fg) in
            match bitms_empty with
            | false -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc If we have already processed `k,Y` (ie, this is not the first time we have seen nonterminal `Y` at stage `k`)...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                let _ = debug_endline "not bitms_empty" in
                (* already processed k Y, so no need to expand; but may have complete item kYk *)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc then we don't need to expand `Y`, but we may have to consider the|}*)
(*{|adoc interaction of `bitm` with the complete item `(k,Y,k)`: if the|}*)
(*{|adoc complete item has been found, we need to cut it against the blocked|}*)
(*{|adoc item.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                match (mem_ixk_done (k,y) s0) with
                | true -> (add_todo (cut bitm k) s0)
                | false -> s0)
            | true -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc If we haven't processed nt `Y` yet...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                let _ = debug_endline "bitms_empty" in
                (* need to process Y *)
                (* do we have an item (k,Y,k) ? no *)
                let _ = assert (mem_ixk_done (k,y) s0 = false) in
                (* expand y *)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc expand `Y`, and add new items. Note that there cannot be any complete|}*)
(*{|adoc items `(k,Y,k)` because this is the first time we are processing `Y`|}*)
(*{|adoc at this stage.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                let new_itms = c0.g0.nt_items_for_nt y (c0.i0.str,k) in
                let fn s1 nitm = add_todo nitm s1 in
                let r = List.fold_left fn s0 new_itms in
                let _ = assert(log P.gh) in
                r
              ) )
        | TM t -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc If `S` is a nonterminal `T`...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            (* have we already processed k T ? *)
            let ktjs = find_ktjs t s0 in
            let _ = assert(log P.hi) in
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc (we need to bind `js` to the indexes resulting from a terminal parse|}*)
(*{|adoc of `T` at this stage)|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let (js,s0) = (
              match ktjs with
              | None -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc if we have not already dealt with `T` at the current stage...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                  let _ = debug_endline "ktjs None" in
                  (* process k T *)
                  let _ = debug_endline "processing k T" in
                  let p = c0.g0.p_of_tm t in
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc we use the terminal parser to get a list of indexes `j`, and update the `ktjs` map.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                  let js = p (c0.i0.str,k,c0.i0.len) in
                  let s0 = { s0 with ktjs=(Map_tm.add t (Some js) s0.ktjs) } in
                  (js,s0))
              | Some js -> (
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc if we have already dealt with `T` at the current stage...|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
                  let _ = debug_endline "ktjs Some" in
                  (js,s0)))
            in
            let _ = assert(log P.ij) in
            (* process blocked; there is only one item blocked at this point *)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Now we have `js`, corresponding to a set of complete items. We need to|}*)
(*{|adoc process these against the relevant blocked items. Note that we only|}*)
(*{|adoc parse a terminal once at stage `k`, and this occurs when we meet the|}*)
(*{|adoc first corresponding blocked item. Thus, there is only one blocked item|}*)
(*{|adoc at this point.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
            let fo s1 j = add_todo (cut bitm j) s1 in
            let r = List.fold_left fo s0 js in
            let _ = assert(log P.jk) in
            r
          )
      )            
)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc === Loop at k|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

(* loop at k *)
let loop_k: ctxt_t -> state_t -> state_t = 
  fun ctxt -> (
      let step_k = step_k ctxt in
      let rec f s0 = (
        match s0.todo with
        | [] -> s0
        | _ -> (f (step_k s0)))
      in
      f)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc === Earley loop over all k|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

(* outer loop: repeatedly process items at stage k, then move to stage
   k+1 *)
let loop: ctxt_t -> state_t -> state_t = (
  fun c0 -> (
      let loop_k = loop_k c0 in
      let rec f s0 = (
        match s0.k > c0.i0.len + 1 with
        | true -> s0
        | false -> (
            (* process items *)
            let s0 = loop_k s0 in
            let old_k = s0.k in
            let k = s0.k+1 in
            let todo =
              wrap Map_int.find k s0.todo_gt_k Nt_item_set.empty
            in
            let todo_done = todo in
            let todo = Nt_item_set.elements todo in
            let todo_gt_k = (
              (* keep debug into around *)
              match !debug with 
              | true -> s0.todo_gt_k | false -> Map_int.remove k s0.todo_gt_k) 
            in
            let ixk_done = Ixk_set.empty in
            let ktjs = Map_tm.empty in
            let bitms_lt_k = (
              (* FIXME the following hints that bitms_lt_k should be a
                 map from k to a map from nt to ... since bitms_at_k is a
                 map from nt *)
              let b_init = s0.bitms_lt_k in
              Blocked_map.add b_init old_k s0.bitms_at_k)
            in
            let bitms_at_k = Map_nt.empty in
            let all_done =  s0.todo_done::s0.all_done in
            let s1 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done}
            in
            f s1
          )
      )
      in
      f
    )
)

(* construct initial context, apply loop *)
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Initialize state, and run Earley loop:|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
let se_staged: ctxt_t -> nt -> state_t = (
  fun c0 nt ->
    let (i,k) = (0,0) in
    let init = {nt;i;as_=[];k;bs=[NT nt]} in (* this is a dummy item to get things going *)
    let todo = [init] in  
    let todo_done = Nt_item_set.empty in
    let todo_gt_k = Map_int.empty in
    let ixk_done = Ixk_set.empty in
    let ktjs = Map_tm.empty in
    let bitms_lt_k = Blocked_map.empty c0.i0.len in
    let bitms_at_k = Map_nt.empty in
    let all_done = [] in
    let s0 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done} in
    loop c0 s0)

(*{|adoc ----|}*)
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
(*{|scala |}*)
(*{|scala import java.io._|}*)
(*{|scala new PrintWriter("se_staged.pp.adoc") { write(se_staged.adoc2); close }|}*)