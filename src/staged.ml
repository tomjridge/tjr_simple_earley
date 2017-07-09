(* TODO:

   use imperative hashmaps as set and map implementations *)

module type Map_ = sig
  type k_
  type 'v map_
  type 'v ops = {
    find: k_ -> 'v map_ -> 'v;
    add: k_ -> 'v -> 'v map_ -> 'v map_;
    empty: 'v map_;
    remove: k_ -> 'v map_ -> 'v map_
  }
end

module type Set_ = sig
  type elt
  type set
  type ops = {
    empty: set;
    is_empty: set -> bool;
    add: elt -> set -> set;
    mem: elt -> set -> bool;
    while_: 'a. (elt -> 'a -> 'a) -> set -> 'a -> 'a;
    elements: set -> elt list;
  }
end

let debug_endline s = ()
let debug = ref true

module type S_ = sig
  type i_t = int
  type k_t = int
  type j_t = int
  type nt
  type tm
  type sym = NT of nt | TM of tm
  type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;
    k: k_t;
    bs: sym list
  }

  module Map_nt : Map_ with type k_ = nt
  type 'a map_nt = 'a Map_nt.map_

  module Map_int: Map_ with type k_ = int
  type 'a map_int = 'a Map_int.map_

  module Map_tm: Map_ with type k_ = tm
  type 'a map_tm = 'a Map_tm.map_

  val map_nt_ : 'a Map_nt.ops
  val map_int_ : 'a Map_int.ops
  val map_tm_: 'a Map_tm.ops

  module Set_nt_item : Set_ with type elt=nt_item
  type nt_item_set = Set_nt_item.set
  val nt_item_set_ : Set_nt_item.ops
end

(* FIXME do all this in a monad, with particular monadic ops? would
   this improve readability? probably yes *)
module Make = functor (S:S_) -> struct
  open S
  open S.Map_nt
  open S.Map_int
  open S.Map_tm
  open S.Set_nt_item

  open Profile

  type ixk = (i_t * nt)  (* i X k *)

  module Ixk_set =
    Set.Make(
    struct
      type t = ixk
      let compare: t -> t -> int = Pervasives.compare
    end)

  type ixk_set = Ixk_set.t

  type bitms_at_k = nt_item_set map_nt  (* bitms blocked at k,X *)

  (* map from int to map from nt to nt_item_set *)
  type bitms_lt_k = (nt_item_set map_nt) option array

  module Blocked_map = struct
    type t = bitms_lt_k
    let empty : int -> t = fun len -> Array.make (len+2) None
    let find : (k_t * nt) -> bitms_lt_k -> nt_item_set = (
      fun (k,nt) t -> 
        let map = dest_Some (Array.get t k) in
        map_nt_.find nt map 
    )
    let add: t -> k_t -> bitms_at_k -> t = (
      fun s0 k bitms -> Array.set s0 k (Some(bitms)); s0)

  end

  type state_t = {
    k: int;
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: nt_item_set map_int;
    ixk_done: ixk_set;  (* i X k *)
    ktjs: int list option map_tm;  (* k T j *)
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    all_done: nt_item_set list;
  }

  (* FIXME remove this - assume funs give default *)
  let wrap find k m default = (
    try find k m with Not_found -> default)

  let bitms: state_t -> (k_t * nt) -> nt_item_set = (
    fun s0 (k,x) ->
      match (k=s0.k) with
      | true -> (wrap map_nt_.find x s0.bitms_at_k nt_item_set_.empty)
      | false -> (wrap Blocked_map.find (k,x) s0.bitms_lt_k nt_item_set_.empty))

  (* nt_item blocked on nt at k *)
  let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
    fun nitm nt s0 ->
      { s0 with
        bitms_at_k = (
          let m = s0.bitms_at_k in
          let s = wrap map_nt_.find nt m nt_item_set_.empty in
          let s' = nt_item_set_.add nitm s in
          let m' = map_nt_.add nt s' m in
          m' ) }

  let pop_todo s0 = (
    match s0.todo with
    | x::xs -> (x,{s0 with todo=xs})
    | _ -> (failwith "pop_todo"))

  let cut: nt_item -> j_t -> nt_item = (
    fun bitm j0 -> (
        let as_ = (List.hd bitm.bs)::bitm.as_ in
        let bs = List.tl bitm.bs in
        let k = j0 in
        let nitm ={bitm with k;as_;bs} in
        nitm ))

  (* k is the current stage *)
  (* FIXME avoid cost of double lookup by using new ocaml sets with
     boolean rv *)
  let add_todo: nt_item -> state_t -> state_t = fun nitm s0 ->
    let k = s0.k in
    match nitm.k > k with
    | true -> (
        let nitms = wrap map_int_.find nitm.k s0.todo_gt_k nt_item_set_.empty in
        let nitms = nt_item_set_.add nitm nitms in
        { s0 with todo_gt_k=(map_int_.add nitm.k nitms s0.todo_gt_k)})
    | false -> (
        match nt_item_set_.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with todo=(nitm::s0.todo);
                      todo_done=nt_item_set_.add nitm s0.todo_done}))

  let add_ixk_done: ixk -> state_t -> state_t =
    fun ix s0 -> { s0 with ixk_done=(Ixk_set.add ix s0.ixk_done)}

  let mem_ixk_done: ixk -> state_t -> bool =
    fun ix s0 -> Ixk_set.mem ix s0.ixk_done 

  let find_ktjs: tm -> state_t -> int list option =
    fun t s0 -> wrap map_tm_.find t s0.ktjs None

  let counter = ref 0

  
  let in_ctxt ctxt = begin
    ctxt @@ fun ~nt_items_for_nt ~input ~p_of_tm ~input_length -> 

    (* step_k ------------------------------------------------------- *)
    let step_k s0 = begin
      debug_endline "XXXstep_k";
      assert(log P.ab);
      let _ = (
        counter:=1 + !counter; 
        if (!counter mod 1000 = 0) then Gc.full_major() else () )
      in
      assert(log P.ac);
      let k = s0.k in    
      let bitms = bitms s0 in
      let (nitm,s0) = pop_todo s0 in
      let nitm_complete = nitm.bs = [] in
      assert(log P.bc);
      match nitm_complete with
      | true -> (
          let (i,x) = (nitm.i,nitm.nt) in
          (* possible NEW COMPLETE (i,X,k) *)
          let already_done = mem_ixk_done (i,x) s0 in
          assert(log P.cd);
          match already_done with
          | true -> (debug_endline "already_done"; s0)
          | false -> (
              debug_endline "not already_done";
              let s0 = add_ixk_done (i,x) s0 in
              let bitms = bitms (i,x) in
              let r = (
                (* FIXME possible optimization if we work with Y -> {h} as i
                   X bs *)
                let f bitm s = add_todo (cut bitm k) s in
                nt_item_set_.while_ f bitms s0)
              in
              assert(log P.de);
              r))
      | false -> (
          (* NEW BLOCKED X -> i as k (S bs') on k S*)
          let bitm = nitm in
          let s = List.hd bitm.bs in
          match s with
          | NT y -> (
              assert(log P.ef);
              (* have we already processed k Y? *)
              let bitms = bitms (k,y) in
              let bitms_empty = nt_item_set_.is_empty bitms in
              (* record blocked FIXME we may already have processed k Y *)
              let s0 = add_bitm_at_k bitm y s0 in
              assert(log P.fg);
              match bitms_empty with
              | false -> (
                  debug_endline "not bitms_empty";
                  (* already processed k Y, so no need to expand;
                     but may have complete item kYk *)
                  match (mem_ixk_done (k,y) s0) with
                  | true -> (add_todo (cut bitm k) s0)
                  | false -> s0)
              | true -> (
                  debug_endline "bitms_empty";
                  assert (mem_ixk_done (k,y) s0 = false);
                  (* expand y *)
                  let new_itms = nt_items_for_nt y (input,k) in
                  let fn s1 nitm = add_todo nitm s1 in
                  let r = List.fold_left fn s0 new_itms in
                  assert(log P.gh);
                  r))
          | TM t -> 
            (* have we already processed k T ? *)
            let ktjs = find_ktjs t s0 in
            assert(log P.hi);
            let (js,s0) = 
              match ktjs with
              | None -> (
                  debug_endline "ktjs None";
                  (* process k T *)
                  debug_endline "processing k T";
                  let p = p_of_tm t in
                  let js = p (input,k,input_length) in
                  let s0 = {s0 with ktjs=(map_tm_.add t (Some js) s0.ktjs) } in
                  (js,s0))
              | Some js -> (debug_endline "ktjs Some"; (js,s0))
            in
            assert(log P.ij);
            (* process blocked; there is only one item blocked at
               this point *)
            let fo s1 j = add_todo (cut bitm j) s1 in
            let r = List.fold_left fo s0 js in
            assert(log P.jk);
            r)
    end (*  step_k *)
    in


    (* loop_k: loop at k -------------------------------------------- *)
    let rec loop_k s0 = 
      match s0.todo with
      | [] -> s0
      | _ -> loop_k (step_k s0)
    in


    (* loop --------------------------------------------------------- *)
    (* outer loop: repeatedly process items at stage k, then move to stage
       k+1 *)
    let rec loop s0 = begin
      match s0.k > input_length + 1 with
      | true -> s0
      | false -> (
          (* process items *)
          let s0 = loop_k s0 in
          let old_k = s0.k in
          let k = s0.k+1 in
          let todo = wrap map_int_.find k s0.todo_gt_k nt_item_set_.empty in
          let todo_done = todo in
          let todo = nt_item_set_.elements todo in
          let todo_gt_k = (
            (* keep debug into around *)
            match !debug with 
            | true -> s0.todo_gt_k 
            | false -> map_int_.remove k s0.todo_gt_k)
          in
          let ixk_done = Ixk_set.empty in
          let ktjs = map_tm_.empty in
          let bitms_lt_k = (
            (* FIXME the following hints that bitms_lt_k should be a
               map from k to a map from nt to ... since bitms_at_k is a
               map from nt *)
            let b_init = s0.bitms_lt_k in
            Blocked_map.add b_init old_k s0.bitms_at_k)
          in
          let bitms_at_k = map_nt_.empty in
          let all_done =  s0.todo_done::s0.all_done in
          let s1 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;
                    bitms_at_k;all_done} in
          loop s1)
    end (* loop *)
    in

    (* staged: main entry point ------------------------------------- *)

    (* construct initial context, apply loop *)
    let staged nt = (
      let (i,k) = (0,0) in
      (* this is a dummy item to get things going *)
      let init = {nt;i;as_=[];k;bs=[NT nt]} in 
      let todo = [init] in  
      let todo_done = nt_item_set_.empty in
      let todo_gt_k = map_int_.empty in
      let ixk_done = Ixk_set.empty in
      let ktjs = map_tm_.empty in
      let bitms_lt_k = Blocked_map.empty input_length in
      let bitms_at_k = map_nt_.empty in
      let all_done = [] in
      let s0 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;
                bitms_at_k;all_done} in
      loop s0)
    in

    staged

  end  (* end in_ctxt *)

  let staged c nt = in_ctxt c nt

end (* Make *)
