(* TODO:

   use imperative hashmaps as set and map implementations 

   test with a default implementation

   replace all type ascriptions with named args


   remove functorization; 'sym nt_item amd sym_case as a record field
   ('sym)sym_ops: sym_case: 'a. ...

   can we assume there is a map from nt to int? if we keep nt
   abstract, we have that some modules for maps and sets depend on
   this abstract type; but perhaps this doesn't matter because we can
   use first class modules inside a function? but what is end result
   exported back to user? nt_item_set list, which depends on nt; maybe
   we keep nt abstract and allow user to instantiate this however they
   like;

   return val should be nt -> i -> j -> ks

   so nt is a free type var when removing functorization


   FIXME add links in code to sects of doc? use ++++ passthrough

   FIXME convert to C

*)

(* all maps are "with default" ie to option, or empty set; no
   exceptions thrown; so we need different map_ops for each map (since
   map_int may need two different defaults), or else take a default
   param FIXME add defaults to maps, or options*)
module type Map_ = sig
  type k_
  type 'v map_
  type 'v ops = {
    find: k_ -> 'v map_ -> 'v;
    (* FIXME following is preferred type? Stick with find for now,
       with specialized defaults *)
    (* find': 'a. k_ -> _Some:('v -> 'a) -> _None:(unit -> 'a) -> 'v map_ -> 'a; *)
    add: k_ -> 'v -> 'v map_ -> 'v map_;
    empty: 'v map_;
    remove: k_ -> 'v map_ -> 'v map_
  }
  val ops: default:'v -> 'v ops
end

module type Set_ = sig
  type elt
  type set
  type ops = {
    empty: set;
    is_empty: set -> bool;
    add: elt -> set -> set;
    mem: elt -> set -> bool;
    with_each_elt: 'a. f:(state:'a -> elt -> 'a) -> init_state:'a -> set -> 'a;
    elements: set -> elt list;
  }
  val ops: ops
end



module List_ = struct

  let fold_left_ ~step ~init_state xs = 
    List.fold_left 
      (fun a b -> step ~state:a b)
      init_state
      xs

  let with_each_elt = fold_left_
end


let bool_case ~true_ ~false_ x = (
  match x with 
  | true -> true_ ()
  | false -> false_ ()
)

let _ = bool_case

module type S_ = sig
  type i_t = int
  type k_t = int
  type j_t = int
  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;  (* NOTE in "reversed" order *)
    k: k_t;
    bs: sym list
  }

  module Set_nt_item : Set_ with type elt=nt_item
  type nt_item_set = Set_nt_item.set
  val nt_item_set_ : Set_nt_item.ops

  type ixk = (i_t * nt)  (* i X k *)
  module Set_ixk : Set_ with type elt=ixk
  type ixk_set = Set_ixk.set
  val ixk_set : Set_ixk.ops


  module Map_nt : Map_ with type k_ = nt
  type 'a map_nt = 'a Map_nt.map_

  module Map_int: Map_ with type k_ = int
  type 'a map_int = 'a Map_int.map_

  module Map_tm: Map_ with type k_ = tm
  type 'a map_tm = 'a Map_tm.map_

  (* these have obvious defaults for find operation *)
  val map_int_ : nt_item_set Map_int.ops
  val map_tm_: (int list option) Map_tm.ops
  val map_nt_ : nt_item_set Map_nt.ops


end

(* FIXME do all this in a monad, with particular monadic ops? would
   this improve readability? probably yes 

   FIXME make eg find_ktjs etc take s.ktjs, not s?
*)
module Make = functor (S:S_) -> struct
  open S
  open S.Map_nt
  open S.Map_int
  open S.Map_tm
  open S.Set_nt_item
  open S.Set_ixk

  open Profile

  type bitms_at_k = nt_item_set map_nt  (* bitms blocked at k,X *)
  (* can use map_nt_ operations for at_k *)

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
    (* per k *)

    todo_done: nt_item_set; 
    (* per k; todo_done at stage k *)

    todo_gt_k: nt_item_set map_int; 
    (* forall k' > k; array (int/j) to nt_item_set? empty for large k'
       > k_current and not needed for j<k *)

    ixk_done: ixk_set;  (* i X k *)  
    (* per k; array (int/i) with values a set of nt?; set of nt
       implemented by binary upto 63/64 bits *)

    ktjs: int list option map_tm;  (* k T j *)  
    (* per k; array (tm) with values a list of int *)

    bitms_lt_k: bitms_lt_k;  (* for all k; defined locally, see above *)

    bitms_at_k: bitms_at_k;  (* per k; ditto *)

    all_done: nt_item_set list;  
    (* not really needed - just for returning results as a list *)
  }

  (* want to separate out the state at k and the other state *)
  module X_ = struct
    type state_at_k = {
      k:k_t;
      todo:nt_item list;
      todo_done:nt_item_set;
      todo_gt_k:nt_item_set map_int;
      ixk_done:ixk_set;
      ktjs:int list option map_tm;
      bitms_at_k:bitms_at_k;
    }
    (* bitms_lt_k is read-only; all_done accumulates todo_done when move to k+1 *)
  end

  let bitms: state_t -> (k_t * nt) -> nt_item_set = (
    fun s0 (k,x) ->
      match (k=s0.k) with
      | true -> (s0.bitms_at_k |> map_nt_.find x)
      | false -> (Blocked_map.find (k,x) s0.bitms_lt_k))

  (* nt_item blocked on nt at k *)
  let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
    fun nitm nt s0 ->
      { s0 with
        bitms_at_k = (
          let m = s0.bitms_at_k in
          let s = map_nt_.find nt m in
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
        let nitms = map_int_.find nitm.k s0.todo_gt_k in
        let nitms = nt_item_set_.add nitm nitms in
        { s0 with todo_gt_k=(map_int_.add nitm.k nitms s0.todo_gt_k)})
    | false -> (
        match nt_item_set_.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with todo=(nitm::s0.todo);
                      todo_done=nt_item_set_.add nitm s0.todo_done}))

  let add_ixk_done: ixk -> state_t -> state_t =
    fun ix s0 -> { s0 with ixk_done=(ixk_set.add ix s0.ixk_done)}

  let mem_ixk_done: ixk -> state_t -> bool =
    fun ix s0 -> ixk_set.mem ix s0.ixk_done 

  let find_ktjs: tm -> state_t -> int list option =
    fun t s0 -> map_tm_.find t s0.ktjs

  let counter = ref 0


  let in_ctxt ctxt : int = 
    ctxt @@ begin fun ~new_items ~input ~parse_tm ~input_length 
      ~debug_enabled ~debug_endline ~init_nt -> 

      (* step_k ------------------------------------------------------- *)
      let step_k s0 = begin
        debug_endline "XXXstep_k";
        assert(log P.ab);
(*        let _ = (
          counter:=1 + !counter; 
          if (!counter mod 1000 = 0) then Gc.full_major() else () )
        in*)
        assert(log P.ac);
        let k = s0.k in    
        let bitms = bitms s0 in
        let (nitm,s0) = pop_todo s0 in
        let nitm_complete = nitm.bs = [] in
        assert(log P.bc);  
        (* NOTE waypoints before each split and at end of branch *)
        nitm_complete 
        |> bool_case
          ~true_: (fun () ->
            let (i,x) = (nitm.i,nitm.nt) in
            (* possible NEW COMPLETE (i,X,k) *)
            let already_done = mem_ixk_done (i,x) s0 in
            assert(log P.cd);
            already_done 
            |> bool_case
              ~true_:(fun () -> 
                debug_endline "already_done"; 
                s0)
              ~false_:(fun () -> 
                debug_endline "not already_done";
                let s0 = add_ixk_done (i,x) s0 in
                (* FIXME possible optimization if we work with Y ->
                     {h} as i X bs *)
                bitms (i,x)
                |> nt_item_set_.with_each_elt
                  ~f:(fun ~state:s bitm -> add_todo (cut bitm k) s)
                  ~init_state:s0
                |> fun s ->
                assert(log P.de);
                s))
          ~false_: (fun () -> 
            (* NEW BLOCKED X -> i as k (S bs') on k S; here S is _Y or t *)
            let bitm = nitm in
            let s = List.hd bitm.bs in
            s 
            |> sym_case
              ~nt:(fun _Y -> 
                (* have we already processed k Y? *)
                let bitms = bitms (k,_Y) in
                let bitms_empty = nt_item_set_.is_empty bitms in
                (* NOTE already_processed_kY = not bitms_empty *)
                let s0 = add_bitm_at_k bitm _Y s0 in
                assert(log P.fg);
                bitms_empty 
                |> bool_case
                  ~false_:(fun () -> 
                    (* already processed k Y, so no need to expand; but
                       may have complete item kYk *)
                    (* FIXME when dpes kYk get added to ixk_done? *)
                    debug_endline "not bitms_empty";
                    mem_ixk_done (k,_Y) s0 
                    |> bool_case
                      ~true_:(fun () -> add_todo (cut bitm k) s0)
                      ~false_:(fun () -> s0))  (* FIXME waypoint? *)
                  ~true_:(fun () ->
                    (* we have not processed k Y; expand sym Y *)
                    debug_endline "bitms_empty";
                    assert (mem_ixk_done (k,_Y) s0 = false);
                    new_items ~nt:_Y ~input ~k 
                    |> List_.with_each_elt 
                      ~step:(fun ~state:s nitm -> add_todo nitm s) 
                      ~init_state:s0
                    |> fun s -> 
                    assert(log P.gh);
                    s))
              ~tm:(fun t ->
                (* have we already processed k T ? *)
                find_ktjs t s0 |> fun ktjs ->
                assert(log P.hi);
                ktjs 
                |> begin function 
                  | None -> (
                      (* process k T *)
                      debug_endline "ktjs None";
                      debug_endline "processing k T";
                      let js = parse_tm ~tm:t ~input ~k ~input_length in
                      let ktjs = map_tm_.add t (Some js) s0.ktjs in
                      (js,{s0 with ktjs}))
                  | Some js -> (
                      debug_endline "ktjs Some"; (js,s0)) 
                end
                |> fun (js,s0) -> 
                assert(log P.ij);
                (* process blocked; there is only one item blocked at
                   this point *)
                js 
                |> List_.with_each_elt
                  ~step:(fun ~state:s j -> add_todo (cut bitm j) s)
                  ~init_state:s0 
                |> fun s -> 
                assert(log P.jk);
                s))
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
        match s0.k >= input_length with  (* correct? FIXME don't we have to go one further? *)
        | true -> s0
        | false -> (
            (* process items *)
            let s0 = loop_k s0 in
            let old_k = s0.k in
            let k = s0.k+1 in
            let todo = map_int_.find k s0.todo_gt_k in
            let todo_done = todo in
            let todo = nt_item_set_.elements todo in
            let todo_gt_k = (
              (* keep debug into around *)
              match debug_enabled with 
              | true -> s0.todo_gt_k 
              | false -> map_int_.remove k s0.todo_gt_k)
            in
            let ixk_done = ixk_set.empty in
            let ktjs = map_tm_.empty in
            let bitms_lt_k = (
              (* FIXME the following hints that bitms_lt_k should be a
                 map from k to a map from nt to ... since bitms_at_k is a
                 map from nt *)
              let b_init = s0.bitms_lt_k in
              Blocked_map.add b_init old_k s0.bitms_at_k)
            in
            let bitms_at_k = map_nt_.empty in
            let all_done = s0.todo_done::s0.all_done in
            let s1 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;
                      bitms_at_k;all_done} in
            loop s1)
      end (* loop *)
      in

      (* staged: main entry point ------------------------------------- *)

      (* construct initial context, apply loop *)
      let result : state_t = (
        let (i,k) = (0,0) in
        (* this is a dummy item to get things going *)
        let init = {nt=init_nt;i;as_=[];k;bs=[_NT init_nt]} in 
        let todo = [init] in  
        let todo_done = nt_item_set_.empty in
        let todo_gt_k = map_int_.empty in
        let ixk_done = ixk_set.empty in
        let ktjs = map_tm_.empty in
        let bitms_lt_k = Blocked_map.empty input_length in
        let bitms_at_k = map_nt_.empty in
        let all_done = [] in
        let s0 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;
                  bitms_at_k;all_done} in
        loop s0)
      in

      (result.k : int)  (* want to return something that can be given a type outside the functor FIXME *)

    end  (* end in_ctxt *)


  let staged ~ctxt = in_ctxt ctxt

end (* Make *)
