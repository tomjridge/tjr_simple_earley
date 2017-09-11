(* wip; preparing to abstract over ntitem *)

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


module List_ = struct

  let fold_left_ ~step ~init_state xs = 
    List.fold_left 
      (fun a b -> step ~state:a b)
      init_state
      xs

  let with_each_elt = fold_left_
end


module Set_ops = struct
  type ('e,'t) set_ops = {
    add: 'e -> 't -> 't;
    mem: 'e -> 't -> bool;
    empty: 't;
    is_empty: 't -> bool;
    elements: 't -> 'e list;
  }
end


module Map_ops = struct
  type ('k,'v,'t) map_ops = {
    map_add: 'k -> 'v -> 't -> 't;
    map_find:'k -> 't -> 'v;
    map_empty:'t;
    map_remove:'k -> 't -> 't;
  }
end


open Set_ops

open Map_ops

module Bitms_lt_k_ops = struct
  type ('k,'v,'t) ltk_map_ops = {
    ltk_add: 'k -> 'v -> 't -> 't;
    ltk_find:'k -> 't -> 'v;
    ltk_empty:int -> 't;  (* need to*)
    ltk_remove:'k -> 't -> 't;
  }
end

open Bitms_lt_k_ops

module type S_ = sig
  type i_t = int
  type k_t = int
  type j_t = int
  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  type nt_item
  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> i_t
  val dot_k: nt_item -> k_t
  val dot_bs: nt_item -> sym list 

  type nt_item_set
  val nt_item_set_ops: (nt_item,nt_item_set) set_ops
  val nt_item_set_with_each_elt: 
    f:(state:'a -> nt_item -> 'a) -> init_state:'a -> nt_item_set -> 'a

  type ixk = (i_t * nt)  (* i X k *)
  type ixk_set
  val ixk_set_ops: (ixk,ixk_set) set_ops

  type map_nt
  val map_nt_ops: (nt,nt_item_set,map_nt) map_ops

  type map_int
  val map_int_ops : (int,nt_item_set,map_int) map_ops

  type map_tm
  val map_tm_ops : (tm,int list option,map_tm) map_ops

  (*
  type bitms_at_k
  val bitms_at_k_ops: (nt,nt_item_set,bitms_at_k) map_ops
   *)

  type bitms_lt_k  (* int -> nt -> nt_item_set; implement by array *)
  val bitms_lt_k_ops: (int,map_nt,bitms_lt_k) ltk_map_ops

  type todo_gt_k
  val todo_gt_k_ops: (int,nt_item_set,todo_gt_k) map_ops

  val cut: nt_item -> j_t -> nt_item

  val debug_enabled : bool
  val debug_endline : string -> unit
end

module Make = functor (S:S_) -> struct
  open S
  open Profile

  type bitms_at_k = map_nt  (* bitms blocked at k,X *)
  let bitms_at_k_ops = map_nt_ops 

  (* state at k changes at k *)
  type state = {
    k:k_t;
    todo:nt_item list;
    (* at k *)

    todo_done:nt_item_set;
    (* per k; todo_done at stage k *)

    todo_gt_k:todo_gt_k;
    (* forall k' > k; array (int/j) to nt_item_set? empty for large k'
       > k_current and not needed for j<k *)

    bitms_lt_k:bitms_lt_k;
    bitms_at_k:bitms_at_k;

    ixk_done:ixk_set;
    (* per k; array (int/i) with values a set of nt?; set of nt
       implemented by binary upto 63/64 bits *)

    ktjs:map_tm;
    (* per k; array (tm) with values a list of int *)
  }

  let bitms s0 (k,x) : nt_item_set = 
    match (k=s0.k) with
    | true -> (s0.bitms_at_k |> bitms_at_k_ops.map_find x)
    | false -> (s0.bitms_lt_k |> bitms_lt_k_ops.ltk_find k |> map_nt_ops.map_find x)

  (* nt_item blocked on nt at k *)
  let add_bitm_at_k nitm nt s0 : state = 
    { s0 with
      bitms_at_k =
        let m = s0.bitms_at_k in
        let s = map_nt_ops.map_find nt m in
        let s' = nt_item_set_ops.add nitm s in
        let m' = map_nt_ops.map_add nt s' m in
        m' }

  let pop_todo s0 =
    match s0.todo with
    | x::xs -> (x,{s0 with todo=xs})
    | _ -> failwith "pop_todo"

  (* k is the current stage *)
  (* FIXME avoid cost of double lookup by using new ocaml sets with
     boolean rv *)
  let add_todo nitm s0 : state = 
    let k = s0.k in
    let nitm_k = nitm|>dot_k in
    match nitm_k > k with
    | true -> 
      let nitms = todo_gt_k_ops.map_find nitm_k s0.todo_gt_k in
      let nitms = nt_item_set_ops.add nitm nitms in
      { s0 with todo_gt_k=(todo_gt_k_ops.map_add nitm_k nitms s0.todo_gt_k)}
    | false -> 
      match nt_item_set_ops.mem nitm s0.todo_done with
      | true -> s0
      | false -> 
        { s0 with todo=(nitm::s0.todo);
                  todo_done=nt_item_set_ops.add nitm s0.todo_done}

  let add_ixk_done ix s0 : state =
    { s0 with ixk_done=(ixk_set_ops.add ix s0.ixk_done)}

  let mem_ixk_done ix s0 : bool =
    ixk_set_ops.mem ix s0.ixk_done 

  let find_ktjs t s0 : int list option =
    map_tm_ops.map_find t s0.ktjs

  let counter = ref 0


  let run_earley ~new_items ~input ~parse_tm ~input_length ~init_nt = (

    (* step_k ------------------------------------------------------- *)
    let step_k s0 = (
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
      let nitm_complete = nitm|>dot_bs = [] in
      assert(log P.bc);  
      (* NOTE waypoints before each split and at end of branch *)
      match nitm_complete with
      | true -> (
          let (i,x) = (nitm|>dot_i,nitm|>dot_nt) in
          (* possible NEW COMPLETE (i,X,k) *)
          let already_done = mem_ixk_done (i,x) s0 in
          assert(log P.cd);
          already_done |> function
          | true -> (
              debug_endline "already_done"; 
              s0)
          | false -> (
              debug_endline "not already_done";
              let s0 = add_ixk_done (i,x) s0 in
              (* FIXME possible optimization if we work with Y ->
                   {h} as i X bs *)
              bitms (i,x)
              |> nt_item_set_with_each_elt
                ~f:(fun ~state:s bitm -> add_todo (cut bitm k) s)
                ~init_state:s0
              |> fun s ->
              assert(log P.de);
              s))  
      | false (* nitm_complete *) -> (
          (* NEW BLOCKED X -> i as k (S bs') on k S; here S is _Y or t *)
          let bitm = nitm in
          let s = List.hd (bitm|>dot_bs) in
          s |> sym_case
            ~nt:(fun _Y -> 
                (* have we already processed k Y? *)
                let bitms = bitms (k,_Y) in
                let bitms_empty = nt_item_set_ops.is_empty bitms in
                (* NOTE already_processed_kY = not bitms_empty *)
                let s0 = add_bitm_at_k bitm _Y s0 in
                assert(log P.fg);
                bitms_empty |> function
                | false -> (
                    (* already processed k Y, so no need to expand; but
                       may have complete item kYk *)
                    (* FIXME when dpes kYk get added to ixk_done? *)
                    debug_endline "not bitms_empty";
                    mem_ixk_done (k,_Y) s0 |> function
                    | true -> add_todo (cut bitm k) s0
                    | false -> s0)  (* FIXME waypoint? *)
                | true -> (
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
                |> (function 
                    | None -> (
                        (* process k T *)
                        debug_endline "ktjs None";
                        debug_endline "processing k T";
                        let js = parse_tm ~tm:t ~input ~k ~input_length in
                        let ktjs = map_tm_ops.map_add t (Some js) s0.ktjs in
                        (js,{s0 with ktjs}))
                    | Some js -> (debug_endline "ktjs Some"; (js,s0)))
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
    ) (*  step_k *)
    in


    (* loop_k: loop at k -------------------------------------------- *)
    let rec loop_k s0 = 
      match s0.todo with
      | [] -> s0
      | _ -> loop_k (step_k s0)
    in


    (* loop --------------------------------------------------------- *)
    (* outer loop: repeatedly process items at stage k, then move to
       stage k+1 *)
    let rec loop s0 = 
      match s0.k >= input_length with  (* correct? FIXME don't we have to go one further? *)
      | true -> s0
      | false -> 
        (* process items *)
        let s0 = loop_k s0 in
        let old_k = s0.k in
        let k = s0.k+1 in
        let todo = todo_gt_k_ops.map_find k s0.todo_gt_k in
        let todo_done = todo in
        let todo = nt_item_set_ops.elements todo in
        let todo_gt_k = 
          (* keep debug into around *)
          match debug_enabled with 
          | true -> s0.todo_gt_k 
          | false -> todo_gt_k_ops.map_remove k s0.todo_gt_k
        in
        let ixk_done = ixk_set_ops.empty in
        let ktjs = map_tm_ops.map_empty in
        let bitms_lt_k = 
          (* FIXME the following hints that bitms_lt_k should be a
             map from k to a map from nt to ... since bitms_at_k is a
             map from nt *)
          bitms_lt_k_ops.ltk_add old_k s0.bitms_at_k s0.bitms_lt_k
        in
        let bitms_at_k = map_nt_ops.map_empty in
        (* FIXME let all_done = s0.todo_done::s0.all_done in *)
        let s1 = 
          {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k} in
        loop s1  
        (* end loop *)
    in

    (* staged: main entry point ------------------------------------- *)
    (* construct initial context, apply loop *)
    let result : state = 
      let k = 0 in
      let init_items = new_items ~nt:init_nt ~input ~k in
      let todo = init_items in  
      let todo_done = nt_item_set_ops.empty in
      let todo_gt_k = todo_gt_k_ops.map_empty in
      let ixk_done = ixk_set_ops.empty in
      let ktjs = map_tm_ops.map_empty in
      let bitms_lt_k = bitms_lt_k_ops.ltk_empty input_length in
      let bitms_at_k = bitms_at_k_ops.map_empty in
      (* let all_done = [] in *)
      let s0 = 
        {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k} in (* ;all_done *)
      loop s0
    in

    result
  ) (* run_earley *)

end (* Make *)
