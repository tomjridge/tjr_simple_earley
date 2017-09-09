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

let bool_case ~true_ ~false_ = function
  | true -> true_ ()
  | false -> false_ ()

let _ = bool_case

type i_t = int
type k_t = int
type j_t = int

(*
  type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;  (* NOTE in "reversed" order *)
    k: k_t;
    bs: sym list
  }
*)

type ('elt,'t) set_ops = {
  add:'elt -> 't -> 't;
  mem:'elt -> 't -> bool;
  is_empty:'t -> bool;
  empty:'t;
  elements: 't -> 'elt list;
  with_each_elt: 'a. f:(state:'a -> 'elt -> 'a) -> init_state:'a -> 't -> 'a;
}

module Set = struct
  let add x = x#set_add
  let mem x = x#set_mem
  let is_empty x = x#is_empty
  let empty x = x#empty
  let elements x = x#elements 
  let wf_set 
      ~(add:'a -> 't -> 't)
      ~(mem:'a -> 't -> bool)
      ~(is_empty: 't -> bool)
      ~(empty: 't)
      ~(elements:'t -> 'a list)
    =
    true
end

type ('k,'v,'t) map_ops = {
  map_add:'k -> 'v -> 't -> 't;
  map_find:'k -> 't -> 'v;
  map_empty:'t;
  map_remove:'k -> 't -> 't;
}

module Map = struct
  let map_add x = x#map_add
  let map_find x = x#map_find
  let map_empty x = x#map_empty
  let map_remove x = x#map_remove
  let wf_map 
      ~(add:'k -> 'v -> 't -> 't) 
      ~(find:'k -> 't -> 'v)
      ~(empty:'t)
      ~(remove:'k -> 't -> 't)
    =
    true
end


open Profile

(* map from int to map from nt to nt_item_set *)
type 'a bitms_lt_k = 'a option array
(* 'a = map(nt -> nt_item_set) *)

let staged (type nt tm sym nt_item nt_item_set) 
    ~sym_case ~(_NT:nt->sym) 
    ~dot_nt ~dot_i ~dot_k ~dot_bs
    ~(nt_item_set_ops:[<`Nt_item_set_ops of 'a])
    ~(ixk_set_ops:[<`Ixk_set_ops of 'b])
    ~(map_nt_ops:[<`Map_nt_ops of 'c])
    ~(map_int_ops:[<`Map_int_ops of 'd])
    ~(map_tm_ops:[<`Map_tm_ops of 'e])
    ~(bitms_at_k:[<`Map_nt_nt_item_set_ops of 'e])
    ~(bitms_lt_k:'a option array) ~(bitms_lt_k_ops:[<`Bitms_lt_k_ops of 'f])
  = 
  ixk_set_ops |> function (`Ixk_set_ops ixk_set_ops) ->
  bitms_at_k |> function (`Map_nt_nt_item_set_ops bitms_at_k_ops) ->
  bitms_lt_k_ops |> function (`Bitms_lt_k_ops bitms_lt_k_ops) -> 
  nt_item_set_ops |> function (`Nt_item_set_ops nt_item_set_ops) ->
  map_tm_ops |> function (`Map_tm_ops map_tm_ops) ->
  let in_ctxt ctxt : int = 
    ctxt @@ begin fun ~new_items ~input ~parse_tm ~input_length 
      ~debug_enabled ~debug_endline ~init_nt -> 


      
      let cut: nt_item -> j_t -> nt_item = failwith "FIXME" in

      let add_bitm_at_k: nt_item -> nt -> nt_item_set -> nt_item_set = failwith "FIXME" in


    let add_todo ~todo_at_k ~todo_gt_k nt_item =
        (* FIXME *)
        let todo_at_k = todo_at_k in
        let todo_gt_k = todo_gt_k in
        (todo_at_k,todo_gt_k)
      in

      let bitms ~k ~bitms_lt_k ~bitms_at_k (i,x) = 
        if i = k 
        then bitms_at_k_ops.map_find x bitms_at_k 
        else bitms_lt_k_ops.map_find x bitms_lt_k
      in

      (* step_k ------------------------------------------------------- *)

      let rec step_k ~kk ~k ~bitms_lt_k ~bitms_at_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs = (
        debug_endline "XXXstep_k";
        assert(log P.ab);
        todo_at_k |> function
        | [] -> kk ~bitms_at_k ~todo_gt_k
        | nitm::todo_at_k -> 
          let nitm_complete = nitm|>dot_bs = [] in
          assert(log P.bc);  
          (* NOTE waypoints before each split and at end of branch *)
          nitm_complete 
          |> bool_case
            ~true_: (fun () ->
                let (i,x) = (nitm|>dot_i,nitm|>dot_nt) in
                (* possible NEW COMPLETE (i,X,k) *)
                let already_done = ixk_set_ops.mem (i,x) ixk_done in
                assert(log P.cd);
                already_done 
                |> bool_case
                  ~true_:(fun () -> 
                      debug_endline "already_done"; 
                      step_k ~kk ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs)
                  ~false_:(fun () -> 
                      debug_endline "not already_done";
                      let ixk_done = ixk_set_ops.add (i,x) ixk_done in
                      (* FIXME possible optimization if we work with Y ->
                           {h} as i X bs *)
                      bitms ~k ~bitms_lt_k ~bitms_at_k (i,x)
                      |> nt_item_set_ops.with_each_elt
                        ~f:(fun ~state:(todo_at_k,todo_gt_k) bitm -> 
                            add_todo ~todo_at_k ~todo_gt_k (cut bitm k))
                        ~init_state:(todo_at_k,todo_gt_k)
                      |> fun (todo_at_k,todo_gt_k) ->
                      assert(log P.de);
                      step_k ~kk ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs))
            ~false_: (fun () -> 
                (* NEW BLOCKED X -> i as k (S bs') on k S; here S is _Y or t *)
                let bitm = nitm in
                let s = List.hd (bitm|>dot_bs) in
                s 
                |> sym_case
                  ~nt:(fun _Y -> 
                      (* have we already processed k Y? *)
                      let bitms = bitms ~k ~bitms_lt_k ~bitms_at_k (k,_Y) in
                      let bitms_empty = nt_item_set_ops.is_empty bitms in
                      (* NOTE already_processed_kY = not bitms_empty *)
                      let bitms_at_k = add_bitm_at_k bitm _Y bitms_at_k in
                      assert(log P.fg);
                      bitms_empty 
                      |> bool_case
                        ~false_:(fun () -> 
                            (* already processed k Y, so no need to expand; but
                               may have complete item kYk *)
                            (* FIXME when dpes kYk get added to ixk_done? *)
                            debug_endline "not bitms_empty";
                            ixk_set_ops.mem (k,_Y) ixk_done
                            |> bool_case
                              ~true_:(fun () -> 
                                  add_todo ~todo_at_k ~todo_gt_k (cut bitm k)
                                  |> fun (todo_at_k,todo_gt_k) ->
                                  step_k ~kk ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs)
                              ~false_:(fun () -> 
                                  (* FIXME waypoint? *)
                                  step_k ~kk ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs))
                        ~true_:(fun () ->
                            (* we have not processed k Y; expand sym Y *)
                            debug_endline "bitms_empty";
                            assert (ixk_set_ops.mem (k,_Y) ixk_done = false);
                            new_items ~nt:_Y ~input ~k 
                            |> List_.with_each_elt
                              ~step:(fun ~state:(todo_at_k,todo_gt_k) nitm -> 
                                  add_todo ~todo_at_k ~todo_gt_k nitm)
                              ~init_state:(todo_at_k,todo_gt_k)
                            |> fun (todo_at_k,todo_gt_k) -> 
                            assert(log P.gh);
                            step_k ~kk  ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs))
                  ~tm:(fun t ->
                      (* have we already processed k T ? *)
                      map_tm_ops.map_find ktjs t |> fun js ->
                      assert(log P.hi);
                      js
                      |> begin function 
                        | None -> (
                            (* process k T *)
                            debug_endline "ktjs None";
                            debug_endline "processing k T";
                            let js = parse_tm ~tm:t ~input ~k ~input_length in
                            let ktjs = map_tm_ops.map_add t (Some js) ktjs in
                            (js,ktjs))
                        | Some js -> (
                            debug_endline "ktjs Some"; (js,ktjs)) 
                      end
                      |> fun (js,ktjs) -> 
                      assert(log P.ij);
                      (* process blocked; there is only one item blocked at
                         this point *)
                      js 
                      |> List_.with_each_elt
                        ~step:(fun ~state:(todo_at_k,todo_gt_k) j -> 
                            add_todo ~todo_at_k ~todo_gt_k (cut bitm j))
                        ~init_state:(todo_at_k,todo_gt_k)
                      |> fun (todo_at_k,todo_gt_k) -> 
                      assert(log P.jk);
                      step_k ~kk  ~k ~bitms_at_k ~bitms_lt_k ~todo_at_k ~todo_gt_k ~ixk_done ~ktjs))
      ) (*  step_k *)
      in


      let _ = step_k
      

      FIXME got here


      (* loop --------------------------------------------------------- *)
      (* outer loop: repeatedly process items at stage k, then move to stage
         k+1 *)
      let rec loop ~k ~bitms_at_k ~todo_gt_k = begin
        match k >= input_length with  (* correct? FIXME don't we have to go one further? *)
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
