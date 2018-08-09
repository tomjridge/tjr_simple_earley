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


open Earley_util.Misc

open Earley_util.Set_ops

open Earley_util.Map_ops



(*:nm:*)
module Make = functor (X:Earley_util.NEEDED_EXTENDED_INTERFACE) -> struct
  (*:np:*)
  open X
(*  open Profile *)

  type bitms_at_k = map_nt  (* bitms blocked at k,X *)
  let bitms_at_k_ops = map_nt_ops 

  (* state at k changes at k *)
  (* at stage k, there are items that have not been processed (todo
     items) and those that have already been processed (done items) *)
  type state = {
    k:k_t;  (* current stage *)
    todo:nt_item list;  (* todo items at stage k *)

    todo_done:nt_item_set;
    (* todo and done items at stage k; per k; used by add_todo to
       avoid adding an item more than once *)

    todo_gt_k:todo_gt_k;
    (* todo items at later stages *)
    (* impl: forall k' > k; empty for large k' > k_current and not
       needed for j<k *)

    (* blocked items are items that are "waiting" for some other parse
       to complete before they can continue; they are split into those at
       the current stage, bitms_at_k, and those at earlier stages,
       bitms_lt_k *)
    bitms_lt_k:bitms_lt_k;
    bitms_at_k:bitms_at_k;

    ixk_done:ixk_set;
    (* Completed items are of the form X -> i,as,k,[]; we record only
       i,X,k *)
    (* impl: per k; array (int/i) with values a set of nt?; set of nt
       implemented by binary upto 63/64 bits *)

    ktjs:map_tm;
    (* Terminals T require parsing the input from k; a successful
       terminal parse will match the input between position k and j; the
       corresponding terminal item is (k,T,j). This map stores those
       items (k,T,j) *)
    (* impl: per k; array (tm) with values a list of int *) 
  }

  (*:nq:*)

  let bitms ~bitms_lt_k_ops s0 (k,x) : nt_item_set = 
    match (k=s0.k) with
    | true -> (s0.bitms_at_k |> bitms_at_k_ops.map_find x)
    | false -> (s0.bitms_lt_k |> bitms_lt_k_ops.map_find k |> map_nt_ops.map_find x)

  (* nt_item blocked on nt at k FIXME nt is just the head of bs; FIXME
     order of nitm and nt (nt is the key) *)
  let add_bitm_at_k nitm nt s0 : state = 
    { s0 with
      bitms_at_k =
        let m = s0.bitms_at_k in
        let s = map_nt_ops.map_find nt m in
        let s' = nt_item_set_ops.add nitm s in
        let m' = map_nt_ops.map_add nt s' m in
        m' }

  (*:nr:*)

  let pop_todo s0 =
    match s0.todo with
    | x::xs -> (x,{s0 with todo=xs})
    | _ -> failwith "pop_todo"

  (* k is the current stage *)
  (* FIXME avoid cost of double lookup by using new ocaml sets with
     boolean rv *)
  let add_todo ~nt_item_ops nitm s0 : state = 
    let k = s0.k in
    let nitm_k = nitm|>(nt_item_ops.dot_k) in
    match nitm_k > k with
    | true -> 
      let nitms = todo_gt_k_ops.map_find nitm_k s0.todo_gt_k in
      let nitms = nt_item_set_ops.add nitm nitms in
      { s0 with todo_gt_k=(todo_gt_k_ops.map_add nitm_k nitms s0.todo_gt_k)}
    | false -> 
      (* NOTE this is todo_done at the current stage *)
      match nt_item_set_ops.mem nitm s0.todo_done with
      | true -> s0
      | false -> 
        { s0 with todo=(nitm::s0.todo);
                  todo_done=nt_item_set_ops.add nitm s0.todo_done}

  (*:ns:*)

  let add_ixk_done ix s0 : state =
    { s0 with ixk_done=(ixk_set_ops.add ix s0.ixk_done)}

  let mem_ixk_done ix s0 : bool =
    ixk_set_ops.mem ix s0.ixk_done 

  (*:nt:*)

  let find_ktjs t s0 : int list option =
    map_tm_ops.map_find t s0.ktjs

  (*:nu:*)

  let counter = ref 0


  let run_earley ~nt_item_ops ~bitms_lt_k_ops ~cut ~new_items ~input ~parse_tm 
      ~input_length ~init_nt = (

    (*:oc:*)

    let {dot_nt;dot_i;dot_k;dot_bs_hd} = nt_item_ops in
    let add_todo = add_todo ~nt_item_ops in
    let bitms = bitms ~bitms_lt_k_ops in

    (*:od:*)

    (* step_k ------------------------------------------------------- *)
    let step_k s0 = (
      debug_endline "XXXstep_k";
      (* assert(log P.ab); *)
      (*        let _ = (
                counter:=1 + !counter; 
                if (!counter mod 1000 = 0) then Gc.full_major() else () )
                in*)
      (* assert(log P.ac); *)
      let k = s0.k in    
      let bitms = bitms s0 in
      let (nitm,s0) = pop_todo s0 in
      let nitm_complete = nitm|>dot_bs_hd = None in
      (* assert(log P.bc);   *)
      (* NOTE waypoints before each split and at end of branch *)
      match nitm_complete with
      (*:oe:*)
      | true -> (
          let (i,x) = (nitm|>dot_i,nitm|>dot_nt) in
          (* possible new complete item (i,X,k) *)
          let already_done = mem_ixk_done (i,x) s0 in
          (* assert(log P.cd);           *)
          already_done |> function
            (*:of:*)
          | true -> (
              (* the item iXk has already been processed *)
              debug_endline "already_done"; 
              s0)
          (*:og:*)
          | false -> (
              (* a new complete item iXk *)
              debug_endline "not already_done";
              let s0 = add_ixk_done (i,x) s0 in
              (* FIXME possible optimization if we work with Y -> {h}
                   as i X bs *)
              bitms (i,x)
              |> nt_item_set_with_each_elt
                ~f:(fun ~state:s bitm -> add_todo (cut bitm k) s)
                ~init_state:s0
              |> fun s ->
              (* assert(log P.de); *)
              s))  
      (*:og:*)
      | false (* = nitm_complete *) -> (
          (* NEW BLOCKED item X -> i,as,k,S bs' on k S *)
          let bitm = nitm in
          let s = (bitm|>dot_bs_hd|>dest_Some) in
          s |> sym_case
            (*:oh:*)
            ~nt:(fun _Y -> 
                (* X -> i,as,k,Y bs'; check if kY is already done *)
                let bitms = bitms (k,_Y) in
                let bitms_empty = nt_item_set_ops.is_empty bitms in
                (* NOTE already_processed_kY = not bitms_empty *)
                (* NOTE the following line serves to record that we
                   are processing kY *)
                let s0 = add_bitm_at_k bitm _Y s0 in
                (* assert(log P.fg); *)
                bitms_empty |> function
                  (*:oi:*)
                | false -> (
                    (* kY has already been done, no need to expand;
                       but there may be a complete item kYk *)
                    debug_endline "not bitms_empty";
                    mem_ixk_done (k,_Y) s0 |> function
                    | true -> add_todo (cut bitm k) s0
                    | false -> s0)  (* FIXME waypoint? *)
                (*:oj:*)
                | true -> (
                    (* we need to expand Y at k; NOTE that there can
                       be no complete items kYk, because this is the
                       first time we have met kY *)
                    debug_endline "bitms_empty";
                    assert (mem_ixk_done (k,_Y) s0 = false);
                    new_items ~nt:_Y ~input ~k 
                    |> Earley_util.List_.with_each_elt 
                      ~step:(fun ~state:s nitm -> add_todo nitm s) 
                      ~init_state:s0
                    |> fun s -> 
                    (* assert(log P.gh); *)
                    s))
            (*:ok:*)
            ~tm:(fun t ->
                (* have we already processed kT ? *)
                find_ktjs t s0 |> fun ktjs ->
                (* assert(log P.hi); *)
                ktjs 
                |> (function 
                    (*:ol:*)
                    | None -> (
                        (* we need to process kT *)
                        debug_endline "ktjs None";
                        debug_endline "processing k T";
                        let js = parse_tm ~tm:t ~input ~k ~input_length in
                        let ktjs = map_tm_ops.map_add t (Some js) s0.ktjs in
                        (js,{s0 with ktjs}))
                    (*:om:*)
                    | Some js -> 
                      (* we have already processed kT *)
                      debug_endline "ktjs Some"; 
                      (js,s0))                  
                |> fun (js,s0) ->                 
                (* assert(log P.ij); *)
                (* cut (k,T,j) against the current item NOTE each item
                   that gets blocked on kT is immediately processed
                   against items kTj *)
                js 
                |> Earley_util.List_.with_each_elt
                  ~step:(fun ~state:s j -> add_todo (cut bitm j) s)
                  ~init_state:s0 
                |> fun s -> 
                (* assert(log P.jk); *)
                s))
    ) (* step_k *)
    in

    (*:or:*)
    (* loop_k: loop at k -------------------------------------------- *)

    let rec loop_k s0 = 
      match s0.todo with
      | [] -> s0
      | _ -> loop_k (step_k s0)
    in

    (*:pm:*)
    (* loop --------------------------------------------------------- *)

    (* outer loop: repeatedly process items at stage k, then move to
       stage k+1 *)
    let rec loop s0 = 
      match s0.k >= input_length with  
      (* correct? FIXME don't we have to go one further? *)
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
          bitms_lt_k_ops.map_add old_k s0.bitms_at_k s0.bitms_lt_k
        in
        let bitms_at_k = map_nt_ops.map_empty in
        (* FIXME let all_done = s0.todo_done::s0.all_done in *)
        let s1 = 
          {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k} in
        loop s1  
        (* end loop *)
    in

    (*:ps:*)

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
      let bitms_lt_k = bitms_lt_k_ops.map_empty in
      let bitms_at_k = bitms_at_k_ops.map_empty in
      (* let all_done = [] in *)
      let s0 = 
        {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k} in 
      loop s0
    in

    (*:pu:*)

    (* NOTE the result contains the todo_done items at stage
       l=|input|, including any complete items; if we have a complete
       item X -> i,as S,j,[] then we know it arose from a blocked item
       X -> i,as,k,S and a complete item (k,S,j); FIXME so when
       cutting items we at least need to record a map from (S,j) ->
       k 

       We could also just retain the todo_done(k) sets, since these
       detail all (complete) items, but then we would have to process
       these sets which might be expensive. *)
    result
  ) (* run_earley *)

end (* Make *)
(*:py:*)
