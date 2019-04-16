(** An experiment to see whether the imperative code (represented using
   a monad) is easier to read; probably it is. *)

module type A = sig

  type i_t = int  
  type k_t = int
  type j_t = int

  type nt
  type tm
  type sym

  type nt_item  

  type nt_item_set

  (* when we increase k, we need to alter the state significantly;
     this reveals the structure of the state *)

  (** nt -> nt_item_set *)
  type bitms_at_k 

  (** int -> bitms_at_k *)
  type bitms_lt_k  

  (** NOTE following are per k *)

  (** int*nt set *)
  type ixk_done  

  (** tm -> j list option *)
  type ktjs  

end



module Make(A:A) = struct

  (* to make doc self-contained *)
  include A

  type item_ops = {
    sym_case: 'a. nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a;
    _NT: nt -> sym;
    dot_nt: nt_item -> nt;
    dot_i: nt_item -> i_t;
    dot_k: nt_item -> k_t;
    dot_bs: nt_item -> sym list;
    cut: nt_item -> j_t -> nt_item;
    elements : nt_item_set -> nt_item list
  }


  (* NOTE a map from int, so we can implement straightforwardly *)
  type todo_gt_k = (int,nt_item_set) Hashtbl.t

  let todo_gt_k_find: int -> todo_gt_k -> nt_item_set = failwith "FIXME"

  type state = {
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: todo_gt_k;
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    ixk_done: ixk_done;
    ktjs:ktjs
  }

  type 'a m = state -> 'a * state
  let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
    fun s ->
      a s |> fun (a,s) -> 
      ab a s
  let _ = ( >>= )
  let return a = fun s -> (a,s)


  let with_state: (state -> state) -> unit m = fun f -> 
    fun s -> ((),f s)

  (* FIXME these expose the state type via _ m *)
  type atomic_operations = {
    (* FIXME this op is already known now we know 'a m *)
    pop_todo: unit -> nt_item option m;

    (* following three could be implemented as funs t -> t, then lifted to the monad *)
    get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
    get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
    add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)

    (* FIXME following two could be implemented here, since we know the state impl type *)
    add_todos_at_k: nt_item list -> unit m;
    add_todos_gt_k: nt_item list -> unit m;

    (* FIXME these, and others, could be implemented outside the monad
       as a funtion t->t, then injected into the monad now we know
       what 'a m is *)
    add_ixk_done: int*nt -> unit m;
    mem_ixk_done: int*nt -> bool m;

    find_ktjs: tm -> int list option m;
    add_ktjs: tm -> int list -> unit m;
  }


  type state_ops = {
    update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k; (* FIXME what for? *)
    empty_bitms_at_k: bitms_at_k;
    empty_ixk_done: ixk_done;
    empty_ktjs: ktjs;
  }


  module Internal2 = struct


    (* profiling; debugging --------------------------------------------- *)

    (* don't want to depend on other libs at this point *)
    let _mark_ref = ref (fun (i:int) -> ())
    let mark x = x |> !_mark_ref

    (* main ------------------------------------------------------------- *)

    let run_earley ~item_ops ~state_ops ~at_ops = 
      let { sym_case; _NT; dot_nt; dot_i; dot_k; dot_bs; cut; elements } =
        item_ops 
      in
      let { update_bitms_lt_k; empty_bitms_at_k;
            empty_ixk_done; empty_ktjs } = state_ops 
      in
      let { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
            add_todos_at_k; add_todos_gt_k; add_ixk_done;
            mem_ixk_done; find_ktjs; add_ktjs } = at_ops
      in
      let is_finished nitm = nitm|>dot_bs = [] in
      let module Let_syntax = struct 
        let bind a ~f = a >>= f 
      end
      in
      fun ~new_items ~input ~parse_tm ~input_length ->
        begin

          (* 

Explanation of step_at_k code which follows:

The basic Earley step is:

X -> i as k',S bs     k' S k
----------------------------
X -> i as S k bs

In the code, there are labels of the form (*:am:*). The following
discussion is indexed by these labels

- af: 
  - the item nitm is complete, ie of the form Y -> k',as,k,[]
  - aj,al: has (k',Y,k) been encountered before? if so, do nothing
  - am: if not encountered before, k' Y k is cut with blocked X ->
    ... and new todo items are added

- ax: 
  - item is not complete ie of form _ -> i,as,k,S bs

- ax/ce: 
  - S is nonterm Y
  - add bitm to blocked items at (k,Y)
  - check if we have seen (k,Y) before (bitms_empty)
  - co: if we have, check if k Y k; cut bitm with k Y k if so
  - cw: if we haven't, generate new items from (k,Y)

- ax/ec:
  - S is terminal tm
  - attempt to retrieve (k,tm,j) set from ktjs
  - ek: if we haven't already met (k,tm) then parse (k,tm), update
    ktjs and pass on js
  - otherwise, just reuse js from previously
  - el: given the set of js (which are all >= k)
  - partition into >k, and =k
  - for j > k, cut bitm with j, and add to todos
    - note that if this is the first time we meet (k,tm), then there
      are no other items blocked on (k,tm); if this is not the first
      time, then we have already processed items blocked on (k,tm); in
      either case, we do not need to do anything more with items
      blocked on (k,tm); in fact, we don't even need to record such
      items
  - em: if k is in js (ie tm matched the empty string) cut bitm with k

*)
          let (^) = List.map in

          let step_at_k k nitm = 
            mark __LINE__;
            let get_bitms (i,x) =
              if i=k then get_bitms_at_k x else
                get_bitms_lt_k (i,x)
            in

            match is_finished nitm with 
            | true -> (                                               (*:af:*)
                mark __LINE__;
                let (k',_Y) = (nitm|>dot_i,nitm|>dot_nt) in  
                let%bind already_done = mem_ixk_done (k',_Y) in           (*:aj:*)
                mark __LINE__;
                match already_done with     
                | true -> mark __LINE__; return ()                                   (*:al:*)
                | false -> (                                          (*:am:*)
                    mark __LINE__;                
                    add_ixk_done (k',_Y) >>= fun _ ->                   
                    mark __LINE__;
                    get_bitms (k',_Y) >>= fun bitms ->                  
                    mark __LINE__;
                    add_todos_at_k ((fun bitm -> cut bitm k) ^ bitms) >>= fun _ ->
                    mark __LINE__; return ()))
            | false -> (                                              (*:ax:*)
                mark __LINE__;
                let bitm = nitm in   
                let _S = List.hd (bitm|>dot_bs) in 
                _S |> sym_case  
                  ~nt:(fun _Y ->                                      (*:ce:*)
                      mark __LINE__;
                      get_bitms_at_k _Y >>= fun bitms ->     
                      mark __LINE__;
                      let bitms_empty = bitms=[] in     
                      add_bitm_at_k bitm _Y >>= fun _ ->     
                      mark __LINE__;
                      match bitms_empty with  
                      | false -> (                                    (*:co:*)
                          mark __LINE__;
                          mem_ixk_done (k,_Y) >>= function    
                          | true -> 
                            add_todos_at_k [cut bitm k] >>= fun _ ->
                            mark __LINE__;
                            return ()
                          | false -> return ())    
                      | true -> (                                     (*:cw:*)
                          mark __LINE__;
                          let itms = new_items ~nt:_Y ~input ~k in
                          add_todos_at_k itms >>= fun _ ->
                          mark __LINE__;
                          return ()
                        ))  
                  ~tm:(fun tm ->                                      (*:ec:*)
                      mark __LINE__;
                      find_ktjs tm >>= fun ktjs ->     
                      (match ktjs with
                       | None -> (
                           (* we need to process kT *)                (*:ek:*)
                           let js = parse_tm ~tm ~input ~k ~input_length in 
                           add_ktjs tm js >>= fun _ ->  
                           return js) 
                       | Some js -> return js) >>= fun js -> 
                      (* there may be a k in js, in which case we have a 
                         new todo at the current stage *)
                      let (xs,js) = List.partition (fun j -> j=k) js in (*:el:*)
                      add_todos_gt_k ((fun j -> cut bitm j) ^ js) >>= fun _ ->
                      match xs with                                   (*:em:*)
                      | [] -> return ()     
                      | _ -> add_todos_at_k [cut bitm k]))
          in 


          (* FIXME monad syntax may make this easier to read *)
          let rec loop_at_k k = 
            (* print_endline "loop_at_k"; *)
            pop_todo () >>= function
            | None -> return ()
            | Some itm -> step_at_k k itm >>= fun _ -> loop_at_k k
          in

          let rec loop k = 
            (* Printf.printf "loop %d\n" k; *)
            match k >= input_length with  
            (* correct? FIXME don't we have to go one further? *)
            | true -> return ()
            | false -> 
              (* process items *)
              loop_at_k k >>= fun _ ->
              let k' = k+1 in
              (* 
           todo and todo_done are updated with todo_gt_k[k'];
           bitms_lt_k is updated: bitms_lt_k[k]=bitms_at_k
           bitms_at_k is reset;
           ixk_done and ktjs are reset *)
              with_state (fun s ->
                  let todo' = todo_gt_k_find k' s.todo_gt_k in
                  let todo = elements todo' in
                  (* Printf.printf "elements: %d" (List.length todo); *)
                  { todo;
                    todo_done=todo';
                    todo_gt_k=s.todo_gt_k;
                    bitms_lt_k=(update_bitms_lt_k k s.bitms_at_k s.bitms_lt_k);
                    bitms_at_k=empty_bitms_at_k;
                    ixk_done=empty_ixk_done;
                    ktjs=empty_ktjs;
                  }) >>= fun _ ->
              loop k'
          in


          loop 0
        end (* run_earley *)

    end

  module Export : sig
    type earley_parser
    val make_earley_parser: 
      item_ops:item_ops ->
      state_ops:state_ops ->
      at_ops:atomic_operations ->
      earley_parser

    val _run_earley_parser:
      earley_parser:earley_parser -> 
      new_items:(nt:nt -> input:'a -> k:i_t -> nt_item list) ->
      input:'a ->
      input_length:i_t -> 
      parse_tm:(tm:tm -> input:'a -> k:i_t -> input_length:i_t -> i_t list) ->
      unit m

    (** returns the final state *)
    val run_earley_parser:
      earley_parser:earley_parser -> 
      new_items:(nt:nt -> input:'a -> k:i_t -> nt_item list) ->
      input:'a ->
      input_length:i_t -> 
      parse_tm:(tm:tm -> input:'a -> k:i_t -> input_length:i_t -> i_t list) ->
      initial_items:nt_item list ->
      state
  end = struct
    type earley_parser = {
      run_parser: 'a. new_items:(nt:nt -> input:'a -> k:i_t -> nt_item list) ->
      input:'a ->
      parse_tm:(tm:tm -> input:'a -> k:i_t -> input_length:i_t -> i_t list) ->
      input_length:i_t -> unit m
    }

    let make_earley_parser ~item_ops ~state_ops ~at_ops =
      {run_parser=fun ~new_items -> Internal2.run_earley ~item_ops ~state_ops ~at_ops ~new_items}

    let _run_earley_parser ~earley_parser 
        ~new_items
        ~input 
        ~input_length
        ~parse_tm
      = earley_parser.run_parser
        ~new_items
        ~input 
        ~input_length
        ~parse_tm

    let run_earley_parser ~earley_parser
        ~new_items
        ~input 
        ~input_length
        ~parse_tm
        ~initial_items =
      let initial_state = failwith "" in
      _run_earley_parser ~earley_parser 
        ~new_items
        ~input 
        ~input_length
        ~parse_tm
        initial_state
      |> fun ((),s) -> s
  end

  include Export

end
