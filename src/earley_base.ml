(** Internal Earley implementation; see {!Earley_simple} for the
    external usable version.

    This is the main (internal) Earley implementation, based on
   processing items at index k in the input, before moving to
   k+1. This version of the code tries to assume as little as possible
   about the representation of the underlying structures.  *)

open Earley_intf


(** What is required by the [Make] functor *) 
module type REQUIRED_BY_BASE = sig

  type i_t = int  
  type k_t = int
  type j_t = int

  type nt
  type tm
  type sym

  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym
  val _TM: tm -> sym

  type nt_item  
  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> i_t
  val dot_k: nt_item -> k_t
  val dot_bs: nt_item -> sym list
  val cut: nt_item -> j_t -> nt_item
  val mk_nt_item : nt -> int -> sym list -> nt_item

  type nt_item_set
  val empty_nt_item_set: nt_item_set
  val elements : nt_item_set -> nt_item list
  val nt_item_set_of_list: nt_item list -> nt_item_set

  (** int -> bitms_at_k  FIXME implement as hashtbl *)
  type bitms_lt_k  

  (* int -> nt_item_set *)
  type todo_gt_k 


  (** NOTE following are per k *)

  (** nt -> nt_item_set *)
  type bitms_at_k 

  (** (int*nt) set *)
  type ixk_done  

  (** tm -> j list option *)
  type ktjs  


  val todo_gt_k_find: int -> todo_gt_k -> nt_item_set
  val update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k (* FIXME what for? *)

  val empty_todo_gt_k: todo_gt_k
  val empty_bitms_lt_k: bitms_lt_k
  val empty_bitms_at_k: bitms_at_k
  val empty_ixk_done: ixk_done
  val empty_ktjs: ktjs

end  (* REQUIRED_BY_BASE *)


(** Construct the Earley parsing implementation *)
module Make(A:REQUIRED_BY_BASE) = struct

  (** {2 Provided by user} *)

  (* to make doc self-contained *)
  include A

  (** {2 Content of Make proper starts here} *)

  type state = {
    count: int;
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: todo_gt_k;
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    ixk_done: ixk_done;
    ktjs:ktjs;
    (* cuts:cuts *)
  }


  let empty_state = {
    count=0;
    todo=[];
    todo_done=empty_nt_item_set;
    todo_gt_k=empty_todo_gt_k;
    bitms_lt_k=empty_bitms_lt_k;
    bitms_at_k=empty_bitms_at_k;
    ixk_done=empty_ixk_done;
    ktjs=empty_ktjs;
    (* cuts=empty_cuts *)
  }
    

  type 'a m = state -> 'a * state
  let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
    fun s ->
      a s |> fun (a,s) -> 
      ab a s
  let _ = ( >>= )
  let return a = fun s -> (a,s)


  (* FIXME these expose the state type via _ m *)
  (** 
     - pop_todo can be implemented directly
     - get_bitms_at_k needs bitms_at_k, and a way to map nt_item_set to list (which we have: elements)
     - add_bitm_at_k is fine
     - add_todos_at_k is fine
     - add_todos_gt_k is fine
     - rest are fine

     NOTE if we use a mutable impl of state, we can avoid having to
     lookup via nt, update set, and update via nt
  *)
  type atomic_operations = {
    pop_todo: unit -> nt_item option m;
    (* FIXME this op is already known now we know 'a m *)

    get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
    get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
    add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
    (* these three could be implemented as funs t -> t, then lifted to the monad *)

    add_todos_at_k: nt_item list -> unit m;
    add_todos_gt_k: nt_item list -> unit m;
    (* FIXME these two could be implemented here, since we know the state impl type *)

    add_ixk_done: int*nt -> unit m;
    mem_ixk_done: int*nt -> bool m;
    (* FIXME these, and others, could be implemented outside the monad
       as a funtion t->t, then injected into the monad now we know
       what 'a m is *)

    find_ktjs: tm -> int list option m;
    add_ktjs: tm -> int list -> unit m;

    (* record_cuts: (nt_item * int) list -> unit m; *)
  }


  (** Hide the following defns from the user *)
  module Internal2 = struct

    let run_earley (*~item_ops*) ~at_ops = 
      let { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
            add_todos_at_k; add_todos_gt_k; add_ixk_done;
            mem_ixk_done; find_ktjs; add_ktjs } = at_ops
      in
      let with_state: (state -> state) -> unit m = fun f -> 
        fun s -> ((),f s)
      in
      let image = List.map in
      let is_finished nitm = nitm|>dot_bs = [] in
      let module Let_syntax = struct 
        let bind a ~f = a >>= f 
      end
      in
      fun ~grammar ~parse_tm ~input ->
        let mark = fun (_s:string) -> () in (* FIXME *)
        let {parse_tm}=parse_tm in
        let {nt_input_to_rhss}=grammar in
        let new_items ~nt ~input ~pos = 
          nt_input_to_rhss ~nt ~input ~pos |> fun rhss -> 
          rhss |> List.map (fun rhs -> 
              let rhs = rhs |> List.map (function Nt nt -> _NT nt | Tm tm -> _TM tm) in
              mk_nt_item nt pos rhs)
        in
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

          let step_at_k k nitm = 
            mark "aa";
            let get_bitms (i,x) =
              if i=k then get_bitms_at_k x else
                get_bitms_lt_k (i,x)
            in

            match is_finished nitm with 
            | true -> (                                                       (*:af:*)
                mark "af";
                let (k',_Y) = (nitm|>dot_i,nitm|>dot_nt) in  
                let%bind already_done = mem_ixk_done (k',_Y) in               (*:aj:*)
                mark "ak";
                match already_done with     
                | true -> mark "al"; return ()                                (*:al:*)
                | false -> (                                                  (*:am:*)
                    mark "am";                
                    add_ixk_done (k',_Y) >>= fun _ ->                   
                    mark "ap";
                    get_bitms (k',_Y) >>= fun bitms ->                  
                    mark "ar";
                    (* record_cuts (List.map (fun bitm -> (bitm,k)) bitms) >>= fun _ -> *)
                    (* mark "as"; *)
                    (* NOTE the following image is guaranteed not to
                       contain duplicates... (?) *)
                    let new_todos_at_k = image (fun bitm -> cut bitm k) bitms in
                    (* Printf.printf "debug: %d %d\n%!" (List.length bitms) (nt_item_set_of_list new_todos_at_k |> elements |> List.length); *)
                    mark "at";
                    add_todos_at_k new_todos_at_k >>= fun _ ->
                    mark "au"; return ()))
            | false -> (                                                      (*:ax:*)
                mark "ax";
                let bitm = nitm in   
                let _S = List.hd (bitm|>dot_bs) in 
                _S |> sym_case  
                  ~nt:(fun _Y ->                                              (*:ce:*)
                      mark "ce";
                      get_bitms_at_k _Y >>= fun bitms ->     
                      mark "ch";
                      let bitms_empty = bitms=[] in     
                      add_bitm_at_k bitm _Y >>= fun _ ->     
                      mark "ck";
                      match bitms_empty with  
                      | false -> (                                            (*:co:*)
                          mark "co";
                          mem_ixk_done (k,_Y) >>= function    
                          | true -> 
                            (* record_cuts [(bitm,k)] >>= fun _ -> *)
                            add_todos_at_k [cut bitm k] >>= fun _ ->
                            mark "cr";
                            return ()
                          | false -> return ())    
                      | true -> (                                             (*:cw:*)
                          mark "cw";
                          let itms = new_items ~nt:_Y ~input ~pos:k in
                          add_todos_at_k itms >>= fun _ ->
                          mark "cz";
                          return ()
                        ))  
                  ~tm:(fun tm ->                                              (*:ec:*)
                      mark "ec";
                      find_ktjs tm >>= fun ktjs ->     
                      (match ktjs with
                       | None -> (
                           (* we need to process kT *)                        (*:ek:*)
                           let js = parse_tm ~tm ~input ~pos:k in 
                           add_ktjs tm js >>= fun _ ->  
                           return js) 
                       | Some js -> return js) >>= fun js -> 
                      (* there may be a k in js, in which case we have a 
                         new todo at the current stage *)
                      let (xs,js) = List.partition (fun j -> j=k) js in       (*:el:*)
                      (* record_cuts (List.map (fun j -> (bitm,j)) js) >>= fun _ ->  *)
                      add_todos_gt_k (image (fun j -> cut bitm j) js) >>= fun _ ->
                      match xs with                                           (*:em:*)
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
            match k > input.input_length with  
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
                  { count=s.count;
                    todo;
                    todo_done=todo';
                    todo_gt_k=s.todo_gt_k;
                    bitms_lt_k=(update_bitms_lt_k k s.bitms_at_k s.bitms_lt_k);
                    bitms_at_k=empty_bitms_at_k;
                    ixk_done=empty_ixk_done;
                    ktjs=empty_ktjs;
                    (* cuts=s.cuts; *)
                  }) >>= fun _ ->
              loop k'
          in


          loop 0
        end (* run_earley *)

    let _ : 
at_ops:atomic_operations ->
grammar:(nt, tm, 'a) input_dependent_grammar ->
parse_tm:(tm, 'a) terminal_input_matcher -> input:'a input -> unit m
= run_earley

  end  (* Internal2 *)


  module Export : sig
    
    (** Abstract type of Earley parsers *)
    type earley_parser

    (** Construct a generic Earley parser (independent of grammar) *)
    val make_earley_parser: 
      at_ops:atomic_operations ->
      earley_parser

    (** Execute the Earley parser on a given grammar and
       input. Returns the final state. We don't mind exposing the
       state type because this is an internal library, intended to be
       used eg by {!Earley_simple}. *)
    val run_earley_parser:
      earley_parser:earley_parser -> 
      grammar:(nt, tm, 'a) input_dependent_grammar ->
      parse_tm:(tm, 'a) terminal_input_matcher -> 
      input:'a input -> 
      initial_state:state ->
      state
  end = struct
    type earley_parser = {
      run_parser: 
        'a. grammar:(nt, tm, 'a) input_dependent_grammar ->
        parse_tm:(tm, 'a) terminal_input_matcher -> 
        input:'a input -> 
        unit m
    }

    let make_earley_parser ~at_ops =
      {run_parser=(fun ~grammar ~parse_tm ~input -> 
           Internal2.run_earley ~at_ops ~grammar ~parse_tm ~input)}

    let _run_earley_parser ~earley_parser ~grammar ~parse_tm ~input = 
      earley_parser.run_parser ~grammar ~parse_tm ~input

    (**FIXME we can hide the earley_parser type, and just return a
       function that takes grammar_etc...; finally, we can return
       interesting parts of the state separately, so that the
       functionality does not depend on any types we define above *)
    let run_earley_parser ~earley_parser ~grammar ~parse_tm ~input ~initial_state =
      _run_earley_parser ~earley_parser ~grammar ~parse_tm ~input initial_state
      |> fun ((),s) -> s
  end

  include Export

end
