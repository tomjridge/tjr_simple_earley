(* an experiment to see whether the imperative code (represented using
   a monad) is easier to read; probably it is *)


let now () = Core.Time_stamp_counter.(
    now () |> to_int63 |> Core.Int63.to_int |> Tjr_profile.dest_Some)

let Tjr_profile.{mark;get_marks} = Tjr_profile.mk_profiler ~now
open Tjr_profile.P


module type M_ = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
end


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

  val cut: nt_item -> j_t -> nt_item

  type nt_item_set
  val elements : nt_item_set -> nt_item list
end


module type STATE = sig  
  type nt_item
  type nt_item_set

  (* when we increase k, we need to alter the state significantly;
     this reveals the structure of the state *)

  type todo_gt_k  (* int -> nt_item_set *)
  type bitms_lt_k  (* int -> bitms_at_k *)
  type bitms_at_k  (* nt -> nt_item_set *)

  (* following are per k *)
  type ixk_done  (* int*nt set *)
  type ktjs  (* tm -> j list option *)

  type state = {
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: todo_gt_k;
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    ixk_done: ixk_done;
    ktjs:ktjs
  }

  val todo_gt_k_find: int -> todo_gt_k -> nt_item_set
  val update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k
  val empty_bitms_at_k: bitms_at_k
  val empty_ixk_done: ixk_done
  val empty_ktjs: ktjs
end


module Make = functor 
  (S:S_)
  (State:STATE with type nt_item = S.nt_item and type nt_item_set = S.nt_item_set)
  (M:M_) -> 
struct
  (*:np:*)
  open S
  open M

  type atomic_operations = {
    get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
    get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
    add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
    pop_todo: unit -> nt_item option m;
    add_todos_at_k: nt_item list -> unit m;
    add_todos_gt_k: nt_item list -> unit m;
    add_ixk_done: int*nt -> unit m;
    mem_ixk_done: int*nt -> bool m;
    find_ktjs: tm -> int list option m;
    add_ktjs: tm -> int list -> unit m;
    with_state: (State.state -> State.state) -> unit m;
  }

  let is_finished nitm = nitm|>dot_bs = [] 


  let run_earley ~at_ops ~new_items ~input ~parse_tm ~input_length = 
    begin

      let { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo; add_todos_at_k; add_todos_gt_k; 
            add_ixk_done; mem_ixk_done; find_ktjs; add_ktjs; with_state } = at_ops 
      in
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
            mem_ixk_done (k',_Y) >>= fun already_done ->          (*:aj:*)
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
          let open State in
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



(* example instance ------------------------------------------------- *)

open Tjr_monad
open Tjr_monad.Monad


module M = struct
  open Tjr_monad.Imperative_instance
  type 'a m = ('a,imperative) Monad.m
  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind  
end

module X1 = (M:M_)

(* Implement the signature required by the Earley code *)
module S = struct
  type i_t = int
  type k_t = int
  type j_t = int

  (* NOTE we need to be able to distinguish nonterminals from terminals *)
  type nt = int (* even, say *)
  type tm = int (* odd, say *)
  type sym = int
  let even x = (x mod 2 = 0)
  let sym_case ~nt ~tm sym = 
    if even sym then nt sym else tm sym

  let _NT: nt -> sym = fun x -> x

  type nt_item = { nt:nt; i_:i_t; k_:k_t; bs:sym list }

  (* Implement the accessor functions by using simple arithmetic *)
  let dot_nt nitm = nitm.nt
  let dot_i nitm = nitm.i_
  let dot_k nitm = nitm.k_
  let dot_bs nitm = nitm.bs

  let cut : nt_item -> j_t -> nt_item = 
    fun bitm j0 -> 
      { bitm with k_=j0; bs=(List.tl bitm.bs)}
  type cut = nt_item -> j_t -> nt_item


  type nt_item_ops = {
    dot_nt: nt_item -> nt;
    dot_i: nt_item -> i_t;
    dot_k: nt_item -> k_t;
    dot_bs: nt_item -> sym list;
  }

  module Set_nt_item = Set.Make(
    struct 
      type t = nt_item 
      let compare : t -> t -> int = Pervasives.compare 
    end)
  type nt_item_set = Set_nt_item.t
  let elements = Set_nt_item.elements
end  

module X2 = (S:S_)


module State = struct
  type nt_item = S.nt_item
  type nt_item_set = S.nt_item_set

  module Todo_gt_k = Tjr_map.Map_int
  type todo_gt_k = nt_item_set Todo_gt_k.t

  module Bitms_at_k = Map.Make(
    struct 
      type t = S.nt
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type bitms_at_k = nt_item_set Bitms_at_k.t

  module Bitms_lt_k = Map.Make(
    struct 
      type t = int 
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type bitms_lt_k = bitms_at_k Bitms_lt_k.t
      
  module Ixk_done = Set.Make(
    struct 
      type t = int * S.nt 
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type ixk_done = Ixk_done.t

  module Ktjs = Map.Make(
    struct
      type t = S.tm                   
      let compare: t -> t -> int = Pervasives.compare 
    end)
  type ktjs = S.j_t list option Ktjs.t

  type state = {
    todo: nt_item list;
    todo_done: nt_item_set;
    todo_gt_k: todo_gt_k;
    bitms_lt_k: bitms_lt_k;
    bitms_at_k: bitms_at_k;
    ixk_done: ixk_done;
    ktjs:ktjs
  }


  let todo_gt_k_find k t = 
    try 
      Todo_gt_k.find k t
    with _ -> S.Set_nt_item.empty

  let update_bitms_lt_k : int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k =
    fun i at lt ->
    Bitms_lt_k.add i at lt

  let empty_bitms_at_k = Bitms_at_k.empty

  let empty_ixk_done = Ixk_done.empty

  let empty_ktjs = Ktjs.empty      

  let empty_state = {
    todo=[];
    todo_done=S.Set_nt_item.empty;
    todo_gt_k=Todo_gt_k.empty;
    bitms_lt_k=Bitms_lt_k.empty;
    bitms_at_k=empty_bitms_at_k;
    ixk_done=empty_ixk_done;
    ktjs=empty_ktjs
  }
  
end

module X3 = (State:STATE)

module Earley = Make (S)(State)(M)

open M
open S
open State

open Earley

let to_m = Imperative_instance.to_m

let run_earley ~state =
  let s = state in
  let get_bitms_at_k = fun nt -> 
    to_m (
      try
        Bitms_at_k.find nt !s.bitms_at_k |> elements
      with _ -> [])
  in
  let get_bitms_lt_k = fun (i,x) ->
    (
      let bitms = 
        try
          Bitms_lt_k.find i !s.bitms_lt_k
        with _ -> Bitms_at_k.empty
      in
      try 
        Bitms_at_k.find x bitms
      with _ -> Set_nt_item.empty)
    |> elements
    |> to_m
  in
  let add_bitm_at_k = fun itm nt ->
    to_m (
      let s_ = !s in
      let itms = 
        try Bitms_at_k.find nt s_.bitms_at_k with _ -> Set_nt_item.empty
      in
      let itms = Set_nt_item.add itm itms in
      s:={s_ with bitms_at_k=Bitms_at_k.add nt itms s_.bitms_at_k};
      ())      
  in
  let pop_todo = fun () ->
    to_m (
      match !s.todo with
      | [] -> None
      | x::xs -> 
        s:={!s with todo=xs};
        Some x)
  in
  let add_todos_at_k itms = 
    to_m (
      let todo_done = !s.todo_done in
      let itms = itms |> List.filter (fun itm -> not (Set_nt_item.mem itm todo_done)) in
      s:={!s with todo=itms@ !s.todo; 
                  todo_done=Set_nt_item.union (Set_nt_item.of_list itms) (!s.todo_done)};
      ())
  in
  let add_todos_gt_k itms = 
    to_m (
      itms |> List.iter (fun itm ->
          let todo_gt_k = !s.todo_gt_k in
          let k' = itm.k_ in 
          let set = 
            try 
              Todo_gt_k.find k' todo_gt_k
            with _ -> Set_nt_item.empty
          in
          let set = Set_nt_item.add itm set in
          let todo_gt_k = Todo_gt_k.add k' set todo_gt_k in
          s:={!s with todo_gt_k});
      ())
  in
  let add_ixk_done (i,x) =
    to_m (
      s:={!s with ixk_done=Ixk_done.add (i,x) !s.ixk_done};
      ())
  in
  let mem_ixk_done (i,x) =
    to_m (
      Ixk_done.mem (i,x) !s.ixk_done)
  in
  let find_ktjs tm = 
    to_m (
      try
        Ktjs.find tm !s.ktjs
      with _ -> None)
  in
  let add_ktjs tm ints = 
    to_m (
      s:={!s with ktjs=Ktjs.add tm (Some ints) !s.ktjs})
  in
  let with_state f =
    to_m (
      s:=f !s)
  in

  let at_ops = {
    get_bitms_at_k;
    get_bitms_lt_k;
    add_bitm_at_k;
    pop_todo;
    add_todos_at_k;
    add_todos_gt_k;
    add_ixk_done;
    mem_ixk_done;
    find_ktjs;
    add_ktjs;
    with_state
  }
  in

  run_earley ~at_ops

let _ :
state:State.state ref ->
new_items:(nt:S.nt -> input:'a -> k:S.j_t -> S.nt_item list) ->
input:'a ->
parse_tm:(tm:S.tm -> input:'a -> k:S.j_t -> input_length:S.j_t -> S.j_t list) ->
input_length:S.j_t -> unit M.m
= run_earley




(* Encode nonterminals and terminals as ints; nts are even; tms are
   odd *)

let _E = 0
let eps = 1
let _1 = 3

(* Encode the grammar E -> E E E | "1" | eps *)
let rhss = [ [_E;_E;_E]; [_1]; [eps] ]

(* Provide a function that produces new items, given a nonterminal and
   an input position k *)
let new_items ~nt ~input ~k = match () with
  | _ when nt = _E -> 
    (* print_endline __LOC__; *)
    rhss   (* E -> E E E | "1" | eps *)
    |> List.map (fun bs -> { nt; i_=k; k_=k; bs})
  | _ -> failwith __LOC__

(* Example input; use command line argument *)
let input = String.make (Sys.argv.(1) |> int_of_string) '1'

(* Provide a function that details how to parse terminals at a given
   position k in the input *)
let parse_tm ~tm ~input ~k ~input_length = 
  match () with
  | _ when tm = eps -> [k]
  | _ when tm = _1 -> 
    (* print_endline (string_of_int k); *)
    if String.get input k = '1' then [k+1] else []
  | _ -> failwith __LOC__

let input_length = String.length input

(* Initial nonterminal *)
let init_nt = _E

(* E -> E *)
let state = 
  let todo = [{nt=_E;i_=0;k_=0;bs=[_E]}] in
  ref { State.empty_state with todo }

let () = 
  run_earley 
    ~state
    ~new_items
    ~input
    ~parse_tm
    ~input_length |> Imperative_instance.from_m

(* result is the final state *)




(*  
time ./a.out 400

real	0m6.166s
user	0m6.156s
sys	0m0.012s

very similar to simple_test.native

*)

let _ = 
  let open Tjr_profile in
  get_marks () |> print_profile_summary


(* profile info

$ experimental $ ./a.out 200
Time:0  198 198 count:1
Time:14088  191 202 count:200
Time:48124  205 158 count:200
Time:59311  181 209 count:400
Time:76791  194 158 count:200
Time:1359537  209 158 count:400
Time:2848920  158 166 count:20100
Time:3095653  202 205 count:200
Time:3147639  169 173 count:20100
Time:5157269  158 181 count:40801
Time:6673086  191 194 count:40201
Time:7737012  179 158 count:20100
Time:8421932  181 186 count:40401
Time:16927145  166 169 count:20100
Time:18279231  198 158 count:40000
Time:34573133  173 175 count:20100
Time:66783285  188 191 count:40401
Time:73524495  194 198 count:40001
Time:132225487  175 177 count:20100
Time:191734699  186 188 count:40401
Time:2174587455  177 179 count:20100


*)
