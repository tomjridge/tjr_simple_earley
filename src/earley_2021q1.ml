(** Quick reimplementation 2021q1, following my current preferred
   coding style, with mutable datastructures. Self-contained single
   file implementation. Unstaged, efficient O(n^3). *)

let dont_log = true

(** {2 Sets, maps implementation} *)

type 'e set = {
  add      : 'e -> unit;
  mem      : 'e -> bool;
  add_list : 'e list -> unit;
  to_list  : unit -> 'e list;
}

type ('k,'v) map_to_set = {
  find: 'k -> 'v set; (* defaults to empty *)
}


(** Mutable set and map implementations based on hashtables *)

let make_set () = 
  let tbl = Hashtbl.create 10 in
  let add e = Hashtbl.replace tbl e () in
  let mem e = Hashtbl.mem tbl e in
  let add_list es = List.iter add es in
  let to_list () = tbl |> Hashtbl.to_seq_keys |> List.of_seq in
  {add;mem;add_list;to_list}

(* map where the values are sets *)
let make_map () = 
  let tbl = Hashtbl.create 10 in
  let find k = 
    Hashtbl.find_opt tbl k |> function
    | Some set -> set
    | None -> 
      let set = make_set () in
      Hashtbl.add tbl k set;
      set
  in
  {find}


(*
type 'e set_factory = {
  make_set: unit -> 'e set;
}
*)

(*
type ('k,'v) map_factory = {
  make_map: unit -> ('k,'v) map_to_set
}
*)

(* let set_factory = {make_set} *)

(* let map_factory = {make_map}   *)


(** {2 Earley} *)

module type S = sig
  type nt
  type tm 
  type sym

  type sym_list

  type nt_item

  type input
end

module Make_compound_types(S:S) = struct
  open S
  (** NOTE not so important to represent these efficiently at this time *)

  type sym_item = { i_:int; sym:sym; j_:int }
  type sym_at_k = { sym:sym; k_:int } 

  type item = 
    | N of nt_item
    | C of sym_item
    | EXP of sym_at_k

  (* runtime *)
  type 's runtime_ops = {
    get_blocked_items  : int -> sym -> nt_item list;
    get_complete_items : int -> sym -> int list;
    add_item           : item -> unit;
    add_items          : item list -> unit;
    pop_todo           : unit -> item option;
    (* note_blocked_cuts  : nt_item -> int list -> unit;  *)
    (* note_complete_cuts : nt_item list -> int -> unit; *)
    (* note_matched_tm    : int -> tm -> int list -> unit; *)
    incr_count         : unit -> unit;
    get_state : unit -> 's;
    debug: 's -> unit;
  }

  (* grammar, typically provided at runtime *)
  type grammar_ops = {
    expand_nt: nt * int -> nt_item list;
    expand_tm: input -> tm -> int -> int list
  }
end

module Make_V(S:S) = struct
  open S
  open (Make_compound_types(S))

  module type V = sig
    (* sym *)
    val is_nt   : sym -> bool
    val _NT     : nt -> sym
    val _TM     : tm -> sym
    val dest_nt : sym -> nt
    val dest_tm : sym -> tm

    (* syms *)
    val syms_nil : sym_list -> bool
    val syms_hd  : sym_list -> sym
    val syms_tl  : sym_list -> sym_list

    (* nt_item *)
    val dot_nt : nt_item -> nt
    val dot_i  : nt_item -> int
    val dot_k  : nt_item -> int
    val dot_bs : nt_item -> sym_list
    val dest_nt_item : nt_item -> nt * int * int * sym_list

    (* extra *)
    val cut : nt_item -> int -> nt_item
  end
end


module Make(S:S) = struct
  open S      
  include Make_compound_types(S)
  include Make_V(S)

  module Make_with_V(V:V) = struct
    open V
        
    let earley 
        ~runtime:r 
        ~grammar:g
        ~input 
      =      
      let open (struct
        (* NOTE we assume the compiler can infer that the fields of r
           don't change, so accesses to r.field will be hoisted *)

        (* process a blocked item *)
        let cut_blocked_item itm = 
          let k_,_S = (itm|>dot_k), (itm|>dot_bs|>syms_hd) in
          let js = r.get_complete_items k_ _S in
          r.add_items (js |> List.map (fun j -> N (cut itm j)));
          ()

        let cut_complete_item {i_;sym;j_} =
          let itms = r.get_blocked_items i_ sym in
          r.add_items (itms |> List.map (fun itm -> N (cut itm j_)));
          ()

        let is_complete bs = syms_nil bs

        (* process an item *)
        let step itm =
          match itm with 
          | N itm -> begin
              r.incr_count();
              let bs = itm|>dot_bs in
              match is_complete bs with
              | true ->  
                let i_,sym,j_ = (itm|>dot_i), _NT (itm|>dot_nt), (itm|>dot_k) in
                r.add_item (C {i_; sym; j_ });
                ()
              | false -> 
                let k_,_S = (itm|>dot_k), (syms_hd bs) in                
                r.add_item (EXP { k_; sym=_S});
                cut_blocked_item itm;
                ()
            end
          | C itm -> cut_complete_item itm
          | EXP {sym;k_} -> 
            match is_nt sym with
            | true -> 
              let xs : nt_item list = g.expand_nt (dest_nt sym,k_) in
              r.add_items (List.map (fun x -> N x) xs);
              ()
            | false -> 
              let tm = dest_tm sym in
              let js = g.expand_tm input tm k_ in
              r.add_items (js |> List.map (fun j_ -> C {i_=k_;sym; j_}));
              ()

        let rec loop () = 
          r.debug (r.get_state());
          match r.pop_todo () with
          | None -> ()
          | Some itm -> step itm; loop ()

      end)
      in
      loop ()
      (* after calling earley; the mutable state via r will be updated *)


    (** {2 Filling in the details} *)

    (* mutable state *)
    type state = {
      count     : int ref;
      todo      : item list ref;
      todo_done : item set;
      blocked   : ( int*sym, nt_item ) map_to_set;
      complete  : ( int*sym, int ) map_to_set;
    }    

    let empty_state () = {
      count     = ref 0;
      todo      = ref [];
      todo_done = make_set ();
      blocked   = make_map ();
      complete  = make_map()
    }

    let make_runtime ~debug = 
      let s = empty_state () in
      let {count;todo;todo_done;blocked;complete} = s in
      let get_blocked_items i _S = (blocked.find (i,_S)).to_list () in
      let get_complete_items k _S = (complete.find (k,_S)).to_list () in
      let add_item itm = 
        match todo_done.mem itm with
        | true -> ()
        | false -> 
          assert(dont_log || (Printf.printf "Adding item\n"; true));
          todo_done.add itm; todo:=itm::!todo;
          (* we add new blocked items and new complete items here *)
          match itm with
          | N x -> begin
              let (nt,i,k,bs) = dest_nt_item x in            
              match syms_nil bs with
              | true -> 
                (complete.find (i,_NT nt)).add k
              | false -> 
                let _S = syms_hd bs in
                (blocked.find (k,_S)).add x
            end
          | _ -> ()
      in
      let add_items itms = List.iter add_item itms in
      let pop_todo () = 
        match !todo with
        | [] -> None 
        | x::xs -> todo:=xs; Some x
      in
      let incr_count () = incr count in
      let get_state () = s in
      s,{ get_blocked_items; get_complete_items; add_item; add_items; pop_todo;
          (* note_blocked_cuts; note_complete_cuts; note_matched_tm;  *)
          incr_count;
          get_state; debug }

    let earley ~debug ~grammar ~input ~initial_nt:nt = 
      let state,runtime = make_runtime ~debug in
      (* An alternative would be to just add EXP (nt,0) as item *)
      runtime.add_item (EXP {sym=_NT nt;k_=0});
      earley ~runtime ~grammar ~input;
      state
      
    let _ : 
      debug:(state -> unit) ->
      grammar:grammar_ops -> input:input -> initial_nt:nt -> state = earley
      
  end

end


  
(** {2 Typical instances} *)

module Instance_1 = struct

  module S = struct
    type nt = string
    type tm = string
    type sym = Nt of nt | Tm of tm
    type sym_list = sym list
    type nt_item = { nt:nt; i:int; k:int; bs: sym_list }
    type input = string
  end
  open S

  module Made = Make(S)
  open Made

  (* pretty printing *)
  let pp = object (s)
    method nt=(fun s -> s)
    method tm=(fun s -> s)
    method sym=(function Nt nt -> s#nt nt | Tm tm -> s#tm tm)
    method sym_list=(fun xs -> 
        Printf.sprintf "[ %s ]" (String.concat ";" (List.map s#sym xs)))
    method nt_item=(fun {nt;i;k;bs} -> 
        Printf.sprintf "{ %s; %d; %d; %s }" (s#nt nt) i k (s#sym_list bs))
    method sym_item=(fun {i_;sym;j_} -> 
        Printf.sprintf "( %d, %s, %d )" i_ (s#sym sym) j_)
    method sym_at_k=(fun {sym; k_} -> 
        Printf.sprintf "(%s, %d)" (s#sym sym) k_) 
    method item=(function
        | N x -> Printf.sprintf "(N %s)" (s#nt_item x)
        | C x -> Printf.sprintf "(C %s)" (s#sym_item x)
        | EXP x -> Printf.sprintf "(X %s)" (s#sym_at_k x))
  end
  
  (* provide values V *)
  module V : V = struct
    let is_nt = function Nt _ -> true | Tm _ -> false
    let _NT nt = Nt nt
    let _TM tm = Tm tm
    let dest_nt (Nt nt) = nt
    let dest_tm (Tm tm) = tm

    let syms_nil = function [] -> true | _ -> false
    let syms_hd (x::xs) = x
    let syms_tl (x::xs) = xs
      
    let dot_nt x = x.nt
    let dot_i x = x.i
    let dot_k x = x.k
    let dot_bs x = x.bs
    let dest_nt_item {nt;i;k;bs} = (nt,i,k,bs)

    let cut {nt;i;k;bs} j = {nt;i;k=j;bs=syms_tl bs}
  end

  module Made_with_V = Make_with_V(V)
  open Made_with_V

  type grammar_ops = Made.grammar_ops
  type state = Made_with_V.state

  let debug state = 
    assert(dont_log || (
        Printf.printf "State: ";
        !(state.todo) |> List.iter (fun x -> print_endline (pp#item x));
        true));
    ()

  let debug state = ()

  let earley : grammar:grammar_ops -> input:input -> initial_nt:input -> state = earley ~debug 

end
open Instance_1
open Instance_1.S
open Instance_1.Made

type grammar_ops = Instance_1.grammar_ops
type state = Instance_1.state

let earley : grammar:grammar_ops -> input:string -> initial_nt:string -> state = earley


(** {2 Test} *)

module Test = struct

  (* E -> E E E | "1" | eps *)
  let expand_nt (nt,i) = 
    match nt with
    | "E" -> [{nt="E";i;k=i;bs=[Nt "E"; Nt "E"; Nt "E"]};
              {nt="E";i;k=i;bs=[Tm "1"]};
              {nt="E";i;k=i;bs=[Tm "eps"]}]
    | _ -> failwith "Nonterminal not recognized"

  let expand_tm input tm i =
    assert(dont_log || (Printf.printf "expand_tm: (%s) @ %d\n" tm i; true));
    match tm with
    | "1" -> (
      match i < String.length input && String.get input i = '1' with
      | true -> [i+1]
      | false -> [])
    | "eps" -> 
      match i <= String.length input with
      | true -> [i]
      | false -> failwith "impossible"

  let grammar = {expand_tm;expand_nt}

  let run input = 
    let s = earley ~grammar ~input ~initial_nt:"E" in
    Printf.printf "%s: count is %d\n" __MODULE__ (!(s.count))              

end


