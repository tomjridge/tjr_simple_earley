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
    incr_count         : unit -> unit;
    get_state          : unit -> 's;
    debug              : 's -> unit;
  }
    (* note_blocked_cuts  : nt_item -> int list -> unit;  *)
    (* note_complete_cuts : nt_item list -> int -> unit; *)
    (* note_matched_tm    : int -> tm -> int list -> unit; *)

  (* grammar, typically provided at runtime *)
  type grammar_ops = {
    expand_nt: nt * int -> nt_item list;
    expand_tm: input -> tm -> int -> int list
  }
end

module Make_V(S:S) = struct
  open S
  module Made_compound_types = Make_compound_types(S)
  (* open Made_compound_types *)

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
  module Made_V = Make_V(S)
  include Made_V
  include Made_V.Made_compound_types

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
      complete  = make_map ()
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
          | N x -> (
              let (nt,i,k,bs) = dest_nt_item x in            
              match syms_nil bs with
              | true -> ()
              | false -> 
                let _S = syms_hd bs in
                (blocked.find (k,_S)).add x)
          | C {i_;sym;j_} -> 
            (complete.find (i_,sym)).add j_
          | EXP _ -> ()
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

(** Instance 1: nt is string, tm is string *)
module Instance_1 = struct

  module S = struct
    type nt = string
    type tm = string
    type sym = Nt of nt | Tm of tm
    type sym_list = sym list
    type nt_item = { nt:nt; i:int; k:int; bs: sym_list }
    type input = string
  end
  include S

  module Made = Make(S)
  include Made
  include Made.Made_compound_types

  (* pretty printing *)
  let pp = object (s)
    method nt       =(fun s -> s)
    method tm       =(fun s -> s)
    method sym      =(function Nt nt -> s#nt nt | Tm tm -> s#tm tm)
    method sym_list =(fun xs -> Printf.sprintf "[ %s ]" (String.concat ";" (List.map s#sym xs)))
    method nt_item  =(fun {nt;i;k;bs} -> Printf.sprintf "{ %s; %d; %d; %s }" (s#nt nt) i k (s#sym_list bs))
    method sym_item =(fun {i_;sym;j_} -> Printf.sprintf "( %d, %s, %d )" i_ (s#sym sym) j_)
    method sym_at_k =(fun {sym; k_} -> Printf.sprintf "(%s, %d)" (s#sym sym) k_) 
    method item     =(function
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
  include Made_with_V

  type grammar_ops = Made.grammar_ops
  type state = Made_with_V.state

  let debug state = 
    assert(dont_log || (
        Printf.printf "State: ";
        !(state.todo) |> List.iter (fun x -> print_endline (pp#item x));
        true));
    ()

  let debug state = ()

  let earley : grammar:grammar_ops -> input:input -> initial_nt:string -> state = earley ~debug 

end

(*
open Instance_1
open Instance_1.S
open Instance_1.Made

type grammar_ops = Instance_1.grammar_ops
type state = Instance_1.state
*)



(** Instance 2: nonterms and terms are ints *)
module Instance_2 = struct

  module S = struct
    type nt = int (* even *)
    type tm = int (* odd *)
    type sym = int 
    type sym_list = sym list
    type nt_item = { nt:nt; i:int; k:int; bs: sym_list }
    type input = string
  end
  include S

  module Made = Make(S)
  include Made
  include Made.Made_compound_types
  
  (* provide values V *)
  module V : V = struct
    let is_nt x = x mod 2=0
    let _NT nt = nt
    let _TM tm = tm
    let dest_nt nt = nt
    let dest_tm tm = tm

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
  include V

  module Made_with_V = Make_with_V(V)
  include Made_with_V

  type grammar_ops = Made.grammar_ops
  type state = Made_with_V.state

  let debug state = ()

  let earley : grammar:grammar_ops -> input:input -> initial_nt:int -> state = earley ~debug
end



(** {2 Test} *)

module Test = struct

  open Instance_1

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

  let grammar : grammar_ops = {expand_nt;expand_tm}  (* type included, but fields not *)

  let run input = 
    let s = earley ~grammar ~input ~initial_nt:"E" in
    Printf.printf "%s: count is %d\n" __MODULE__ (!(s.count))              

end


(** {2 Helper functions to build a grammar} *)

module Helper() = struct


  type nt = int (* even *)
  type tm = int (* odd *)
  type sym = int

  let max_nt=ref 0
  let max_tm=ref 1

  (* map where the values are opaque *)
  type ('e,'f) tm_map = {
    set:'e -> 'f -> unit;
    get:'e -> 'f;
    (* mem:'e -> bool; *)
  }

  let wf_tm tm = tm mod 2 = 1 && tm <= !max_tm
  let wf_nt nt = nt mod 2 = 0 && nt <= !max_nt

  (* error to get the expand_tm function without setting it first *)
  let tm_map () = 
    (* let expand_tm ~input ~i = [] in *)
    let tbl = Hashtbl.create 10 in
    let get k = 
      Hashtbl.find_opt tbl k |> function
      | Some f -> f
      | None -> failwith "tm_map"
    in
    let set k f = 
      Hashtbl.replace tbl k f
    in
    (* let mem k = Hashtbl.mem tbl k in *)
    {set;get}

  let tm_map = tm_map ()

  let expand_tm (input:string) tm (i:int) : int list =
    tm_map.get tm |> fun f -> f ~input ~i

  (* expect to get a list of rules *)
  let nt_map () = 
    let tbl = Hashtbl.create 10 in
    let get k = 
      Hashtbl.find_opt tbl k |> function
      | Some f -> f
      | None -> 
        let v = ref [] in
        Hashtbl.replace tbl k v;
        v
    in
    let set k f = Hashtbl.replace tbl k f in
    {set;get}

  let nt_map = nt_map ()

  open Instance_2

  let expand_nt (nt,i) = 
    !(nt_map.get nt) |> fun rhs_s -> 
    rhs_s |> List.map (fun rhs -> {nt;i;k=i;bs=rhs})

  let g = 
    object
      method mk_nt=
        max_nt:=2 + !max_nt;
        !max_nt
      method mk_tm expand =
        max_tm:=2 + !max_tm;
        let tm = !max_tm in
        tm_map.set tm expand;          
        tm

      (** set_tm allows to override a possibly-dummy expand function
          provided when tm was declared *)
      method set_tm tm f = 
        tm_map.set tm f
      method add_rule nt rhs =
        let xs = nt_map.get nt in
        xs:=rhs::!xs
      method add_rules nt rules = 
        let xs = nt_map.get nt in
        xs:=rules @ !xs
      method grammar : grammar_ops =
        { expand_nt; expand_tm }
    end

end


(** {2 Examples} *)
 
module Tmp = struct

  (** Model a simple grammar for expressing rules etc

      {[
        E -> E E E | "1" | "eps";

        F -> F;
      ]}

  *)
  let meta_grammar p = 
    let open (struct
      let a = p#a
      let ( || ) = p#alt
      let seq = p#seq
      let list_sep = p#list_of
      let ws = p#ws
      let re = p#re


      let nt       = re "[A-Z]+"
      let tm       = re {|["][a-z][A-Za-z]*["]|}
      let sym      = nt || tm
      let rhs      = list_sep ~sep:ws sym
      let bar      = seq [ws; a"|"; ws]
      let rhs_list = list_sep ~sep:bar rhs
      let rule     = seq [ nt; a "->"; rhs_list; a ";" ]
      let g        = list_sep ~sep:ws rule
    end)
    in
    g

  let _ = meta_grammar


  (* E -> E E E | "1" | "eps" *)
  let example_grammar p = 
    let open (struct
      let a = p#a
      let _E = p#_E

      let g = [
        (_E, [ [_E;_E;_E]; [a"1"]; [a""]])
      ]                      
    end)
    in
    g

  let p = 
    object
      method a x = `A x
      method _E = `E
    end

  let g : (([ `A of string | `E ] as 'a) * 'a list list) list = (example_grammar p)

  (* map to int; terminals are odd, nonterms are even *)
  let to_int = function
    | `E -> 0
    | `A "1" -> 1
    | `A "" -> 3

  (* convert g to ints *)
  let g = g |> List.map (fun (nt,rhs_s) -> 
      (to_int nt, List.map (fun rhs -> List.map to_int rhs) rhs_s))

  let _ : (int * int list list) list = g

  let expand_tm input tm i =
    match tm with
    | 1 -> (
        match i < String.length input && String.get input i = '1' with
        | true -> [i+1]
        | false -> [])
    | 3 -> 
      match i <= String.length input with
      | true -> [i]
      | false -> failwith "impossible"

  (* process g to get function from nt to rhs_s *)
  let arr = Array.make 1 []  (* needs to hold all nts *)
  let _ = 
    g |> List.iter (fun (nt,rhs_s) -> 
        arr.(nt) <- rhs_s)

  open Instance_2

  let expand_nt (nt,i) = 
    arr.(nt) |> List.map (fun rhs -> {nt;i;k=i;bs=rhs})

  let grammar : grammar_ops = {expand_tm;expand_nt}

  let earley ~input = earley ~grammar ~input ~initial_nt:0
end





(** Same example, using the Helper *)
module Test_helper = struct

  module EEE = struct
    open Helper()

    (** Terminals *)

    let one = g#mk_tm (fun ~input ~i -> 
        match i < String.length input && String.get input i = '1' with
        | true -> [i+1]
        | false -> [])

    let eps = g#mk_tm (fun ~input ~i -> 
        match i <= String.length input with
        | true -> [i]
        | false -> failwith "impossible")


    (** Non-terminals *)

    let _E = g#mk_nt


    (** Rules and grammar *)

    (* E -> E E E | "1" | eps *)
    let _ = g#add_rules _E [
        [ _E; _E; _E ];
        [one];
        [eps]
      ]

    let grammar = g#grammar
  end

  let test input = EEE.(
      Printf.printf "%s: testing Instance_2 with helper\n" __FILE__;
      Instance_2.earley ~grammar ~input ~initial_nt:_E)
end
