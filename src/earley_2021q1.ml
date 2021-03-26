(** Quick reimplementation 2021q1, following my current preferred
   coding style, with mutable datastructures. Self-contained single
   file implementation. Unstaged, efficient O(n^3). *)

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
    | Nt_item of nt_item
    | Sym_item of sym_item
    | Sym_at_k of sym_at_k

  (* runtime *)
  type runtime_ops = {
    get_blocked_items  : int -> sym -> nt_item list;
    get_complete_items : int -> sym -> int list;
    add_item           : item -> unit;
    add_items          : item list -> unit;
    pop_todo           : unit -> item option;
    note_blocked_cuts  : nt_item -> int list -> unit; 
    note_complete_cuts : nt_item list -> int -> unit;
    note_matched_tm    : int -> tm -> int list -> unit;
    incr_count         : unit -> unit;
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
        let cut_blocked_item = fun itm -> 
          let k_,_S = itm|>dot_k, itm|>dot_bs|>syms_hd in
          r.get_complete_items k_ _S |> fun js ->
          r.note_blocked_cuts itm js;
          js |> List.map (fun j -> Nt_item (cut itm j)) |> r.add_items;
          ()

        let cut_complete_item =
          fun {i_;sym;j_} -> 
          r.get_blocked_items i_ sym |> fun itms ->
          r.note_complete_cuts itms j_;
          itms |> List.map (fun itm -> Nt_item (cut itm j_)) |> r.add_items;
          ()

        (* process an item *)
        let step itm =
          match itm with 
          | Nt_item itm -> begin
              r.incr_count();
              let bs = itm|>dot_bs in
              match bs|>syms_nil with
              | true ->  (* item is complete *)
                let i_,sym,j_ = (itm|>dot_i), _NT (itm|>dot_nt), (itm|>dot_k) in
                let itm = Sym_item {i_; sym; j_ } in
                r.add_item itm;
                ()
              | false ->  (* item is not complete *)
                let k_,_S,bs = (itm|>dot_k), (syms_hd bs), (syms_tl bs) in
                (* record that we need to expand _S *)
                r.add_item (Sym_at_k { k_; sym=_S});
                (* and we need to process the item against complete items *)
                cut_blocked_item itm;
                ()
            end
          | Sym_item itm -> cut_complete_item itm
          | Sym_at_k {sym;k_} -> 
            match is_nt sym with
            | true -> 
              let xs : nt_item list = g.expand_nt (dest_nt sym,k_) in
              r.add_items (List.map (fun x -> Nt_item x) xs);
              ()
            | false -> 
              let tm = dest_tm sym in
              let js = g.expand_tm input tm k_ in
              r.note_matched_tm k_ tm js;
              ()

        let rec loop () = 
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

    let empty_state = {
      count     = ref 0;
      todo      = ref [];
      todo_done = make_set ();
      blocked   = make_map ();
      complete  = make_map()
    }

    let make_runtime () = 
      let s = empty_state in
      let {count;todo;todo_done;blocked;complete} = s in
      let get_blocked_items i _S = (blocked.find (i,_S)).to_list () in
      let get_complete_items k _S = (complete.find (k,_S)).to_list () in
      let add_item itm = 
        match todo_done.mem itm with
        | true -> ()
        | false -> todo_done.add itm; todo:=itm::!todo
      in
      let add_items itms = List.iter add_item itms in
      let pop_todo () = 
        match !todo with
        | [] -> None 
        | x::xs -> todo:=xs; Some x
      in
      let note_blocked_cuts itm js = 
        let (nt,i,k,bs) = dest_nt_item itm in
        let _S = syms_hd bs in
        (complete.find (k,_S)).add_list js
      in
      let note_complete_cuts itms j = 
        itms |> List.iter (fun itm -> 
            let (nt,i,k,bs) = dest_nt_item itm in
            let _S = syms_hd bs in
            (complete.find (k,_S)).add j)
      in
      let note_matched_tm i tm js =
        js |> List.iter (fun j -> 
            (complete.find (i,_TM tm)).add j)
      in
      let incr_count () = incr count in
      s,{ get_blocked_items; get_complete_items; add_item; add_items; pop_todo;
        note_blocked_cuts; note_complete_cuts; note_matched_tm; incr_count }

    let earley ~grammar ~input ~initial_nt:nt = 
      let state,runtime = make_runtime () in
      let itms = grammar.expand_nt (nt,0) in
      runtime.add_items (itms |> List.map (fun x -> Nt_item x));
      earley ~runtime ~grammar ~input;
      state
      
    let _ : grammar:grammar_ops -> input:input -> initial_nt:nt -> state = earley
      
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

  let earley : grammar:grammar_ops -> input:input -> initial_nt:input -> state = earley
end
open Instance_1

type grammar_ops = Instance_1.grammar_ops
type state = Instance_1.state

let earley : grammar:grammar_ops -> input:string -> initial_nt:string -> state = earley
