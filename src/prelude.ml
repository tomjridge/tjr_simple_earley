
(** {2 Some profiling parameters, set later in bin/} *)

(* let _ = Log.log @@ lazy (Printf.printf "%s: various mark_ref globals\n%!" __FILE__) *)

let base_mark_ref : (string -> unit) ref = ref (fun s -> ())

let spec_mark_ref : (string -> unit) ref = ref (fun s -> ())

let unstaged_mark_ref : (string -> unit) ref = ref (fun s -> ())


(** {2 Type for inputs} *)

type 'a input = {
  input:'a;
  input_length: int
}


(** {2 Type for grammars} *)

(** In the interface to Earley, the user has to know the structure of
   the sym type; so it is useful to have this known outside *)
type ('nt,'tm) generic_sym = Nt of 'nt | Tm of 'tm

(** A simple representation of a grammar. We might want to generalize this. *)
type ('nt,'tm) simple_grammar = {
  nt_to_rhss: nt:'nt -> ('nt,'tm) generic_sym list list
}

type ('nt,'tm,'a) input_dependent_grammar = {
  nt_input_to_rhss: nt:'nt -> input:'a input -> pos:int -> ('nt,'tm)generic_sym list list
}




(** {2 Types for terminal parsers} *)

type ('tm,'a) terminal_input_matcher = {
  parse_tm: tm:'tm -> input:'a input -> pos:int -> int list
}

(*
(** Provided by user; NOTE that this exposes the [nt_item] type, but
   we could instead go for a list of sym. Then we might also just
   require a map from nt to rhs list *)
type ('nt,'tm,'nt_item,'input) grammar_etc = {
  new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
  parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
  input:'input;
  input_length:int;
}
*)


(** {2 Parsing result type} *)

(** Result of parse; complete items is something like (i,sym) -> j set *)
type ('b,'c) parse_result = {
  count: int;
  items:'b;
  complete_items:'c  
}



(** {2 Common required interface} *)

(** We often parameterize over nt,tm *)
module type NT_TM = sig  type nt type tm end

(** What is often required by the [Make] functor *) 
module type REQUIRED = sig

  type nt
  type tm
  type sym

  val is_nt: sym -> bool
  val dest_nt: sym -> nt
  val dest_tm: sym -> tm
  val _NT: nt -> sym
  val _TM: tm -> sym

  type sym_list
  val syms_nil: sym_list -> bool
  val syms_hd: sym_list -> sym
  val syms_tl: sym_list -> sym_list

  type nt_item  
  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> int
  val dot_k: nt_item -> int
  val dot_bs: nt_item -> sym_list

  val cut: nt_item -> int -> nt_item

  (* type 'input grammar_etc' = (nt,tm,nt_item,'input) grammar_etc *)
end  (* REQUIRED *)

(** Simple instantiation of basic types *)
module Simple_items(A:sig type nt type tm end) = struct
  open A

  type sym = (nt,tm)generic_sym
  let is_nt = function Nt x -> true | Tm x -> false
  let dest_nt = function Nt x -> x | _ -> failwith "dest_nt"
  let dest_tm = function Tm x -> x | _ -> failwith "dest_tm"
  let _NT (x:nt) = Nt x
  let _TM (x:tm) = Tm x

  let sym_case ~nt ~tm = function
    | Nt x -> nt x
    | Tm y -> tm y

  type sym_list = sym list
  let syms_nil (xs:sym_list) = match xs with [] -> true | _ -> false
  let syms_hd (xs:sym_list) = List.hd xs
  let syms_tl (xs:sym_list) = List.tl xs

  type nt_item = { nt:nt; i_:int; k_:int; bs:sym_list }
  let dot_nt x = x.nt
  let dot_i x = x.i_
  let dot_k x = x.k_
  let dot_bs x = x.bs
  let mk_nt_item nt i bs = { nt; i_=i; k_=i; bs }

  let cut itm j = {itm with k_=j; bs=List.tl itm.bs}

  (* type sym_item = { i_:int; sym:sym; j_:int } *)
  (* type sym_at_k = { sym:sym; k_:int }  *)
end



(*
(** Generic type of items (for spec?) *)
type ('a,'b,'c) generic_item = 
  | Nt_item of 'a
  | Sym_item of 'b
  | Sym_at_k of 'c
*)

(*
  let filter_sort_items itms = 
    itms
    |> Misc.rev_filter_map (function
        | Nt_item x -> Some x
        | _ -> None)
    |> List.sort (fun itm1 itm2 -> 
        let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
        Pervasives.compare (f itm1) (f itm2))
*)

