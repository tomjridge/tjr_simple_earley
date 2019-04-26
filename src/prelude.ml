(** Provided by user *)
type ('nt,'tm,'nt_item,'input) grammar_etc = {
  new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
  parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
  input:'input;
  input_length:int;
}

(** Result of parse; complete items is something like (i,sym) -> j set *)
type ('b,'c) parse_result = {
  count: int;
  items:'b;
  complete_items:'c  
}



(** {2 Some profiling parameters, set later in bin/} *)

(* let _ = Log.log @@ lazy (Printf.printf "%s: various mark_ref globals\n%!" __FILE__) *)

let base_mark_ref : (string -> unit) ref = ref (fun s -> ())

let spec_mark_ref : (string -> unit) ref = ref (fun s -> ())

let unstaged_mark_ref : (string -> unit) ref = ref (fun s -> ())

(** We often parameterize over nt,tm *)
module type NT_TM = sig  type nt type tm end

(** Some types used by the spec *)
module Spec_types = struct

  type ('nt,'tm) sym = Nt of 'nt | Tm of 'tm

  module Item_types = struct
    type ('nt,'bs) nt_item = { nt:'nt; i_:int; k_:int; bs:'bs }
    type 'sym sym_item = { i_:int; sym:'sym; j_:int }
    type 'sym sym_at_k = { sym:'sym; k_:int } 
    type ('a,'b,'c) item = 
      | Nt_item of 'a
      | Sym_item of 'b
      | Sym_at_k of 'c
  end

  module Make_derived_types(A:NT_TM) = struct
    open A
    open Item_types
    type sym' = (nt,tm) sym
    type sym_item' = sym' sym_item
    type sym_at_k' = sym' sym_at_k
    type nt_item' = (nt,sym' list) nt_item
    type item' = (
      nt_item',
      sym_item',
      sym_at_k') item
  end

end

let filter_sort_items itms = 
  let open Spec_types.Item_types in
  itms
  |> Misc.rev_filter_map (function
      | Nt_item x -> Some x
      | _ -> None)
  |> List.sort (fun itm1 itm2 -> 
      let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
      Pervasives.compare (f itm1) (f itm2))



(** {2 Common required interface} *)

(** What is required by the [Make] functor *) 
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

  type nt_item  

  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> int
  val dot_k: nt_item -> int
  val dot_bs: nt_item -> sym_list

  val syms_nil: sym_list -> bool
  val syms_hd: sym_list -> sym
  val syms_tl: sym_list -> sym_list

  val cut: nt_item -> int -> nt_item

  type 'input grammar_etc' = (nt,tm,nt_item,'input) grammar_etc
end  (* REQUIRED *)

