(** Provided by user *)
type ('nt,'tm,'nt_item,'input) grammar_etc = {
  new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
  parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
  input:'input;
  input_length:int;
}

(** Result of parse *)
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
  type ('nt,'bs) nt_item = { nt:'nt; i_:int; k_:int; bs:'bs }
  type 'sym sym_item = { i_:int; sym:'sym; j_:int }
  type 'sym sym_at_k = { sym:'sym; k_:int } 
  type ('a,'b,'c) item = 
    | Nt_item of 'a
    | Sym_item of 'b
    | Sym_at_k of 'c

  module Make_derived_types(A:NT_TM) = struct
    open A
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
  let open Spec_types in
  itms
  |> Misc.rev_filter_map (function
      | Nt_item x -> Some x
      | _ -> None)
  |> List.sort (fun itm1 itm2 -> 
      let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
      Pervasives.compare (f itm1) (f itm2))


