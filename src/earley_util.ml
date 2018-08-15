module Misc = struct
  let dest_Some = function | Some x -> x | None -> failwith __LOC__
end

(*:ma:*)
module List_ = struct

  let fold_left_ ~step ~init_state xs = 
    List.fold_left 
      (fun a b -> step ~state:a b)
      init_state
      xs

  let with_each_elt = fold_left_
end


module Set_ops = struct
  type ('e,'t) set_ops = {
    add: 'e -> 't -> 't;
    mem: 'e -> 't -> bool;
    empty: 't;
    is_empty: 't -> bool;
    elements: 't -> 'e list;
  }
end


module Map_ops = struct
  type ('k,'v,'t) map_ops = {
    map_add: 'k -> 'v -> 't -> 't;
    map_find:'k -> 't -> 'v;
    map_empty:'t;
    map_remove:'k -> 't -> 't;
  }
end

(* FIXME really clarify what the interfaces should be *)
module type NEEDED_BASIC_INTERFACE = sig
  open Set_ops

  (*:mn:*)
  type i_t = int  
  type k_t = int
  type j_t = int
  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  (* val _NT: nt -> sym *)

  (*:mo:*)    

  type nt_item  
  type nt_item_ops = {
    dot_nt: nt_item -> nt;
    dot_i: nt_item -> i_t;
    dot_k: nt_item -> k_t;  (* FIXME we don't need this - already have k info at use points *)
    dot_bs_hd: nt_item -> sym option; (* can use to check empty *)
  }

  (*:mp:*)
  type nt_item_set
  val nt_item_set_ops: (nt_item,nt_item_set) set_ops

end

(*:mm:*)
module type NEEDED_EXTENDED_INTERFACE = sig  
  open Set_ops
  open Map_ops

  include NEEDED_BASIC_INTERFACE

  val nt_item_set_with_each_elt: 
    f:(state:'a -> nt_item -> 'a) -> init_state:'a -> nt_item_set -> 'a


  type ixk = (i_t * nt)  (* i X k *)
  type ixk_set
  val ixk_set_ops: (ixk,ixk_set) set_ops

  type map_nt
  val map_nt_ops: (nt,nt_item_set,map_nt) map_ops

  type map_int
  val map_int_ops : (int,nt_item_set,map_int) map_ops

  type map_tm
  val map_tm_ops : (tm,int list option,map_tm) map_ops

  (*:mq:*)

  type bitms_lt_k  (* int -> nt -> nt_item_set; implement by array *)
  type bitms_lt_k_ops = (int,map_nt,bitms_lt_k) map_ops
  (* passed in at run time val bitms_lt_k_ops: (int,map_nt,bitms_lt_k) map_ops *)

  (*:mr:*)

  type todo_gt_k
  val todo_gt_k_ops: (int,nt_item_set,todo_gt_k) map_ops

  (*:ms:*)

  type cut = nt_item -> j_t -> nt_item

  (* FIXME why are these here? *)
  val debug_enabled : bool
  val debug_endline : string -> unit
end
