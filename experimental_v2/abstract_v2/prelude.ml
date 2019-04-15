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

  type todo_gt_k  (* int -> nt_item_set *)
  type bitms_lt_k  (* int -> bitms_at_k *)
  type bitms_at_k  (* nt -> nt_item_set *)

  (* following are per k *)
  type ixk_done  (* int*nt set *)
  type ktjs  (* tm -> j list option *)

  type 'a m

end
