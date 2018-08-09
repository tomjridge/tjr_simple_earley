(* A version of Earley which incorporates the optimization from Leo'91 


   NOTE currently this is just a simplified and more efficient version of
   Earley; it doesn't include Leo's optimization currently
*)


open Earley_util.Set_ops
open Earley_util.Map_ops

module type REQUIRES = sig
  type 'a m
  val return : 'a -> 'a m
  val ( >>= ): 'a m -> ('a -> 'b m) -> 'b m

  include Earley_util.NEEDED_BASIC_INTERFACE  (* NOTE don't need dot_k *)
end


module Make(Requires: REQUIRES) = struct
  open Requires
  (* maintain invariant that if (X->i,as,k,B bs) is in the current
     set, and nullable(B) then (X -> i,as B, k, bs) is in the set *)


  let earley 
      ~add_blocked_item_at_current_k   (* needs to add all transitively nullable items as well *)
      ~add_item_at_suc_k  (* after matching a terminal; ditto transitively nullable  *)
      ~cut
      ~cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items
      ~expand_nonterm     (* takes k as argument *)
      ~finished 
(*      ~get_initial_items_at_k   *)
      ~get_item
      ~get_k 
      ~have_we_expanded_nonterm_at_current_k 
      ~incr_k 
      ~input_matches_tm_at_k
      ~note_complete_item_at_current_k
      ~nt_item_ops
    = 
    let { dot_nt; dot_i; dot_bs_hd } = nt_item_ops in
    let loop_at_k = 
      get_k () >>= fun k -> 
      let process_itm itm = 
        match dot_bs_hd itm with 
        | None -> (
            (* NOTE complete item (i,X,k) *)
            let (i,_X) = (dot_i itm, dot_nt itm) in
            note_complete_item_at_current_k ~i ~nt:_X >>= fun seen_before ->
            match seen_before with
            | true -> return ()
            | false -> 
              (* cut with blocked items *)
              cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items ~i ~nt:_X >>= fun () ->
              return ())              
        | Some _S -> 
          _S |> sym_case 
            ~nt:(fun _X ->
                add_blocked_item_at_current_k ~nt:_X ~itm >>= fun () ->
                have_we_expanded_nonterm_at_current_k ~nt:_X >>= function
                | true -> return ()
                | false -> 
                  expand_nonterm ~k ~nt:_X >>= fun () ->
                  return ())
            ~tm:(fun tm -> 
                input_matches_tm_at_k ~k ~tm >>= function
                | true -> 
                  let itm' : nt_item = cut itm (k+1) in
                  add_item_at_suc_k ~itm:itm'
                | false ->
                  return ())
      in
      let rec loop () =
        get_item () >>= function
        | Some itm -> 
          process_itm itm >>= fun () -> 
          loop ()
        | None ->
          return ()
      in
      loop ()
    in
    let rec earley () =
      loop_at_k >>= fun () ->
      incr_k () >>= fun () ->        
      (* finished if no initial items at the next stage, or we have reached the end of the input *)
      finished () >>= function
      | true -> return ()  (* actually return the final state *)
      | false -> earley ()
    in
    earley

  let _ = earley


end



(* specialize monad and implement reqs ----------------------------- *)

(*

  type nt_set
  val nt_set_ops: (nt,nt_set) set_ops


  type state = {
    k:int;
    current_items: nt_item list;
    items_at_suc_k: nt_item_set;
    nonterms_expanded_at_current_k: nt_set;
    complete_items_at_current_k: nt_item_set;
  }
*)

(* val run: code:'a m -> init_state:unit -> 'a *)

