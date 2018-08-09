(* A version of Earley which incorporates the optimization from Leo'91 


   NOTE currently this is just a simplified and more efficient version of
   Earley; it doesn't include Leo's optimization currently
*)



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

open Set_ops

open Map_ops


module Make(X: sig
    type 'a m
    val return : 'a -> 'a m
    val ( >>= ): 'a m -> ('a -> 'b m) -> 'b m

    val run: code:'a m -> init_state:unit -> 'a

  type i_t = int  
  type k_t = int
  type j_t = int
  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  (*:mo:*)    

  type nt_item  
  type nt_item_ops = {
    dot_nt: nt_item -> nt;
    dot_i: nt_item -> i_t;
    dot_bs_hd: nt_item -> sym option; (* can use to check empty *)
  }
  val nt_item_ops : nt_item_ops

  (*:mp:*)

  type nt_item_set
  val nt_item_set_ops: (nt_item,nt_item_set) set_ops
  val nt_item_set_with_each_elt: 
    f:(state:'a -> nt_item -> 'a) -> init_state:'a -> itms:nt_item_set -> 'a

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



end) = struct

  open X
  (* maintain invariant that if (X->i,as,k,B bs) is in the current
     set, and nullable(B) then (X -> i,as B, k, bs) is in the set *)

  type state

  let earley 
      ~add_blocked_item_at_current_k   (* needs to add all transitively nullable items as well *)
      ~add_item_at_suc_k  (* after matching a terminal; ditto transitively nullable  *)
      ~cut
      ~cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items
      ~expand_nonterm     (* takes k as argument *)
      ~finished 
      ~(get_final_state:unit -> state m)
      ~get_initial_items_at_k  
      ~get_item
      ~get_k 
      ~have_we_expanded_nonterm_at_current_k 
      ~incr_k 
      ~input_matches_tm_at_k
      ~note_complete_item_at_current_k
      
    = 
    let { dot_nt; dot_i; dot_bs_hd } = nt_item_ops in
    let loop_at_k = 
      get_k () >>= fun k -> 
      let rec loop () =
        finished () >>= function
        | true -> return ()
        | false -> 
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
                      let itm' = cut itm (k+1) in
                      add_item_at_suc_k ~itm:itm'
                    | false ->
                      return ())
          in
          get_item () >>= fun itm -> 
          process_itm itm >>= fun () -> 
          loop ()
      in
      loop ()
    in
    let rec earley () =
      loop_at_k >>= fun () ->
        incr_k () >>= fun () ->        
        (* finished if no initial items at the next stage, or we have reached the end of the input *)
        finished () >>= function
        | true -> get_final_state ()  (* what we actually return *)
        | false -> earley ()
    in
    (* exit the monad *)
    let r : state = run ~code:(earley()) ~init_state:() in
    r
      
    

end
