(* A parsing alg. like Earley, but unstaged *)


open Earley_util.Set_ops
open Earley_util.Map_ops

(* FIXME probably separate out monad from NEEDED, and apply the functor to the state passing monad *)

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val ( >>= ): 'a m -> ('a -> 'b m) -> 'b m
end


module Make(Monad:MONAD)(Requires: Earley_util.NEEDED_BASIC_INTERFACE) = struct
  open Monad
  open Requires


let earley 
    ~add_blocked_item
    ~add_item
    ~cut
    ~cut_complete_item_with_blocked_items 
    ~expand_nonterm
    ~get_item 
    ~have_we_expanded_nonterm
    ~input_matches_tm
    ~note_complete_item 
    ~nt_item_ops 
  =
  let { dot_nt; dot_i; dot_k; dot_bs_hd } = nt_item_ops in
  let step itm = 
    match dot_bs_hd itm with 
      | None -> (
          (* NOTE complete item (i,X,k) *)
          let (i,_X,k) = (dot_i itm, dot_nt itm,dot_k itm) in
          note_complete_item ~i ~nt:_X ~k >>= fun seen_before ->
          match seen_before with
          | true -> return ()
          | false -> 
            (* cut with blocked items *)
            cut_complete_item_with_blocked_items ~i ~nt:_X ~k >>= fun () ->
            return ())
      | Some _S -> 
        _S |> sym_case 
          ~nt:(fun _X ->
              let k = dot_k itm in
              add_blocked_item ~nt:_X ~k ~itm >>= fun () ->
              have_we_expanded_nonterm ~k ~nt:_X >>= function
              | true -> return ()
              | false -> 
                expand_nonterm ~k ~nt:_X >>= fun () ->
                return ())
          ~tm:(fun tm ->                
              (* FIXME we probably want to remember terminal matches *)
              let k = dot_k itm in
              input_matches_tm ~k ~tm |> function
              | true -> 
                let itm' : nt_item = cut itm (k+1) in
                add_item ~itm:itm' >>= fun () ->
                return ()
              | false ->
                return ())
  in
  let rec loop () =
    get_item () >>= function
    | None -> return ()
    | Some itm -> step itm >>= fun _ -> loop ()
  in
  loop()

end


(* specialize monad and implement reqs ----------------------------- *)

open Tjr_monad
open Tjr_monad.Monad

open Simple_datastructure_implementations.S

module Set_nt = Set.Make(
  struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
type nt_set = Set_nt.t
let nt_set_ops = Set_nt.{ add; mem; empty; is_empty; elements }

type state = {
  todo:nt_item list;
  todo_done: nt_item_set;
  (* bitms: bitms_map;  (\* map from int? *\) *)
}

let make_empty_state ~input_length = failwith "FIXME"

module M = struct
  open Tjr_monad
  type 'a m = ('a,state state_passing) Monad.m
  open State_passing_instance
  let mops = monad_ops()
  let return = mops.return
  let ( >>= ) = mops.bind  
end



module Earley = Make(M)(Simple_datastructure_implementations.S)

let make_earley ~expand_nonterm ~input_length ~input_matches_tm = (

  let with_world = Tjr_monad.State_passing_instance.with_world in

  let add_many_items set itms =
    itms |> Earley_util.List_.with_each_elt
      ~init_state:set
      ~step:(fun ~state itm -> 
          nt_item_set_ops.add itm state)
  in
                     
  let add_blocked_item ~nt ~k ~itm =
    with_world (fun s ->
        ((),{s with
             bitms_at_k=
               map_nt_ops.map_find nt s.bitms_at_k |> fun itms' ->
               (* Printf.printf "Size of itms': %d %d\n%!" s.k (nt_item_set_ops.elements itms' |> List.length); *)
               (* Printf.printf "Called %d %d %d %d \n%!" itm.nt itm.i_ itm.k_ (List.length itm.bs); *)
               nt_item_set_ops.add itm itms' |> fun itms'' ->               
               map_nt_ops.map_add nt itms'' s.bitms_at_k}))
  in

  let add_item ~itm =
    with_world (fun s ->
        ((),{s with
             items_at_suc_k=
               match nt_item_set_ops.mem itm s.items_at_suc_k with
               | true -> s.items_at_suc_k
               | false -> 
                 (trans_items ~k:(s.k+1) itm) 
                 |> add_many_items s.items_at_suc_k}))
  in

  let cut_complete_item_with_blocked_items ~i ~nt =
    with_world (fun s ->
        ((),match s.k=i with 
          | true -> s
          | false -> 
            (* i < k *)
            (* get blocked items *)
            Array.get s.bitms_lt_k i |> fun bitms ->
            map_nt_ops.map_find nt bitms |> fun bitms ->
            nt_item_set_ops.elements bitms |> fun bitms ->
            List.map (fun bitm -> cut bitm s.k) bitms |> fun bitms ->
            List.filter (fun bitm -> not (nt_item_set_ops.mem bitm s.todo_done_at_k)) bitms |> fun bitms ->
            List.map (trans_items ~k:s.k) bitms |> List.concat |> fun new_itms ->            
            let current_items = new_itms@s.current_items in
            let todo_done_at_k = add_many_items s.todo_done_at_k new_itms in  (* bug was here! *)
            (* Printf.printf "Length of items: %d\n%!" (List.length current_items); *)
            {s with current_items; todo_done_at_k}))
  in


  let expand_nonterm ~k ~nt =
    with_world (fun s ->
        assert(k=s.k);
        assert(not (nt_set_ops.mem nt s.nonterms_expanded_at_current_k));
        ((),
         expand_nonterm ~k ~nt |> fun itms -> 
         List.map (trans_items ~k) itms |> List.concat |> fun new_itms ->
         (* we only expand new_itms once at each k, so the items can't already be in the current_items *)
         (* List.filter (fun itm -> not (nt_item_set_ops.mem itm s.todo_done_at_k)) new_itms -> fun new_itms -> *)
            {s with
             current_items=new_itms@s.current_items;
             todo_done_at_k=add_many_items s.todo_done_at_k new_itms;
             nonterms_expanded_at_current_k=nt_set_ops.add nt s.nonterms_expanded_at_current_k
            }))
  in
         
  let get_item () = 
    with_world (fun s ->
        s.current_items |> function
        | [] -> None,s
        | x::current_items -> Some x, {s with current_items})
  in

  let have_we_expanded_nonterm ~nt =
    with_world (fun s -> 
        assert(nt_set_ops.elements s.nonterms_expanded_at_current_k |> List.length < 2);
        nt_set_ops.mem nt s.nonterms_expanded_at_current_k, s)
  in

  let note_complete_item ~i ~nt ~k =
    with_world (fun s -> 
        let seen_before = ixk_set_ops.mem (i,nt) s.complete_items_at_current_k in
        seen_before, { s with
                       complete_items_at_current_k=(ixk_set_ops.add (i,nt) s.complete_items_at_current_k )})
  in

  let nt_item_ops = { dot_i; dot_nt; dot_k; dot_bs_hd } in  (* FIXME dot_k *)

  Earley.earley 
    ~add_blocked_item
    ~add_item
    ~cut
    ~cut_complete_item_with_blocked_items 
    ~expand_nonterm
    ~get_item 
    ~have_we_expanded_nonterm
    ~input_matches_tm
    ~note_complete_item 
    ~nt_item_ops 
    ~nt_item_ops)

