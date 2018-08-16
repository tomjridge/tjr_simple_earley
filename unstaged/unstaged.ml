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
  bitms: map_nt array;
  nonterms_expanded: nt_set array;
  complete_items: nt_set array array
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
             bitms=
               Array.get s.bitms k |> fun map ->
               map_nt_ops.map_find nt map |> fun itms ->
               nt_item_set_ops.add itm itms |> fun itms ->
               map_nt_ops.map_add nt itms map |> fun map ->
               Array.set s.bitms k map;
               s.bitms}))
  in

  let add_item ~itm =
    with_world (fun s ->
        ((),match nt_item_set_ops.mem itm s.todo_done with
          | true -> s
          | false -> {s with
                      todo=itm::s.todo;
                      todo_done=nt_item_set_ops.add itm s.todo_done
                     }))
  in

  let cut_complete_item_with_blocked_items ~i ~nt ~k =
    with_world (fun s ->
        Array.get s.bitms i |> fun bitms ->
        map_nt_ops.map_find nt bitms |> fun bitms ->
        nt_item_set_ops.elements bitms |> fun bitms ->
        List.map (fun bitm -> cut bitm k) bitms |> fun bitms ->
        List.filter (fun bitm -> not (nt_item_set_ops.mem bitm s.todo_done)) bitms |> fun bitms ->        
        ((),{s with
             todo=bitms@s.todo;
             todo_done=add_many_items s.todo_done bitms}))
  in


  let expand_nonterm ~k ~nt =
    with_world (fun s ->
        assert(not (nt_set_ops.mem nt (Array.get s.nonterms_expanded k)));
        ((),
         expand_nonterm ~k ~nt |> fun itms -> 
         {s with
          todo=itms@s.todo;
          todo_done=add_many_items s.todo_done itms;
          nonterms_expanded=(
            Array.get s.nonterms_expanded k |> fun set ->
            nt_set_ops.add nt set |> fun set ->
            Array.set s.nonterms_expanded k set;
            s.nonterms_expanded)
         }))
  in
         
  let get_item () = 
    with_world (fun s ->
        s.todo |> function
        | [] -> None,s
        | x::todo -> Some x, {s with todo})
  in

  let have_we_expanded_nonterm ~k ~nt =
    with_world (fun s -> 
        nt_set_ops.mem nt (Array.get s.nonterms_expanded k), s)
  in

  let note_complete_item ~i ~nt ~k =
    with_world (fun s -> 
        let seen_before = s.complete_items.(i).(k) |> nt_set_ops.mem nt in
        seen_before, { s with
                       complete_items=(
                         s.complete_items.(i).(k) |> nt_set_ops.add nt |> fun set ->
                         s.complete_items.(i).(k) <- set;
                         s.complete_items)})
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
    ~nt_item_ops)

