(* A parsing alg. like Earley, but unstaged *)

let count = ref 0

open Earley_util.Set_ops
open Earley_util.Map_ops
let int_set_ops = Earley_util.int_set_ops

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
    ~add_items
    ~cut
    ~cut_complete_item_with_blocked_items 
    ~cut_blocked_item_with_complete_items
    ~expand_nonterm
    ~get_item 
    ~have_we_expanded_nonterm
    ~input_matches_tm
    ~note_complete_item 
    ~nt_item_ops 
  =
  let { dot_nt; dot_i; dot_k; dot_bs_hd } = nt_item_ops in
  let step itm = 
    count := (1+ !count);
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
              add_blocked_item ~nt:_X ~k ~itm >>= fun seen_before ->
              match seen_before with
              | true -> return ()
              | false ->
                cut_blocked_item_with_complete_items ~nt:_X ~k ~itm >>= fun () ->
                have_we_expanded_nonterm ~k ~nt:_X >>= function
                | true -> return ()
                | false -> 
                  expand_nonterm ~k ~nt:_X >>= fun () ->
                  return ())
          ~tm:(fun tm ->                
              (* FIXME we probably want to remember terminal matches *)
              let k = dot_k itm in
              input_matches_tm ~k ~tm |> fun js ->
              List.map (fun j -> cut itm j) js |> fun itms ->
              add_items ~itms >>= fun () -> 
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

(* citms are a map from int (array) to map from nt to int set *)

type is = Earley_util.Int_set.t
type map_nt_is = Earley_util.Int_set.t Map_nt.t


let map_nt_is_ops : (nt,is,map_nt_is) map_ops = Map_nt.{ map_add;map_find=map_find Earley_util.int_set_ops.empty;map_empty;map_remove }

type state = {
  todo:nt_item list;
  todo_done: nt_item_set;
  bitms: map_nt array;
  nonterms_expanded: nt_set array;
  citms: map_nt_is array
}

let make_empty_state ~input_length = {
  todo=[];
  todo_done=nt_item_set_ops.empty;
  bitms=Array.make 500 map_nt_ops.map_empty;
  nonterms_expanded=Array.make 500 nt_set_ops.empty;
  citms=Array.make 500 map_nt_is_ops.map_empty
}

module M = struct
  open Tjr_monad
  type 'a m = ('a,state state_passing) Monad.m
  open State_passing_instance
  let mops = monad_ops()
  let return = mops.return
  let ( >>= ) = mops.bind  
end


let add_many_items set itms =
  itms |> Earley_util.List_.with_each_elt
    ~init_state:set
    ~step:(fun ~state itm -> 
        nt_item_set_ops.add itm state)


module Earley = Make(M)(Simple_datastructure_implementations.S)

let make_earley ~expand_nonterm ~input_length ~input_matches_tm = (

  let with_world = Tjr_monad.State_passing_instance.with_world in

                     
  let add_blocked_item ~nt ~k ~itm =
    with_world (fun s ->
        Array.get s.bitms k |> fun map ->
        map_nt_ops.map_find nt map |> fun itms ->
        let seen_before = nt_item_set_ops.mem itm itms in
        nt_item_set_ops.add itm itms |> fun itms ->
        map_nt_ops.map_add nt itms map |> fun map ->
        Array.set s.bitms k map;
        (seen_before,s))  (* bitms mutable *)
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

  let add_items ~itms =
    with_world (fun s ->
        itms |> List.filter (fun itm -> not (nt_item_set_ops.mem itm s.todo_done)) |> fun itms ->
        (),{s with 
            todo=itms@s.todo;
            todo_done=add_many_items s.todo_done itms
           })
  in

  let cut_blocked_item_with_complete_items ~nt ~k ~itm = 
    with_world (fun s ->
        (* need k _X j complete items from k *)
        Array.get s.citms k |> fun map ->
        map_nt_is_ops.map_find nt map |> fun is ->
        int_set_ops.elements is |> fun is ->
        List.map (cut itm) is |> fun itms ->
        List.filter (fun itm -> not (nt_item_set_ops.mem itm s.todo_done)) itms |> fun itms ->
        (),{s with 
            todo=itms@s.todo;
            todo_done=add_many_items s.todo_done itms
           })
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
        let seen_before,map,is = 
          s.citms.(i) |> fun map ->
          map_nt_is_ops.map_find nt map |> fun is ->
          int_set_ops.mem k is,map,is
        in
        s.citms.(i) <- map_nt_is_ops.map_add nt (int_set_ops.add k is) map; 
        seen_before, s)
  in

  let nt_item_ops = { dot_i; dot_nt; dot_k; dot_bs_hd } in  (* FIXME dot_k *)

  Earley.earley 
    ~add_blocked_item
    ~add_item
    ~add_items
    ~cut
    ~cut_blocked_item_with_complete_items
    ~cut_complete_item_with_blocked_items 
    ~expand_nonterm
    ~get_item 
    ~have_we_expanded_nonterm
    ~input_matches_tm
    ~note_complete_item 
    ~nt_item_ops)


let earley ~expand_nonterm ~input_length ~input_matches_tm ~init_items =
  let init_state = 
    make_empty_state ~input_length |> fun s ->
    {s with 
     todo=init_items;
     todo_done=add_many_items nt_item_set_ops.empty init_items } 
  in
  make_earley ~expand_nonterm ~input_length ~input_matches_tm
  |> Tjr_monad.State_passing_instance.run ~init_state
  |> fun (_,s) -> s


(* test ------------------------------------------------------------- *)

(* from simple_test.ml *)

(* Encode nonterminals and terminals as ints; nts are even; tms are
   odd *)

let _E = 0
let _1 = 1
let eps = 3

let nullable sym = match sym with 
  | _ when sym=_E -> true
  | _ -> false

(* Encode the grammar E -> E E E | "1" | eps; eps is captured by nullable *)
let rhss = [ [_E;_E;_E]; [_1]; [eps] ]
(* let rhss = [ [_E;_E;_E]; [_1] ] *)

(* Provide a function that produces new items, given a nonterminal and
   an input position k *)
let expand_nonterm ~k ~nt = match () with
  | _ when nt = _E -> 
    rhss   (* E -> E E E | "1" | eps *)
    |> List.map (fun bs -> { nt; i_=k; k_=k; bs})
  | _ -> failwith __LOC__

(* Example input; use command line argument *)
let input = String.make (Sys.argv.(1) |> int_of_string) '1'

let input_length = String.length input

(* Provide a function that details how to parse terminals at a given
   position k in the input *)
let input_matches_tm ~k ~tm =
  match tm with
  | _ when tm=_1 ->
    if k < input_length then [k+1] else []
  | _ when tm=eps ->
    [k]
  | _ -> failwith __LOC__


(* Initial nonterminal *)
let init_nt = _E

let nt_item_ops = {
  dot_nt;
  dot_i;
  dot_k;
  dot_bs_hd
}



(* Finally, run Earley! *)

let main () = 
  earley ~expand_nonterm ~input_length ~input_matches_tm
    ~init_items:[{nt=_E; i_=0; k_=0; bs=[_E]}]
  |> fun s -> Printf.printf "Finished: %d\n%!" !count 

let _ = main ()

(*
let _ = 
  let open Tjr_profile in
  get_marks () |> print_profile_summary
*)


(*

$ unstaged $ time ./unstaged.native 400
Finished: 243006

real	0m15.053s
user	0m15.032s
sys	0m0.016s

*)
