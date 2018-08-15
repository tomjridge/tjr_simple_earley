(* A version of Earley which incorporates the optimization from Leo'91 


   NOTE currently this is just a simplified and more efficient version of
   Earley; it doesn't include Leo's optimization currently
*)

let now () = Core.Time_stamp_counter.(
    now () |> to_int63 |> Core.Int63.to_int |> Tjr_profile.dest_Some)

let Tjr_profile.{mark;get_marks} = Tjr_profile.mk_profiler ~now
(* let mark x = () *)
open Tjr_profile.P

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
  (* maintain invariant that if (X->i,as,k,B bs) is in the current
     set, and nullable(B) then (X -> i,as B, k, bs) is in the set *)


  let earley 
      ~add_blocked_item_at_current_k   (* needs to add all transitively nullable items as well *)
      ~add_item_at_suc_k  (* after matching a terminal; ditto transitively nullable  *)
      ~cut
      ~cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items
      ~expand_nonterm     (* takes k as argument *)
      ~finished 
      ~get_item
      ~get_k 
      ~have_we_expanded_nonterm_at_current_k 
      ~incr_k 
      ~input_matches_tm_at_k
      ~note_complete_item_at_current_k
      ~nt_item_ops
    = 
    let { dot_nt; dot_i; dot_bs_hd } = nt_item_ops in
    let rec step_at_k ~k ~itm = 
      mark __LINE__;
      match dot_bs_hd itm with 
      | None -> (
          mark __LINE__;
          (* NOTE complete item (i,X,k) *)
          let (i,_X) = (dot_i itm, dot_nt itm) in
          note_complete_item_at_current_k ~i ~nt:_X >>= fun seen_before ->
          mark __LINE__;
          match seen_before with
          | true -> mark __LINE__; return ()
          | false -> 
            (* cut with blocked items *)
            (* FIXME is it clear that we never twice add an item to the list ? Needs some thought here *)
            match i=k with
            | true -> mark __LINE__; return ()
            | false -> 
              cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items ~i ~nt:_X >>= fun () ->
              mark __LINE__;
              return ())              
      | Some _S -> 
        mark __LINE__;
        _S |> sym_case 
          ~nt:(fun _X ->
              mark __LINE__;
              add_blocked_item_at_current_k ~nt:_X ~itm >>= fun () ->
              mark __LINE__;
              have_we_expanded_nonterm_at_current_k ~nt:_X >>= function
              | true -> mark __LINE__; return ()
              | false -> 
                mark __LINE__;
                expand_nonterm ~k ~nt:_X >>= fun () ->
                mark __LINE__;
                return ())
          ~tm:(fun tm ->                
              input_matches_tm_at_k ~k ~tm |> function
              | true -> 
                mark __LINE__;
                let itm' : nt_item = cut itm (k+1) in
                add_item_at_suc_k ~itm:itm' >>= fun () ->
                mark __LINE__;
                return ()
              | false ->
                mark __LINE__;
                return ())
    and loop_at_k ~k = 
      get_item () >>= function
      | None -> return ()
      | Some itm ->
        step_at_k ~k ~itm >>= fun _ -> loop_at_k ~k
    and earley () =
      get_k () >>= fun k ->
      loop_at_k ~k >>= fun () ->
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

open Tjr_monad
open Tjr_monad.Monad

open Simple_datastructure_implementations.S

module Set_nt = Set.Make(
  struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
type nt_set = Set_nt.t
let nt_set_ops = Set_nt.{ add; mem; empty; is_empty; elements }

type state = {
  k:int;
  current_items: nt_item list;
  todo_done_at_k: nt_item_set;
  items_at_suc_k: nt_item_set;
  nonterms_expanded_at_current_k: nt_set;
  complete_items_at_current_k: ixk_set;
  bitms_at_k: map_nt;
  bitms_lt_k: map_nt array
}

let make_empty_state ~input_length = {
  k=0;
  current_items=[];
  todo_done_at_k=nt_item_set_ops.empty;
  items_at_suc_k=nt_item_set_ops.empty;
  nonterms_expanded_at_current_k=nt_set_ops.empty;
  complete_items_at_current_k=ixk_set_ops.empty;
  bitms_at_k=map_nt_ops.map_empty;
  bitms_lt_k=Array.make (input_length+1) map_nt_ops.map_empty
}

module M = struct
  open Tjr_monad.Imperative_instance
  type 'a m = ('a,imperative) Monad.m
  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind  
end


module Leo = Make(M)(Simple_datastructure_implementations.S)

let s = ref (make_empty_state ~input_length:500)

let with_world f = 
  Tjr_monad.Imperative_instance.to_m (!s |> f |> fun (a,s') -> s:=s'; a)

(* FIXME we probably want an option to cut an item without altering the k val *)

(* NOTE expand_nonterm must update nonterms_expanded_at_current_k;
   actually assume that the function passed in simply returns the
   ntitems *)
let make_earley ~nullable ~expand_nonterm ~input_length ~input_matches_tm_at_k = (

  (* NOTE following could be based on trans_rhs, which need only be calculated once per rhs *)
  let trans_items ~k itm = 
    (* Printf.printf "Called %d %d %d %d \n%!" itm.nt itm.i_ itm.k_ (List.length itm.bs); *)
    let rec f itm = 
      (* Printf.printf "f %d %d %d %d \n%!" itm.nt itm.i_ itm.k_ (List.length itm.bs); *)

      itm::(
        match dot_bs_hd itm with
        | None -> []
        | Some s ->
          match nullable s with
          | true -> f (cut itm k)
          | false -> [])
    in
    f itm
  in

  let add_many_items set itms =
    itms |> Earley_util.List_.with_each_elt
      ~init_state:set
      ~step:(fun ~state itm -> 
          nt_item_set_ops.add itm state)
  in
                     
  let add_blocked_item_at_current_k ~nt ~itm =
    with_world (fun s ->
        ((),{s with
             bitms_at_k=
               map_nt_ops.map_find nt s.bitms_at_k |> fun itms' ->
               (* Printf.printf "Size of itms': %d %d\n%!" s.k (nt_item_set_ops.elements itms' |> List.length); *)
               (* Printf.printf "Called %d %d %d %d \n%!" itm.nt itm.i_ itm.k_ (List.length itm.bs); *)
               nt_item_set_ops.add itm itms' |> fun itms'' ->               
               map_nt_ops.map_add nt itms'' s.bitms_at_k}))
  in

  let add_item_at_suc_k ~itm =
    with_world (fun s ->
        ((),{s with
             items_at_suc_k=
               match nt_item_set_ops.mem itm s.items_at_suc_k with
               | true -> s.items_at_suc_k
               | false -> 
                 (trans_items ~k:(s.k+1) itm) 
                 |> add_many_items s.items_at_suc_k}))
  in

  let cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items ~i ~nt =
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
         

  let finished () =
    with_world (fun s ->
        (s.k = input_length || [] = s.current_items),s)
  in

  let get_item () = 
    with_world (fun s ->
        s.current_items |> function
        | [] -> None,s
        | x::current_items -> Some x, {s with current_items})
  in

  let get_k () = 
    with_world (fun s -> s.k,s)
  in

  let have_we_expanded_nonterm_at_current_k ~nt =
    with_world (fun s -> 
        assert(nt_set_ops.elements s.nonterms_expanded_at_current_k |> List.length < 2);
        nt_set_ops.mem nt s.nonterms_expanded_at_current_k, s)
  in

  let incr_k () =
    with_world (fun s -> 
        (* Printf.printf "Init items at stage %d: %d\n%!" (s.k+1) (nt_item_set_ops.elements s.items_at_suc_k |> List.length); *)
        (), { k=s.k+1;
              current_items=nt_item_set_ops.elements s.items_at_suc_k;
              todo_done_at_k=s.items_at_suc_k;
              items_at_suc_k=nt_item_set_ops.empty;
              nonterms_expanded_at_current_k=nt_set_ops.empty;
              complete_items_at_current_k=ixk_set_ops.empty;
              bitms_at_k=map_nt_ops.map_empty;
              bitms_lt_k=(
                Array.set s.bitms_lt_k s.k s.bitms_at_k;
                (* NOTE arrays are mutable anyway... *)
                s.bitms_lt_k);
            })
  in

  let note_complete_item_at_current_k ~i ~nt =
    with_world (fun s -> 
        let seen_before = ixk_set_ops.mem (i,nt) s.complete_items_at_current_k in
        seen_before, { s with
                       complete_items_at_current_k=(ixk_set_ops.add (i,nt) s.complete_items_at_current_k )})
  in

  let nt_item_ops = { dot_i; dot_nt; dot_k; dot_bs_hd } in  (* FIXME dot_k *)

  Leo.earley 
      ~add_blocked_item_at_current_k
      ~add_item_at_suc_k 
      ~cut
      ~cut_complete_item_at_curr_k_with_blocked_items_and_add_new_items
      ~expand_nonterm
      ~finished 
      ~get_item
      ~get_k 
      ~have_we_expanded_nonterm_at_current_k 
      ~incr_k 
      ~input_matches_tm_at_k
      ~note_complete_item_at_current_k
      ~nt_item_ops)

let _ :
nullable:(Simple_datastructure_implementations.S.sym -> bool) ->
expand_nonterm:(k:int ->
                nt:Simple_datastructure_implementations.S.nt -> nt_item list) ->
input_length:int ->
input_matches_tm_at_k:(k:int ->
                       tm:Simple_datastructure_implementations.S.tm ->
                       bool) ->
unit -> unit M.m
= make_earley

let earley ~nullable ~expand_nonterm ~input_length ~input_matches_tm_at_k ~init_items =
  let init_state = 
    make_empty_state ~input_length |> fun s ->
    {s with current_items=init_items }  (* FIXME todo_done_at_k? *)
  in
  let _ = s:=init_state in
  make_earley ~nullable ~expand_nonterm ~input_length ~input_matches_tm_at_k ()
  |> Tjr_monad.Imperative_instance.from_m 
  |> fun _ -> !s


let _ : 
nullable:(sym -> bool) ->
expand_nonterm:(k:int -> nt:nt -> nt_item list) ->
input_length:int ->
input_matches_tm_at_k:(k:int -> tm:tm ->bool) ->
init_items:nt_item list -> 
state
= earley



(* test ------------------------------------------------------------- *)

(* from simple_test.ml *)

(* Encode nonterminals and terminals as ints; nts are even; tms are
   odd *)

let _E = 0
let _1 = 1

let nullable sym = match sym with 
  | _ when sym=_E -> true
  | _ -> false

(* Encode the grammar E -> E E E | "1" | eps; eps is captured by nullable *)
let rhss = [ [_E;_E;_E]; [_1] ]

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
let input_matches_tm_at_k ~k ~tm =
  assert (tm=_1);
  if k < input_length then true else false


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
  earley ~nullable ~expand_nonterm ~input_length ~input_matches_tm_at_k
    ~init_items:[{nt=_E; i_=0; k_=0; bs=[_E]}]
  |> fun s -> s.k |> string_of_int |> print_endline

let _ = main ()

let _ = 
  let open Tjr_profile in
  get_marks () |> print_profile_summary

(*

$ leo $ time ./leo.native 200
200

real	0m2.546s
user	0m2.544s
sys	0m0.000s

Slower because we need to avoid adding items we have already seen?


$ leo $ time ./leo.native 400
400

real	0m34.919s
user	0m34.896s
sys	0m0.020s

v. slow compared to test/test2

Profiling info w state passing:

200
Time:0  59 59 count:1
Time:37753  67 83 count:200
Time:55183  86 50 count:200
Time:64780  72 76 count:200
Time:88023  78 50 count:200
Time:149639  83 86 count:200
Time:3833103  76 78 count:200
Time:23271706  64 50 count:20100
Time:392653772  50 67 count:1373901
Time:498078296  50 53 count:2686700
Time:524828025  57 59 count:2666600
Time:581872031  67 70 count:1373701
Time:1632820100  72 74 count:1373501
Time:1759465732  74 50 count:1373501
Time:3031282741  70 72 count:1373701
Time:3188218525  57 64 count:20100
Time:3857811582  59 50 count:2666599 - return from seen before
Time:6229920531  53 57 count:2686700 - note complete item

real	0m13.723s
user	0m13.232s
sys	0m0.484s

With imperative monad:

real	0m1.946s
user	0m1.932s
sys	0m0.012s

Why is this so slow compared to experimental?




old $ leo $ time ./leo.native 200
old 200
old Time:0  53 53 count:1
old Time:52091  73 89 count:200
old Time:60382  78 82 count:200
old Time:64913  51 53 count:200
old Time:75288  92 51 count:200
old Time:110710  89 92 count:200
old Time:111319  84 51 count:200
old Time:1828693  82 84 count:200
old Time:28341436  70 51 count:20100
old Time:243617864  53 51 count:199
old Time:306412186  55 73 count:1373901
old Time:376911493  63 65 count:2666600
old Time:646772294  73 76 count:1373701
old Time:688170760  55 59 count:2686700
old Time:984968977  78 80 count:1373501
old Time:2072100263  80 51 count:1373501
old Time:2463280315  76 78 count:1373701
old Time:3361288346  63 70 count:20100
old Time:3528461144  65 51 count:2666600 - this is a return from l. 65; half the time
old Time:4365131924  51 55 count:4060601  - this is get_item, which should be quick
old Time:5247637371  59 63 count:2686700
old 
old real	0m15.521s
old user	0m14.708s
old sys	0m0.808s
old 

overall, it is just about possible that the defn of the monad is not being optimized, whereas it was in other versions

*)
