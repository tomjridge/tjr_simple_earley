open Staged

module Make_set' = functor (Ord:Set.OrderedType) -> struct
    type elt = Ord.t
    module Set' = Set.Make(struct type t = elt let compare : elt -> elt -> int = Pervasives.compare end)
    type set = Set'.t
    type ops = {
      empty: set;
      is_empty: set -> bool;
      add: elt -> set -> set;
      mem: elt -> set -> bool;
      with_each_elt: 'a. f:(state:'a -> elt -> 'a) -> init_state:'a -> set -> 'a;
      elements: set -> elt list;
    }
    let with_each_elt ~f ~init_state set = Set'.fold (fun a b -> f ~state:b a) set init_state
    let ops = {
      empty=Set'.empty;
      is_empty=Set'.is_empty;
      add=Set'.add;
      mem=Set'.mem;
      with_each_elt;
      elements=Set'.elements
    }
end

module Make_map' = functor(Ord:Set.OrderedType) -> struct
  module Map' = Map.Make(Ord)
  type k_ = Ord.t
  type 'v map_ = 'v Map'.t
  type 'v ops = {
    find: k_ -> 'v map_ -> 'v;
    add: k_ -> 'v -> 'v map_ -> 'v map_;
    empty: 'v map_;
    remove: k_ -> 'v map_ -> 'v map_
  }
  let ops ~default = {
    find=(fun k m -> try Map'.find k m with _ -> default);
    add=Map'.add;
    empty=Map'.empty;
    remove=Map'.remove
  }
end



module S = struct
  type i_t = int
  type k_t = int
  type j_t = int
  type nt = int (* even, say *)
  type tm = int (* odd, say *)
  type sym = int
  let even x = (x mod 2 = 0)
  let sym_case ~nt ~tm sym = 
    if even sym then nt sym else tm sym

  let _NT: nt -> sym = fun x -> x

  type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;  (* NOTE in "reversed" order *)
    k: k_t;
    bs: sym list
  }

  module Set_nt_item : Set_ with type elt=nt_item = Make_set'(struct type t = nt_item let compare : t -> t -> int = Pervasives.compare end)

  type nt_item_set = Set_nt_item.set
  let nt_item_set_ : Set_nt_item.ops = Set_nt_item.ops

  type ixk = (i_t * nt)  (* i X k *)
  module Set_ixk : Set_ with type elt=ixk = Make_set'(struct type t = ixk let compare : t -> t -> int = Pervasives.compare end)
  type ixk_set = Set_ixk.set
  let ixk_set : Set_ixk.ops = Set_ixk.ops


  module Map_nt : Map_ with type k_ = nt = Make_map'(struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
  type 'a map_nt = 'a Map_nt.map_

  module Map_int: Map_ with type k_ = int = Make_map'(struct type t = int let compare : t -> t -> int = Pervasives.compare end)
  type 'a map_int = 'a Map_int.map_

  module Map_tm: Map_ with type k_ = tm = Make_map'(struct type t = tm let compare : t -> t -> int = Pervasives.compare end)
  type 'a map_tm = 'a Map_tm.map_

  (* these have obvious defaults for find operation *)
  let map_int_ : nt_item_set Map_int.ops = Map_int.ops ~default:nt_item_set_.Set_nt_item.empty
  let map_tm_: (int list option) Map_tm.ops =  Map_tm.ops ~default:None
  let map_nt_ : nt_item_set Map_nt.ops = Map_nt.ops ~default:nt_item_set_.Set_nt_item.empty

end

module Staged = Staged.Make(S)


(* simple test ------------------------------------------------------ *)

open S
open Staged

let _E = 0
let eps = 1
let _1 = 3

let new_items ~nt ~input ~k = match () with
  | _ when nt = _E -> 
    [ [_E;_E;_E]; [_1]; [eps] ]   (* E -> E E E | "1" | eps *)
    |> List.map (fun bs -> {nt;i=k;as_=[];k;bs})
  | _ -> failwith __LOC__

let input = String.make (Sys.argv.(1) |> int_of_string) '1'

let parse_tm ~tm ~input ~k ~input_length = 
  match () with
  | _ when tm = eps -> [k]
  | _ when tm = _1 -> 
    (* print_endline (string_of_int k); *)
    if String.get input k = '1' then [k+1] else []
  | _ -> failwith __LOC__

let input_length = String.length input

let debug_enabled = false

let debug_endline = fun s -> ()

(*
ctxt:((new_items:(nt:S.nt -> input:'a -> k:int -> S.nt_item list) ->
       input:'a ->
       parse_tm:(tm:S.Map_tm.k_ ->
                 input:'a -> k:int -> input_length:int -> S.j_t list) ->
       input_length:int ->
       debug_enabled:bool ->
       debug_endline:(string -> 'b) -> init_nt:S.nt -> int) ->
      int) ->
int

*)

let init_nt = _E

let ctxt = fun f ->
  f ~new_items ~input ~parse_tm ~input_length ~debug_enabled ~debug_endline ~init_nt

let main () = Staged.staged ~ctxt |> string_of_int |> print_endline

let _ = main ()

(* FIXME check this is actually giving the right results 

$ src $ time ./test.native 200
200

real	0m0.984s
user	0m0.968s
sys	0m0.012s

This compares with e3 Start example 17y ......stop in 1.804879 seconds, so quicker


$ src $ time ./test.native 400
400

real	0m7.821s
user	0m7.732s
sys	0m0.084s


*)
