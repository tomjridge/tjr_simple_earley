(* test2, represent nt_item as int *)


open Earley_util

(* Compared to test.ml, we compute int mappings lazily *)


(* first we enumerate syms, lazily *)

module Make_sym(X:sig type sym val is_nt: sym -> bool end) = struct
  open X

  (* nts are even, tms are odd; we maintain a mapping *)
  type sym_enum = {
    mutable free:int;
    tbl: (sym,int) Hashtbl.t  (* mutable anyway *)
  }


  let is_even x = x mod 2 = 0
  let is_nt: int -> bool = is_even

  let make () = 
    let enum = { free=0; tbl=Hashtbl.create 10 } in
    let sym2int = fun s -> 
      try
        Hashtbl.find enum.tbl s
      with _ -> 
        let n = enum.free in
        let n = 
          match X.is_nt s, is_even n with
          | true,true -> n
          | false,false -> n
          | _ -> n+1
        in
        Hashtbl.add enum.tbl s n;
        enum.free <- n+1;
        n
    in
    fun f -> f ~sym2int
end 


module Make_rhs(X:sig type sym end) = struct

  open X

  (* map syms to an int, and a possible head of the list; the use of a
     list of syms is fine, because this is only for the interface -
     the grammar function is memoized so that sym lists are never used
     more than once per nonterm per parse *)
  type rhs_enum = {
    mutable free:int;
    bs2i: (sym list,int) Hashtbl.t;  (* mutable anyway *)
    hd_bs: int option array 
  }

  let make ~sym2int = 
    let enum = { free=0; bs2i=Hashtbl.create 10; hd_bs=Array.make 100 None} in
    let rhs2i = fun rhs -> 
      try
        Hashtbl.find enum.bs2i rhs
      with _ -> 
        (* if we don't already have it, we add rhs and all suffixes of rhs *)
        let rec loop rhs = match rhs with
          | [] -> (
              let n = enum.free in
              Hashtbl.add enum.bs2i rhs n;
              (* leave hd_bs as None *)
              enum.free <- n +1;
              (* Printf.printf "free %d\n%!" enum.free; *)
              ())
          | sym::rest -> (
              let n = enum.free in
              Hashtbl.add enum.bs2i rhs n;
              Array.set enum.hd_bs n (Some (sym2int sym));
              enum.free <- n +1;
              (* Printf.printf "free %d\n%!" enum.free; *)
              loop rest)
        in
        loop rhs;
        Hashtbl.find enum.bs2i rhs
    in
    (* NOTE hd_bs takes bs as an *int* *)
    let hd_bs = fun rhs_n -> 
      (* Printf.printf "rhs_n %d\n%!" rhs_n; *)
      Array.get enum.hd_bs rhs_n in
    fun f -> f ~rhs2i ~hd_bs

end


  (* itms are triples (nt,i,bs_as_int)  *)

open Tjr_earley
open Set_ops
open Map_ops

(* Simple map implementation *)
module Map_make = functor (Ord:Set.OrderedType) -> struct
  include struct
    module X = Map.Make(Ord)
    open X
    type 'a t = 'a X.t
    let map_add = add
    let map_find default = fun k m ->
      try find k m with _ -> default
    let map_empty = empty
    let map_remove = remove
  end
end

(* Implement the signature required by the Earley code *)
module S = struct
  type i_t = int
  type k_t = int
  type j_t = int

  (* NOTE we need to be able to distinguish nonterminals from terminals *)
  type nt = int (* even, say *)
  type tm = int (* odd, say *)
  type sym = int
  let even x = (x mod 2 = 0)
  let sym_case ~nt ~tm sym = 
    if even sym then nt sym else tm sym

  let _NT: nt -> sym = fun x -> x

  (* here we have a bound on sym which we can use for encoding a list;
     we also have a bound on max length of list; alternatively just
     number the possible rhs?

     from an nt_item, we want to extract nt,i,k, and bs; easiest to
     repn all possible bs by an int, and a lookup table to compute the
     relevant sym list
     
     we also need to be able to take the tl of a list of bs
  *)

  type rhs = int

  type nt_item = int  (*  nt * int_i * int_k * int_rhs *)

  (* Here we choose b to be 1024; obviously this places a limit on the
     number of rules; for larger grammars we need larger b *)
  let b1 = 1024 
  let b2 = b1 * 1024 
  let b3 = b2 * 1024 

  (* Encode an item (omitting the "as" component) as an int *)
  let to_int (nt,i,k,bs) = 
    (* Printf.printf "to_int %d %d %d %d\n%!" nt i k bs; *)
    nt*b3 + i*b2 + k*b1 + bs
    
  (* not needed?
  let mk_nt_item arr nt i k bs =
    let bs' = lookup arr bs|>function Some x -> x | _ -> failwith __LOC__ in
    assert(bs'<b1);      (* assume lookup ... < 1024 *)
    to_int (nt,i,k,bs')
  *)

  (* Implement the accessor functions by using simple arithmetic *)
  let dot_nt nitm = nitm / b3
  let dot_i nitm = (nitm / b2) mod b1
  let dot_k nitm = (nitm / b1) mod b1
  let dot_bs_as_int nitm = nitm mod b1

  let cut : nt_item -> j_t -> nt_item = 
    let from_int nitm = 
      (dot_nt nitm,dot_i nitm, dot_k nitm, dot_bs_as_int nitm) |> fun (nt,i,k,bs) ->
      (* Printf.printf "from_int %d %d %d %d\n%!" nt i k bs; *)
      (nt,i,k,bs)
    in
    fun bitm j0 -> 
      let (nt,i,k,bs) = from_int bitm in
      let bs = bs+1 in (* NOTE artefact of the numbering *)
      let k = j0 in
      let nitm =to_int (nt,i,k,bs) in
      nitm 
  type cut = nt_item -> j_t -> nt_item


  type nt_item_ops = {
    dot_nt: nt_item -> nt;
    dot_i: nt_item -> i_t;
    dot_k: nt_item -> k_t;
    dot_bs_hd: nt_item -> sym option;
  }

  (* The rest of the code is straightforward *)
                      
  module Set_nt_item = Set.Make(
    struct type t = nt_item let compare : t -> t -> int = Pervasives.compare end)
  type nt_item_set = Set_nt_item.t
  let nt_item_set_ops = Set_nt_item.{ add;mem;empty;is_empty;elements }
  let nt_item_set_with_each_elt ~f ~init_state set =
    Set_nt_item.fold (fun e acc -> f ~state:acc e) set init_state

  type ixk=(i_t*nt)
  module Set_ixk = Set.Make(
    struct type t = ixk let compare : t -> t -> int = Pervasives.compare end)
  type ixk_set = Set_ixk.t
  let ixk_set_ops = Set_ixk.{ add;mem;empty;is_empty;elements } 
  
  module Map_nt = Map_make(
      struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
  type map_nt = nt_item_set Map_nt.t
  let map_nt_ops = Map_nt.{ map_add;map_find=map_find nt_item_set_ops.empty;map_empty;map_remove }

  module Map_int = Map_make(
      struct type t = int let compare : t -> t -> int = Pervasives.compare end)
  type map_int = nt_item_set Map_int.t
  let map_int_ops = 
    Map_int.{ map_add;map_find=map_find nt_item_set_ops.empty;map_empty;map_remove }

  module Map_tm = Map_make(
      struct type t = tm let compare : t -> t -> int = Pervasives.compare end)
  type map_tm = int list option Map_tm.t
  let map_tm_ops = Map_tm.{ map_add;map_find=map_find None;map_empty;map_remove }

  type bitms_lt_k = (*nt_item_set*) map_nt array
  type bitms_lt_k_ops = (int,map_nt,bitms_lt_k) map_ops

  type todo_gt_k = map_int
  let todo_gt_k_ops = map_int_ops

  let debug_enabled = false
  let debug_endline (s:string) = ()

end  (* S *)

module Earley = Tjr_earley.Make(S)
open Earley


(* simple test ------------------------------------------------------ *)

open S

(* Encode nonterminals and terminals as ints; nts are even; tms are
   odd *)

type sym = E | One | Eps

let is_nt = function | E -> true | _ -> false

module X1 = struct type nonrec sym = sym let is_nt = is_nt end

module Tmp1 = Make_sym(X1)

let sym2int : sym -> int = Tmp1.make () @@ fun ~sym2int -> sym2int

module Tmp2 = Make_rhs(X1)

(* Encode the grammar E -> E E E | "1" | eps *)
let rhss = [ [E;E;E]; [One]; [Eps] ]

let (rhs2i,hd_bs) = Tmp2.make ~sym2int @@ fun ~rhs2i ~hd_bs -> (rhs2i,hd_bs)

(* Provide a function that produces new items, given a nonterminal and
   an input position k *)

let rhss' = List.map rhs2i rhss

let new_items ~nt ~input ~k = 
  match nt with
  | _ when nt = sym2int E -> 
    rhss'
    |> List.map (fun bs -> let i = k in S.to_int (nt,i,k,bs))
  | _ -> failwith __LOC__

(* Example input; use command line argument *)
let input = String.make (Sys.argv.(1) |> int_of_string) '1'

(* Provide a function that details how to parse terminals at a given
   position k in the input *)
let parse_tm ~tm ~input ~k ~input_length = 
  match () with
  | _ when tm = sym2int Eps -> [k]
  | _ when tm = sym2int One -> 
    (* print_endline (string_of_int k); *)
    if String.get input k = '1' then [k+1] else []
  | _ -> failwith __LOC__

let input_length = String.length input

(* Initial nonterminal *)
let init_nt = sym2int E

let dot_bs_hd nitm = 
  nitm |> S.dot_bs_as_int |> hd_bs

(* Construct (nonterminal) item operations *)
let nt_item_ops = {
  dot_nt;
  dot_i;
  dot_k;
  dot_bs_hd
}

let bitms_lt_k_ops = {
  map_add=(fun k v t -> t.(k) <- v; t);
  map_find=(fun k t -> t.(k));
  map_empty=(Array.make (input_length + 1) map_nt_ops.map_empty);   (* FIXME +1? *)
  map_remove=(fun k t -> failwith __LOC__);  (* not used *)
}

let cut = S.cut

(* Finally, run Earley! *)

let main () = 
  run_earley ~nt_item_ops ~bitms_lt_k_ops ~cut ~new_items ~input ~parse_tm ~input_length ~init_nt 
  |> fun s -> s.k |> string_of_int |> print_endline

let _ = main () 

(* 

$ bin $ time ./test2.native 400
400

real	0m2.508s
user	0m2.484s
sys	0m0.016s

# (h:pc1177) (p:~/l/github/p_tjr_simple_earley/src/bin) (d:/dev/loop7[/git])  [dev !?]

*)




