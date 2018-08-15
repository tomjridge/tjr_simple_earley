(* test2, represent nt_item as int *)

(* compared to test, we use lazy enumeration *)

open Earley_util.Map_ops
open Fast_datastructure_implementations

(* simple test ------------------------------------------------------ *)

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

open S

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

open Earley

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



let _ = Printf.printf "Number of items processed: %d\n%!" (!Earley.counter)
