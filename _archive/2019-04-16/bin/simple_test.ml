(* test, represent nt_item as a simple record *)
open Tjr_simple_earley
open Util.Map_ops 

(* simple test ------------------------------------------------------ *)

open Simple_ds.S

(* Encode nonterminals and terminals as ints; nts are even; tms are
   odd *)

let _E = 0
let eps = 1
let _1 = 3

(* Encode the grammar E -> E E E | "1" | eps *)
let rhss = [ [_E;_E;_E]; [_1]; [eps] ]

(* Provide a function that produces new items, given a nonterminal and
   an input position k *)
let new_items ~nt ~input ~k = match () with
  | _ when nt = _E -> 
    rhss   (* E -> E E E | "1" | eps *)
    |> List.map (fun bs -> { nt; i_=k; k_=k; bs})
  | _ -> failwith __LOC__

(* Example input; use command line argument *)
let input = String.make (Sys.argv.(1) |> int_of_string) '1'

(* Provide a function that details how to parse terminals at a given
   position k in the input *)
let parse_tm ~tm ~input ~k ~input_length = 
  match () with
  | _ when tm = eps -> [k]
  | _ when tm = _1 -> 
    (* print_endline (string_of_int k); *)
    if String.get input k = '1' then [k+1] else []
  | _ -> failwith __LOC__

let input_length = String.length input

(* Initial nonterminal *)
let init_nt = _E

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

let cut = Simple_ds.S.cut

(* Finally, run Earley! *)

open Simple_ds.Earley

let main () = 
  run_earley 
    ~nt_item_ops ~bitms_lt_k_ops ~cut ~new_items ~input 
    ~parse_tm ~input_length ~init_nt 
  |> fun s -> s.k |> string_of_int |> print_endline

let _ = main ()


(*
$ bin $ time ./simple_test.native 200
200

real	0m0.800s
user	0m0.796s
sys	0m0.000s


$ bin $ time ./simple_test.native 400
400

real	0m6.477s
user	0m6.476s
sys	0m0.000s

*)
