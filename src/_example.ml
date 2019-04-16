open Impl.Earley_
open Impl


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
    (* print_endline __LOC__; *)
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

(* E -> E *)
let state : Earley_.state ref = 
  let todo = [{nt=_E;i_=0;k_=0;bs=[_E]}] in
  ref { empty_state with todo }

let () = 
  run_earley_parser
    ~state
    ~new_items
    ~input
    ~parse_tm
    ~input_length |> Tjr_monad.Imperative.from_m

(* result is the final state *)




(*  
time ./a.out 400

real	0m6.166s
user	0m6.156s
sys	0m0.012s

very similar to simple_test.native

*)

let _ = 
  Tjr_profile.print_profile_summary


(* profile info

$ experimental $ ./a.out 200
Time:0  198 198 count:1
Time:14088  191 202 count:200
Time:48124  205 158 count:200
Time:59311  181 209 count:400
Time:76791  194 158 count:200
Time:1359537  209 158 count:400
Time:2848920  158 166 count:20100
Time:3095653  202 205 count:200
Time:3147639  169 173 count:20100
Time:5157269  158 181 count:40801
Time:6673086  191 194 count:40201
Time:7737012  179 158 count:20100
Time:8421932  181 186 count:40401
Time:16927145  166 169 count:20100
Time:18279231  198 158 count:40000
Time:34573133  173 175 count:20100
Time:66783285  188 191 count:40401
Time:73524495  194 198 count:40001
Time:132225487  175 177 count:20100
Time:191734699  186 188 count:40401
Time:2174587455  177 179 count:20100


*)

