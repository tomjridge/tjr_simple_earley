(* main routine for se_simple *)

open Se_common
open Se_simple

open Se_examples.E_EEE


(* l:pq *)

let _ = se_simple (c0 (String.make 200 'x')) e'
let _ = print_endline "Finished"


(* sample timings

length 100:
Finished

real	0m0.308s
user	0m0.302s
sys	0m0.004s

200:
Finished

real	0m2.460s
user	0m2.452s
sys	0m0.008s


400:
Finished

real	0m20.496s
user	0m20.485s
sys	0m0.020s

*)
