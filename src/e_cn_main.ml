open E_common
open E_cn

open E_examples.E_EEE


(* l:pq *)

let _ = earley (c0 (String.make 400 'x')) e'
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
