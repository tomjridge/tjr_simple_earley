open Se_common
open Se_profile
open Se_staged

open Se_examples.E_EEE

let _ = se_staged (c0 (String.make 200 'x')) e'
let _ = print_endline "Finished"

let _ = assert(print_logs())
    
(* 
Sample time 

For length 100
Finished

real	0m0.124s
user	0m0.120s
sys	0m0.004s

For length 200: 
Finished

real	0m0.958s
user	0m0.950s
sys	0m0.008s


For length 400:
Finished

real	0m8.228s
user	0m8.219s
sys	0m0.012s

500:
Finished

real	0m16.456s
user	0m16.411s
sys	0m0.048s

600:
Finished

real	0m28.890s
user	0m28.842s
sys	0m0.060s


*)
