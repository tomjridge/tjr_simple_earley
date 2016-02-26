open E_common
open E_fg

open E_examples.E_EEE

let _ = earley (c0 (String.make 200 'x')) e'
let _ = print_endline "Finished"
    
(* 
Sample time for string length 200: 
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
