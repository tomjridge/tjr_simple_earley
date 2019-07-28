(** {2 Logging} 

NOTE logging is enabled/disabled by a ppx_optcomp flag.
*)

[%%import "earley_optcomp_config.ml"]

[%%if LOGGING_ENABLED]
let log = fun (x:unit Lazy.t) -> Lazy.force x [@@inline]
[%%else]
let log = fun (x:unit Lazy.t) -> () [@@inline]
[%%endif]



