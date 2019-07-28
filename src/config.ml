[%%import "earley_optcomp_config.ml"]

[%%if PROFILING_ENABLED]
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

[%%if LOGGING_ENABLED]
let logging_enabled = true
[%%else]
let logging_enabled = false
[%%endif]


(*
(** {2 Some profiling parameters, set later in bin/} *)

(* let _ = Log.log @@ lazy (Printf.printf "%s: various mark_ref globals\n%!" __FILE__) *)

let base_mark_ref : (string -> unit) ref = ref (fun s -> ())

let spec_mark_ref : (string -> unit) ref = ref (fun s -> ())

let unstaged_mark_ref : (string -> unit) ref = ref (fun s -> ())
*)
