type ('nt,'tm,'nt_item,'input) grammar_etc = {
  new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
  parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
  input:'input;
  input_length:int;
}


(** {2 Some profiling parameters, set later in bin/} *)

let _ = Log.log @@ lazy (Printf.printf "%s: various mark_ref globals\n%!" __FILE__)

let base_mark_ref : (string -> unit) ref = ref (fun s -> ())

let spec_mark_ref : (string -> unit) ref = ref (fun s -> ())

let unstaged_mark_ref : (string -> unit) ref = ref (fun s -> ())
