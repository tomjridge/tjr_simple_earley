(** {2 Config and util} *)

module Config = Config

module Misc = Misc

(* module Prelude = Prelude *)


(** {2 Interfaces} *)

module Earley_intf = Earley_intf


(** {2 Internal Earley implementation} *)

module Earley_base = Earley_base


(** {2 Earley implementation} *)

module Earley_simple = Earley_simple

module Earley_with_symbols_as_string = Earley_simple.Make(struct
    type nt = string
    type tm = string
  end)

(** Lift the parsing function to the top level of the library *)
let run_earley_parser = Earley_with_symbols_as_string.run_earley_parser


(** {2 Experimental unstaged (but efficient) parsing algorithm} *)

module Earley_unstaged = Earley_unstaged


(** {2 A simple specification of parsing} *)

module Earley_spec = Earley_spec

