let input_length = ref 0
let input = ref ""

open Tjr_simple_earley

let grammar = ref (Examples.get_grammar_by_name "EEE")
