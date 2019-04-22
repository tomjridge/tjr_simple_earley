open Tjr_simple_earley

let _ = 
  match Array.to_list Sys.argv |> List.tl with
  | ["simple";len] -> 
    let len = int_of_string len in
    Params.input_length := len;
    Params.input := String.make len '1';
    let module M = Simple_test.Make() in ()
  | ["spec";grammar;input] -> 
    Params.input_length := String.length input;
    Params.input := input;
    Params.grammar:=Examples.get_grammar_by_name grammar;
    let module M = Test_spec.Make() in ()
  | ["unstaged";grammar;input] -> 
    Params.input_length := String.length input;
    Params.input := input;
    Params.grammar:=Examples.get_grammar_by_name grammar;
    let module M = Test_unstaged.Make() in ()
