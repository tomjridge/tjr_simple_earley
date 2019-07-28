open Tjr_simple_earley

(** Encode input as :CxNNNN where C is a character, and NNNN is a number *)
let make_input i =
  match i<>"" && i.[0]=':' && i.[2]='x' with
  | true -> 
    let c = i.[1] in
    let n = int_of_string (String.sub i 3 (String.length i -3)) in
    String.make n c
  | false -> i

let _ = 
  Printf.printf "begin -------------------------------------------------\n%!";
  begin 
    match Array.to_list Sys.argv |> List.tl with
    | ["simple";grammar;input] -> 
      let input = make_input input in
      Test_params.input := input;
      Test_params.grammar:=grammar;
      Printf.printf "parser=simple; grammar=%s; input_length=%d (%s)\n%!" 
        grammar 
        (String.length input)
        __FILE__;
      Simple_test.main()
    | ["spec";grammar;input] -> 
      let input = make_input input in
      Test_params.input := input;
      Test_params.grammar:=grammar;
      Printf.printf "parser=spec; grammar=%s; input_length=%d (%s)\n%!" 
        grammar 
        (String.length input)
        __FILE__;
      Test_spec.main()
    | ["unstaged";grammar;input] -> 
      let input = make_input input in
      Test_params.input := input;
      Test_params.grammar:=grammar;
      Printf.printf "parser=unstaged; grammar=%s; input_length=%d (%s)\n%!" 
        grammar 
        (String.length input)
        __FILE__;
      Test_unstaged.main ()
    | ["actions";(*grammar;*)input] ->
      let input = make_input input in
      Test_params.input := input;
      (* Test_params.grammar := grammar; *)
      Printf.printf "actions; grammar=EEE; input_length=%d (%s)\n%!" 
        (String.length input)
        __FILE__;
      Test_actions.main ()
  end;
  Printf.printf "end ---------------------------------------------------\n%!";
