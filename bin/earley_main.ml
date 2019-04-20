let _ = 
  match Sys.argv.(1),(Sys.argv.(2)|>int_of_string) with
  | "simple",len -> 
    Params.input_length := len;
    Params.input := String.make len '1';
    let module M = Simple_test.Make() in ()
  | "spec",len -> 
    Params.input_length := len;
    Params.input := String.make len '1';
    let module M = Test_spec.Make() in ()
  | "unstaged",len -> 
    Params.input_length := len;
    Params.input := String.make len '1';
    let module M = Test_unstaged.Make() in ()
