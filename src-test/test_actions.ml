(** An experiment to test applying actions to the result of an Earley
   parse. *)

open Misc
open Spec_common
(* open Test_unstaged *)
open Test_unstaged.Internal
       
let main () = 
  (* let grammar = Examples.get_grammar_by_name !Params.grammar in *)
  let grammar = Examples_with_actions._EEE in
  (* NOTE in the following the initial nt returns an int *)
  let initial_nt : int Examples_with_actions.nt = grammar.initial_nt in

  (* First step is to run the basic Earley parse *)

  let expand_nt (nt,i) = 
    grammar.rules nt |> fun rhss ->
    rhss |> List.map (fun (rhs,_act) -> { nt; i_=i; k_=i; bs=rhs })
  in
  (* FIXME prefer to get expand_tm from the defn site of grammar *)
  let expand_tm (tm,i) =     
    let input = !Test_params.input in
    match Misc.string_matches_at ~string:input ~sub:tm ~pos:i with
    | true -> [i+(String.length tm)] 
    | false -> []
  in
  let u_nt = Examples_with_actions.nt2u initial_nt in
  earley_unstaged ~expand_nt ~expand_tm ~initial_nt:u_nt
  |> fun { count; complete_items; _ } -> 
  Printf.printf "%d nt_items produced (%s)\n%!"
    count
    __FILE__;
  Printf.printf "Initial nt parsed to %d\n" (
    let set = complete_items (0,Nt "E",[]) in
    Misc.Int_set.max_elt set
  );
(*  debug |> Hashtbl.iter (fun (i,_S,bs) set -> 
    Printf.printf "%d |%2s| %s (%s)\n%!" i (_S|>sym_to_string) (bs |> syms_to_string)
      (set |> Int_set.elements |> List.map string_of_int |> String.concat " ")); *)
  
  (* After the Earley parse, we can apply the actions *)

  let module Internal_req = struct
    type nt = Examples_with_actions.u_nt
    type tm = Examples_with_actions.u_tm
    type sym = Examples_with_actions.u_sym
    type uni_val = Examples_with_actions.uni_val

    (** This must match the type of initial_nt *)
    let u2int : uni_val -> int = fun u -> Obj.magic u
    let int2u : int -> uni_val = fun i -> Obj.magic i

  end 
  in
  let module Actions = Actions.Internal(Internal_req) in  
  let get_rhss ~nt = 
    (* assert(Printf.printf "Nonterm is %s\n" nt; nt="E"); *)
    let rhss = grammar.rules nt in
    rhss
  in
  let cut i (sym,syms) j = 
    assert(syms<>[]);
    complete_items (i,sym,syms) |> fun set -> 
    (* get elts leq j *)
    Int_set.split (j+1) set |> fun (set,_,_) -> 
    Int_set.elements set |> List.rev  (* reverse order *)
  in
  let apply_tm ~tm ~i ~j = 
    (* just return the length (0 or 1) of the string parsed *)
    assert(j-i = 0 || j-i=1);
    (* Printf.printf "apply_tm: %s %d %d\n%!" tm i j; *)
    (* FIXME need to make this a bit nicer... need terminal parsers to
       also include actions *)
    match tm with 
    | "" -> if j=i then Some (0|>Internal_req.int2u) else None
    | "1" -> (if i+1=j then Some(1|>Internal_req.int2u) else None)
    | _ -> failwith __LOC__
  in 
  let is_nt = function Nt _x -> true | Tm _x -> false in
  let dest_nt = function Nt x -> x | Tm _x -> failwith "dest_nt" in
  let dest_tm = function Tm x -> x | Nt _x -> failwith "dest_tm" in
  (* initial_nt already bound *)
  Actions.apply_actions
    ~is_nt ~dest_nt ~dest_tm 
    ~get_rhss
    ~cut
    ~apply_tm
    ~nt:u_nt
    ~i:0
    ~j:(String.length !Test_params.input)
    []
  |> function
  | None -> (Printf.printf "No result!\n%!")
  | Some i -> Printf.printf "Result was %d\n%!" (Internal_req.u2int i)

  
