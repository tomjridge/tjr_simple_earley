(** Internal; work-in-progress *)

type sym' = E | One | Eps

let example_grammar p =
  let ( --> ) = p#make_rule in
  let _1,_3 = p#_1,p#_3 in
  p#grammar [
    E -->_3 (E,E,E);
    E -->_1 One;
    E -->_1 Eps;
  ]

let p =
  let make_rule a b = (a,b) in
  let hs1 x = `Rhs1 x in
  let hs3 x = `Rhs3 x in
  let grammar x = x in
  object
    method _1=hs1
    method _3=hs3
    method make_rule=make_rule
    method grammar=grammar
  end

let ex = example_grammar p
(* [(E, `Rhs3 (E, E, E)); (E, `Rhs1 One); (E, `Rhs1 Eps)] *)



(* alternative, with lists ------------------------------------------ *)

type sym2 = E | One | Eps | F | G

let example_grammar make =
  let ( --> ) = make#rule in
  make#rules [
    E --> [E;E;E];
    E --> [One];
    E --> [Eps];
    F --> [E];
    G --> [F];
  ],
  make#terminals (function 
      | One -> (fun x -> failwith "FIXME")
      | Eps -> (fun x -> failwith "FIXME"))

let p =
  let make_rule a b = (a,b) in
  let rules x = 
    (* map from nt to list of rules *)
    let tbl = Hashtbl.create 100 in
    x |> List.iter (fun (nt,rhs) -> 
        let lst = 
          Hashtbl.find_opt tbl nt |> function
          | None -> (Hashtbl.add tbl nt []; [])
          | Some xs -> xs
        in
        Hashtbl.replace tbl nt (rhs::lst));
    tbl |> Hashtbl.to_seq |> List.of_seq |> fun rules -> 
    let nts = List.map fst rules in
    (nts, rules)
  in
  let terminals f = f in
  (* FIXME should check that we either have a rule for an nt, or that
     we have a terminal parser; also, that terminals and nts do not
     overlap; or at least display a warning if we encounter these
     scenarios *)
  object
    method rule=make_rule
    method rules=rules
    method terminals=terminals
  end


let _ = example_grammar p
(*
([E; F; G], [(E, [[Eps]; [One]; [E; E; E]]); (F, [[E]]); (G, [[F]])])
*)



let example_grammar make =
  let ( --> ) = make#rule in
  let _1,_3 = make#_1,make#_3 in
  make#rules [
    E -->_3 [E;E;E] (fun (a,b,c) -> ());
    E -->_1 [One] (fun a -> ());
    E -->_1 [Eps] (fun _ -> ());  (* FIXME NOTE actions have to have
                                     same type... so could present
                                     rules as a list of lists *)
  ]

(* probably prefer version with tuples *)
