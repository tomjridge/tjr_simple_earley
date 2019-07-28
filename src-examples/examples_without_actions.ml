(** Some examples *)


(** Example grammar
   names are: EEE, aho_s, aho_sml, brackets, S_xSx 

{%html:

<pre>
    let _EEE = 
      p#grammar 
        ~name:"EEE"
        ~descr:"Very ambiguous grammar, for testing Earley"
        ~initial_nt:_E
        ~rules:[
          _E -->_3 (_E,_E,_E);
          _E -->_1 one;
          _E -->_1 eps;
        ]
    in
    let aho_s = 
      p#grammar
        ~name:"aho_s"
        ~descr:"Aho et al. example grammar"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,_S,_S);
          _S -->_1 eps
        ]
    in
    let aho_sml = 
      p#grammar
        ~name:"aho_sml"
        ~descr:"Aho et al. example grammar 2"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (_S,_S,x);
          _S -->_1 eps
        ]
    in
    let brackets = 
      p#grammar
        ~name:"brackets"
        ~descr:
          "Well-bracketed expressions, in a particular nasty form for parsing"
        ~initial_nt:_E
        ~rules:[
          _E -->_2 (_E,_E);
          _E -->_3 (a"(",_E,a")");
          _E -->_1 eps
        ]
    in
    let _S_xSx = 
      p#grammar 
        ~name:"S_xSx"
        ~descr:"Unambiguous grammar that favours right-most parsers"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,_S,x);
          _S -->_1 x
        ]
    in
</pre>

%}

*)


(** Internal: grammmars defined abstractly. NOTE the following assumes
   nt, tm and sym are all the same type. *)
module Internal = struct

  let example_grammars p =
    let ( --> ) = p#make_rule in
    let _1,_2,_3 = p#_1,p#_2,p#_3 in
    let _E,_S,a = p#_E,p#_S,p#a in
    let [one;eps;x] = List.map a ["1";"";"x"] in
    let _EEE = 
      p#grammar 
        ~name:"EEE"
        ~descr:"Very ambiguous grammar, for testing Earley"
        ~initial_nt:_E
        ~rules:[
          _E -->_3 (_E,_E,_E);
          _E -->_1 one;
          _E -->_1 eps;
        ]
    in
    let aho_s = 
      p#grammar
        ~name:"aho_s"
        ~descr:"Aho et al. example grammar"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,_S,_S);
          _S -->_1 eps
        ]
    in
    let aho_sml = 
      p#grammar
        ~name:"aho_sml"
        ~descr:"Aho et al. example grammar 2"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (_S,_S,x);
          _S -->_1 eps
        ]
    in
    let brackets = 
      p#grammar
        ~name:"brackets"
        ~descr:
          "Well-bracketed expressions, in a particular nasty form for parsing"
        ~initial_nt:_E
        ~rules:[
          _E -->_2 (_E,_E);
          _E -->_3 (a"(",_E,a")");
          _E -->_1 eps
        ]
    in
    let _S_xSx = 
      p#grammar 
        ~name:"S_xSx"
        ~descr:"Unambiguous grammar that favours right-most parsers"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,_S,x);
          _S -->_1 x
        ]
    in
    [_EEE;aho_s;aho_sml;brackets;_S_xSx]

  let _ = example_grammars
end



(** Example instantiation with strings for symbols *)
module Internal_example_instantiation = struct

  (** A named tuple for tagging grammars in a slightly more digestible
      form than a plain tuple *)
  type ('a,'b) grammar = {
    name:string;
    descr:string;
    initial_nt:'a;
    rules:'b
  }


  open Earley_intf

  type nt = string
  type tm = string
  type sym = (nt,tm) Earley_intf.generic_sym
  type rule = nt * sym list

  (** Hack to determine nt/tm based on string repr starting with a
      capital letter *)
  let is_nt nt = nt <> "" && (String.get nt 0 |> function
    | 'A' .. 'Z' -> true
    | _ -> false)

  let string_to_sym s = match is_nt s with 
    | true -> Nt s
    | false -> Tm s

  let make_rule nt rhs = (nt,rhs|>List.map string_to_sym)

  let _1 s = [s]
  let _2 (s1,s2) = [s1;s2]
  let _3 (s1,s2,s3) = [s1;s2;s3]

  let _E = "E"
  let _S = "S"
  let a s = s
  let eps = ""
  let one = "1"
  let x = "x"

  let grammar ~name ~descr ~initial_nt ~rules = {name;descr;initial_nt;rules}

  let example_grammars = 
    let p = object
      method _1 = _1
      method _2 = _2
      method _3 = _3
      method make_rule = make_rule
      method grammar = grammar
      method _E = _E
      method _S = _S
      method a = a
      method eps = eps
      method one = one
      method x = x
      end
    in
    Internal.example_grammars p

  let _ = example_grammars

  module Export = struct

    (** NOTE nonterminals and terminals are represented by strings *)

    let grammar_names = ["EEE";"aho_s";"aho_sml";"brackets";"S_xSx"]

    let get_grammar_by_name name : (nt,tm) Earley_intf.simple_grammar * nt = 
      let g = example_grammars |> List.find (fun g -> g.name = name) in
      let tbl = Hashtbl.create 100 in
      List.rev g.rules |> List.iter (fun (nt,rhs) -> 
        Hashtbl.find_opt tbl nt |> function
        | None -> Hashtbl.replace tbl nt [rhs]
        | Some rhss -> Hashtbl.replace tbl nt (rhs::rhss));
      let nt_to_rhss nt = Hashtbl.find_opt tbl nt |> function
      | None -> []
      | Some rhss -> rhss
      in
      ({nt_to_rhss},g.initial_nt)


  end
  
end

include Internal_example_instantiation.Export
