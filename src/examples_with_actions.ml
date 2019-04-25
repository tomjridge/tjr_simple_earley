(** Some examples, with actions *)


(** Internal: grammmars defined abstractly *)
module Internal(A: sig type 'a nt type 'a sym type 'a rhs type 'a rule end) = struct

  open A

  type rhs' = {
    _1: 'a 'b. 'a sym -> ('a -> 'b) -> 'b rhs;
    _2: 'a 'b 'c. ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs;
    _3: 'a 'b 'c 'd. ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs;
  }

  let example_grammars p =
    let ( --> ) (nt:'a nt) (rhs:'a rhs) : 'a rule = p#make_rule nt rhs in
    let nt (x:'a nt) : 'a sym = p#nt x in
    let {_1;_2;_3} = p#rhs' in
    let _E : int nt = p#_E in
    let _S = p#_S in
    let a s = p#a s in
    let [(one:string sym);eps;x] = List.map a ["1";"";"x"] in
    let _EEE = 
      p#grammar 
        ~name:"EEE"
        ~descr:"Very ambiguous grammar, for testing Earley"
        ~initial_nt:_E
        ~rules:[
          _E -->_3 (nt _E,nt _E,nt _E) (fun (x,y,z) -> x+y+z);
          _E -->_1 one (fun _ -> 1);
          _E -->_1 eps (fun _ -> 0);
        ]
    in
    let aho_s = 
      p#grammar
        ~name:"aho_s"
        ~descr:"Aho et al. example grammar"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,nt _S,nt _S) (fun (x,y,z) -> 1+y+z);
          _S -->_1 eps (fun _ -> 0)
        ]
    in
    [_EEE;aho_s]

  let _ = example_grammars
end


(** A named tuple for tagging grammars in a slightly more digestible
   form than a plain tuple *)
type ('a,'b) grammar = {
  name:string;
  descr:string;
  initial_nt:'a;
  rules:'b
}

(*

(** Example instantiation with strings for symbols *)
module Example_instantiation = struct

  type nt = string
  type tm = string
  type sym = string
  type rule = nt * sym list

  let make_rule nt rhs = (nt,rhs)
                         
  type u
  let u (x:'a) : u = Obj.magic x

  let _1 s (act:'a -> 'b) = 
    let act: u -> u = fun v -> (Obj.magic v) |> act |> u in
    ([s],fun [v] -> act v)

  let _2 (s1,s2) (act: 'b*'c -> 'a) = 
    let act: u * u -> u = fun (v1,v2) -> act (Obj.magic v1,Obj.magic v2) |> u in
    ([s1;s2],fun [v1;v2] -> act (v1,v2))

  let _3 (s1,s2,s3) act = 
    let act: u * u * u -> u = fun (v1,v2,v3) -> act (Obj.magic v1,Obj.magic v2,Obj.magic v3) |> u in
    ([s1;s2;s3],fun [v1;v2;v3] -> act (v1,v2,v3))

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

    let get_grammar_by_name name = 
      example_grammars |> List.find (fun g -> g.name = name)


    (** We also want to get grammars with type [grammar_etc] *)

    open Prelude

    (** Hack to determine nt/tm based on string repr starting with a
       capital letter *)
    let is_nt nt = nt <> "" && (String.get nt 0 |> function
      | 'A' .. 'Z' -> true
      | _ -> false)
      
    open Spec_types
    let string_to_sym s = match is_nt s with 
      | true -> Nt s
      | false -> Tm s

    (** NOTE this returns a partial [grammar_etc] (input and
       input_length are dummies), and nt_items are a tuple
       [(nt,i,k,bs)] *)
    let _get_grammar_etc_by_name name = 
      get_grammar_by_name name 
      |> fun { rules; _ } ->
      let new_items ~nt ~input ~pos = 
        rules |> Misc.rev_filter_map (function (nt',(rhs,act)) ->
            match nt'=nt with
            | false -> None
            | true -> 
              let bs = List.map string_to_sym rhs in
              Some {nt;i_=pos;k_=pos;bs});
      in
      let parse_tm ~tm ~input ~pos ~input_length =
        match Misc.string_matches_at ~string:input ~sub:tm ~pos with
        | true -> [pos+(String.length tm)]
        | false -> []
      in
      { new_items; parse_tm; input=""; input_length=(-1) }

    let get_grammar_etc_by_name ~name ~input ~input_length = 
      _get_grammar_etc_by_name name |> fun g ->
      { g with input; input_length }
    (** Returns a non-partial [grammar_etc] *)
    


  end
  
end

include Example_instantiation.Export
*)
