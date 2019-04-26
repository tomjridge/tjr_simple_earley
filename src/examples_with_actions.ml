(** Some examples, with actions *)

(** Internal *)
module type A = sig 
  type 'a nt 
  type 'a sym 
  type 'a rhs 
  type rule 
  val _1: 'a sym -> ('a -> 'b) -> 'b rhs
  val _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs
  val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs

  val ( --> ) : 'a nt -> 'a rhs -> rule
  val nt : 'a nt -> 'a sym

  (* type u_nt  (\* untyped nt *\) *)

  type 'a grammar 
  val grammar: name:string -> descr:string -> initial_nt:'a nt -> rules:rule list -> 'a grammar
end

(** Internal: grammmars defined abstractly *)
module Internal(A:A) = struct

  open A

  let example_grammars p =
    let _E : int nt = p#_E in
    let _S : string nt = p#_S in
    let a s = p#a s in
    let [one;eps;x] : string sym list = List.map a ["1";"";"x"] in
    let _EEE = 
      grammar 
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
      grammar
        ~name:"aho_s"
        ~descr:"Aho et al. example grammar"
        ~initial_nt:_S
        ~rules:[
          _S -->_3 (x,nt _S,nt _S) (fun (x,y,z) -> "x"^y^z);
          _S -->_1 eps (fun _ -> "")
        ]
    in
    (_EEE,aho_s)

  let _ = example_grammars
end

module Internal2(B:sig 
    type 'a nt 
    type u_nt
    val nt2u : 'a nt -> u_nt
    type 'a sym 
    type u_sym
    val sym2u : 'a sym -> u_sym
    type uni_val 
end) = struct
  open B

  type u_rhs = u_sym list * (uni_val list -> uni_val)
  type rule = u_nt * u_rhs

  type 'a rhs = u_rhs

  let _1 (_S:'a sym) (f:'a -> 'b) : 'b rhs = 
    ([sym2u _S], fun [v] -> Obj.magic (f (Obj.magic v)))

  let _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs = 
    fun (s1,s2) f ->
    ([sym2u s1; sym2u s2], fun[v1;v2] -> Obj.magic (f (Obj.magic v1, Obj.magic v2)))

  let _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs =
    fun (s1,s2,s3) f ->
    [sym2u s1; sym2u s2; sym2u s3],
    fun [v1;v2;v3] -> Obj.magic (f (Obj.magic v1, Obj.magic v2, Obj.magic v3))

  let mk_rule : 'a nt -> 'a rhs -> rule = fun nt rhs -> (nt2u nt,rhs)
  let ( --> ) = mk_rule

  type 'a grammar = {
    name:string;
    descr:string;
    initial_nt:'a nt;
    rules:u_nt -> u_rhs list
  } 

  let grammar: name:string -> descr:string -> initial_nt:'a nt ->
    rules:rule list -> 'a grammar 
    =
    fun ~name ~descr ~initial_nt ~rules -> 
    let tbl = Hashtbl.create 100 in
    List.rev rules |> List.iter (fun (nt,rhs) -> 
        Hashtbl.find_opt tbl nt |> function
        | None -> Hashtbl.replace tbl nt [rhs]
        | Some rhss -> Hashtbl.replace tbl nt (rhs::rhss));
    let rules nt = Hashtbl.find_opt tbl nt |> function
      | None -> []
      | Some rhss -> rhss
    in
    { name; descr; initial_nt; rules }
end

module Internal3 = struct

  module B = struct
    type 'a nt = string
    type u_nt = string
    let nt2u : 'a nt -> u_nt = fun x -> x
    type 'a sym = string
    type u_sym = string
    let sym2u : 'a sym -> u_sym = fun x -> x
    type uni_val

    (* from A ; FIXME *)
    let nt : 'a nt -> 'a sym = fun x -> x
  end

  module C = Internal2(B)

  module E = struct
    include B
    include C
  end

  module D : A = E

  module F = Internal(E)

  module Export : sig
    type 'a grammar = 'a C.grammar
    (*val example_grammars : 
      < _E : string; _S : string; a : string -> string; .. > ->
      int grammar * string grammar*)
    val _EEE : int grammar
    val aho_s : string grammar
  end = struct
    type 'a grammar = 'a C.grammar
    let example_grammars = F.example_grammars
    let _EEE,aho_s = 
      let p = object 
        method _E="E"
        method _S="S"
        method a=fun s -> s
      end
      in
      example_grammars p
  end
  
end

include Internal3.Export


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
