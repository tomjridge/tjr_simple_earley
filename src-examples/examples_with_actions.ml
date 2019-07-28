(** Some examples, with actions *)

(**

{%html:

<pre>
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
</pre>

%}
*)

module Internal0 = struct
  module type INTERNAL_TYPED_SYMS = sig
    type 'a nt
    type 'a sym
    type 'a tm
    val _E : int nt
    val _S : string nt
    val a: string -> string tm
  end

  module type INTERNAL_REQS = sig 
    include INTERNAL_TYPED_SYMS
    type 'a rhs 
    type rule 
    val _1: 'a sym -> ('a -> 'b) -> 'b rhs
    val _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs
    val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs

    val ( --> ) : 'a nt -> 'a rhs -> rule
    val nt : 'a nt -> 'a sym
    val tm : 'a tm -> 'a sym

    (* type u_nt  (\* untyped nt *\) *)

    type 'a grammar 
    val grammar: name:string -> descr:string -> initial_nt:'a nt ->
      rules:rule list -> 'a grammar
  end

  module type INTERNAL2_REQS = sig 
    type 'a nt 
    type u_nt
    val nt2u : 'a nt -> u_nt
    type 'a sym 
    type u_sym
    val sym2u : 'a sym -> u_sym
    type uni_val 
  end

(*
  module type INTERNAL2_REQS = sig 
    include INTERNAL_ABSTRACT_TYPED_SYMS

    val u_nt2string: u_nt -> string
    val u_sym2string: u_sym -> string
  end
*)

end

open Internal0

(** Internal: grammmars defined abstractly *)
module Internal(A:INTERNAL_REQS) = struct

  open A

  let example_grammars =
    let [one;eps;x] : string sym list = List.map a ["1";"";"x"] |> List.map tm in
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
          _S -->_3 (x,nt _S,nt _S) (fun (_x,y,z) -> "x"^y^z);
          _S -->_1 eps (fun _ -> "")
        ]
    in
    (_EEE,aho_s)

  let _ = example_grammars
end

(** Internal2: translate grammars to untyped representation *)
module Internal2(B:INTERNAL2_REQS) = struct
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

  (* Implementation of INTERNAL2_REQS *)
  module Internal2_reqs : sig
    type 'a nt

    (** For debugging, make u_nt a string *)
    type u_nt = string 
    val nt2u : 'a nt -> u_nt

    type 'a tm
    type u_tm = string


    type 'a sym
    type u_sym = (u_nt,u_tm)Earley_intf.generic_sym
    val sym2u : 'a sym -> u_sym
    type uni_val
    val nt : 'a nt -> 'a sym
    val tm: 'a tm -> 'a sym
    val _E : int nt
    val _S : string nt
    val a : string -> string tm
  end = struct
    type 'a nt = string
    type u_nt = string
    let nt2u : 'a nt -> u_nt = fun x -> x

    type 'a tm = string
    type u_tm = string

    type u_sym = (u_nt,u_tm)Earley_intf.generic_sym
    type 'a sym = u_sym
    let sym2u : 'a sym -> u_sym = fun x -> x
    type uni_val

    let u_nt2string x = x
    let u_sym2string x = x

    (* from A ; FIXME *)
    let nt : 'a nt -> 'a sym = fun x -> Nt x
    let tm : 'a tm -> 'a sym = fun x -> Tm x

    let _E : int nt ="E"
    let _S : string nt = "S"
    let a s = s
  end

  (** NOTE use Internal2 *)
  module C = Internal2(Internal2_reqs)

  module A2 = struct
    include Internal2_reqs
    include C
  end

  module D : INTERNAL_REQS = A2

  (** NOTE use Internal *)
  module F = Internal(A2)

  (** Export abstract interface. *)
  module Export : sig
    type 'a grammar = 'a C.grammar
    val _EEE : int grammar
    val aho_s : string grammar
  end = struct
    type 'a grammar = 'a C.grammar
    let example_grammars = F.example_grammars
    let _EEE,aho_s = example_grammars
  end
  
end

open Internal3

(* include Internal3.Internal2_reqs *)

(** NOTE following types are present in the ['a grammar] type *)

(** Just a string *)
type u_nt = Internal3.Internal2_reqs.u_nt 

type 'a nt = 'a Internal3.Internal2_reqs.nt
let nt2u: 'a nt -> u_nt = Internal3.Internal2_reqs.nt2u

(** Just a string *)
type u_tm = Internal3.Internal2_reqs.u_tm 

(** A [generic_sym] over u_nt and u_tm *)
type u_sym = Internal2_reqs.u_sym

(** Used for actions *)
type uni_val = Internal2_reqs.uni_val

(** A list of u_sym, and an action *)
type u_rhs = Internal3.C.u_rhs


type 'a grammar = 'a Internal3.C.grammar


let _EEE : int grammar = Internal3.Export._EEE
let aho_s : string grammar = Internal3.Export.aho_s


