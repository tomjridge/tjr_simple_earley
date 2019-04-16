(*

We are given:

- types for terminals and nonterms (nonterms likely ints, but we shouldn't assume this)
- for nts, a *function* that takes an nt and provides the rhss for the rules
- for a given nonterminal X appearing as part of the rhs of a rule st

   Y -> i alpha X j beta  

   i.e., (...something about Y and the rule...); (i, alpha X beta) is reachable from start sym; (i, alpha X, j) matches the input

   we are given a function which takes (i, X, j) and produces those k such that

   k X j  where k >= i  

  Other choices for the parse info are possible here, since we know a lot more about how the rhs matched - FIXME think about what the *right* choice is

  Another (better? at least it fits better with terminal parsers) is to work forwards from i, ie the oracle takes (i,X,j) and produces k such that (i,X,k) and j <= j


- for each rhs, we are also given an action to apply

- for the rhs, at the moment we assume the implementation is via gadt as in p1_extra/parsing_dsl2.ml, but with elts either tm or nt

*)

(* inefficient? *)
let all_pairs f xs ys = 
  xs |> List.rev_map f
  |> List.rev_map (fun fx -> List.map fx ys)
  |> List.concat

let _ = all_pairs



type 'unt ctxt = C_empty | C of int * int * 'unt list  (* really set *)

let ctxt_contains ~nt2u ~ctxt (i,_X,j) = 
  match ctxt with
  | C_empty -> false
  | C (i',j',nts) -> (i,j) = (i',j') && List.mem (nt2u _X) nts

let ctxt_add ~nt2u ~ctxt (i,_X,j) =
  let _X = nt2u _X in
  match ctxt with
  | C_empty -> C(i,j,[_X])
  | C (i',j',nts) ->
    assert(i'<=i&& j<=j');
    match (i,j) = (i',j') with
    | true -> (
        assert(not (List.mem _X nts));
        C (i',j',_X::nts))
    | false -> C (i,j,[_X])



module type REQUIRES = sig

  type 'a nt

  type untyped_nt  (* needed for the p1 context *)

  val nt2u : 'a nt -> untyped_nt

  type 'a tm

  type tm_ops = {
    parse_tm: 'a. 'a tm -> int -> int -> 'a list  (* takes i,j and returns vs *)
  }

  type 'a elt

  val elt_case: nt:('a nt -> 'b) -> tm:('a tm -> 'b) -> 'a elt -> 'b


  (* fix a type for elts *)
  type _a

  type 'z rhs = Rhs of _a elt list * (_a list -> 'z)

  type nt_ops = {
    expand: 'a. 'a nt -> 'a rhs list
  }

  type oracle_ops = {
    cut: 'a. int -> 'a elt -> int -> int list  (* NOTE this takes i,E,j and produces k such that k <= j and i,E,k *)
  }

end


module Make(Requires:REQUIRES) = struct

  open Requires

(* 

For nonterminals X: the executor takes a range i,j; it calls expand to
get the rhss, then for each rhs it looks at the first sym S
and calls the oracle on i,S,j; the ks are returned and then it
recursively calls itself on the remainder of the rhs, gets the results,
combines each with the values returned from recursive call on (k,S,j), and
merges the results

*)


  type nonrec ctxt = untyped_nt ctxt

  type rec_calls = {
    exec_nt_elt:  'a. ctxt -> int -> int -> 'a nt -> 'a list;  (* provide to users *)
    exec_rhs: 'a. ctxt -> int -> int -> 'a rhs -> 'a list
  }

  let execute ~rec_calls ~oracle_ops ~tm_ops ~nt_ops =
    (* results for i,_X,j *)
    let exec_nt_elt ctxt i j _X =
      match ctxt_contains ~nt2u ~ctxt (i,_X,j) with
      | true -> []
      | false -> 
        let ctxt = ctxt_add ~nt2u ~ctxt (i,_X,j) in
        let rhss = nt_ops.expand _X in
        rhss |> List.map (fun rhs -> (rec_calls ()).exec_rhs ctxt i j rhs) |> List.concat
    in
    let exec_tm_elt i j tm = tm_ops.parse_tm tm i j in
    let exec_elt ctxt i j _Y = (_Y |> elt_case 
                            ~nt:(fun _Y -> exec_nt_elt ctxt i j _Y)
                            ~tm:(fun tm -> exec_tm_elt i j tm))
    in
    (* results for i,rhs,j *)
    let exec_rhs ctxt i j = function
      | Rhs ([],f) -> failwith __LOC__
      | Rhs ([_Y],f) -> 
        (* NOTE f is expecting a list of length 1 *)
        let rs : _a list = exec_elt ctxt i j _Y in
        let rs' = rs |> List.map (fun x -> f [x]) in
        rs'
      | Rhs (_Y::es,f) -> 
        oracle_ops.cut i _Y j 
        |> List.map (fun k -> 
            exec_elt ctxt i k _Y |> fun xs ->
            (rec_calls ()).exec_rhs ctxt k j (Rhs (es,fun x -> x)) |> fun ys ->
            all_pairs (fun x y -> f (x::y)) xs ys)
        |> List.concat
    in  
    (exec_nt_elt,exec_rhs)

  let _ = execute

  let execute ~oracle_ops ~tm_ops ~nt_ops =
    let rec execute' () = execute ~rec_calls ~oracle_ops ~tm_ops ~nt_ops
    and rec_calls = fun () -> 
      let (exec_nt_elt,exec_rhs) = execute' () in 
      let exec_nt_elt : ctxt -> int -> int -> 'a nt -> 'a list = exec_nt_elt in
      let exec_rhs : ctxt -> int -> int -> 'a rhs -> 'a list = exec_rhs in
      { exec_nt_elt=(Obj.magic exec_nt_elt); exec_rhs=(Obj.magic exec_rhs) }
    in
    rec_calls

  let _ = execute


  (* TODO: add memoization *)

end


module Test = struct

  module Requires = struct
    type 'a nt = E
    let _E : int nt = E

    type untyped_nt = int
    let nt2u x = 0
    type 'a tm = Eps | One
    type tm_ops = {
      parse_tm: 'a. 'a tm -> int -> int -> 'a list  (* takes i,j and returns vs *)
    }
    type 'a elt = Nt of 'a nt | Tm of 'a tm

    let elt_case: nt:('a nt -> 'b) -> tm:('a tm -> 'b) -> 'a elt -> 'b = fun ~nt ~tm e ->
      match e with
      | Nt x -> nt x
      | Tm x -> tm x

    type _a
    type 'z rhs = Rhs of _a elt list * (_a list -> 'z)

    type nt_ops = {
      expand: 'a. 'a nt -> 'a rhs list
    }

    type oracle_ops = {
      cut: 'a. int -> 'a elt -> int -> int list  (* NOTE this takes i,E,j and produces k such that k <= j and i,E,k *)
    }

  end

  open Requires

  module Made = Make(Requires)

  open Made

  let execute : 
    oracle_ops:Requires.oracle_ops ->
    tm_ops:Requires.tm_ops -> nt_ops:Requires.nt_ops -> unit -> Made.rec_calls
    = Made.execute

  let oracle_ops = {
    cut=fun i _E j -> match _E with
      | Nt E -> Tjr_list.from_to i j
      | Tm Eps -> [i]
      | Tm One -> if i < j then [i+1] else []
  }

  let tm_ops = {
    parse_tm= 
      let f  : int tm -> int -> int -> int list  = fun tm i j -> match tm with
      | Eps -> if i=j then [0] else []
      | One -> if i+1=j then [1] else []
      in
      Obj.magic f
  }

  let nt_ops = {
    expand=
      let f = fun (x: int nt) -> match x with
      | E -> [
          Rhs ([Nt E; Nt E; Nt E],fun [e1;e2;e3] -> 
              let (e1,e2,e3) = Obj.magic (e1,e2,e3) in
              e1+e2+e3);
          Rhs ([Tm One],fun [e1] -> Obj.magic e1);
          Rhs ([Tm Eps],fun [e1] -> Obj.magic e1);
        ]
      in
      Obj.magic f
  }

  let exec_nt = (execute ~oracle_ops ~tm_ops ~nt_ops ()).exec_nt_elt

  let run_test len = 
    let r = exec_nt C_empty 0 len _E in
    List.iter (fun i -> Printf.printf "%d\n" i) (r : int list)


  let main () = run_test 3

end


let _ = Test.main ()
