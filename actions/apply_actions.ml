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

module type REQUIRES = sig

  type 'a nt

  type 'a tm

  type input

  type tm_ops = {
    parse_tm: 'a. 'a tm -> int -> input -> (int*'a) list
  }

  type 'a elt

  val elt_case: nt:('a nt -> 'b) -> tm:('a tm -> 'b) -> 'a elt -> 'b

  type 'z rhs = 
    | Rhs1: 'a elt * ('a -> 'z) -> 'z rhs
    | Rhs2: ('a elt * 'b elt) * ('a * 'b -> 'z) -> 'z rhs
    | Rhs3: ('a elt * 'b elt * 'c elt) * ('a * 'b * 'c -> 'z) -> 'z rhs
    | Rhs4: ('a elt * 'b elt * 'c elt * 'd elt) * ('a * 'b * 'c * 'd -> 'z) -> 'z rhs
    | Rhs5: ('a elt * 'b elt * 'c elt * 'd elt * 'e elt) * 
            ('a * 'b * 'c * 'd * 'e -> 'z) -> 'z rhs
    | Rhs6: ('a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt) * 
            ('a * 'b * 'c * 'd * 'e * 'f -> 'z) -> 'z rhs
    | Rhs7: ('a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt * 'g elt) * 
            ('a * 'b * 'c * 'd * 'e * 'f * 'g -> 'z) -> 'z rhs

  type nt_ops = {
    expand: 'a. 'a nt -> 'a rhs list
  }

  (* FIXME perhaps we want to take 'a elt ? *)
  type oracle_ops = {
    cut: 'a. int -> 'a elt -> int -> int list  (* NOTE this takes i,E,j and produces k such that k <= j and i,E,k *)
  }

end


module Make(Requires:REQUIRES) = struct

  open Requires

  (* 

For nonterminals X: the executor takes a range i,j; it calls expand to
get the rhss, then for each rhs (not Rhs1!) it looks at the last sym S
and calls the oracle on i,S,j; the ks are returned and then it
recursively calls itself on the prefix of the rhs, gets the results,
combines each with the values returned from recursive call on (k,S,j), and
merges the results

*)


  type rec_calls = {
    exec_nt: 'a. int -> int -> 'a nt -> 'a list;
    exec_rhs: 'a. int -> int -> 'a rhs -> 'a list
  }


(*

FIXME the GADT types are not buying much and are rather inflexible. Is
there anyway to get almost-as-good typing without using GADTs?

*)

  let is_nt nt = true
  let dest_nt nt = failwith ""

  let execute ~oracle_ops ~nt_ops ~rec_calls =
    let exec_nt i j =
      let f : 'a nt -> 'a list = fun _X ->
        let rhss = nt_ops.expand _X in
        rhss |> List.map (fun rhs -> rec_calls.exec_rhs i j rhs) |> List.concat
      in
      f
    in
    let exec_rhs i j = 
      (* this is where we need to identify the first sym and recurse *)
      failwith ""
    in 
    let exec_rhs1 i j _Y f = (_Y |> elt_case 
                            ~nt:(fun _Y -> rec_calls.exec_nt i j _Y)
                            ~tm:(fun _ -> failwith "") |> List.map f)
    in
    let exec_rhs' i j = 
      let exec_rhs = rec_calls.exec_rhs in
      let f : 'a rhs -> 'a list = function
        | Rhs1 (_Y,f) -> exec_rhs1 i j _Y f
        | Rhs2((e1,e2),f) when is_nt e2 -> 
          let e2 = dest_nt e2 in
          oracle_ops.cut i e2 j 
          |> List.map (fun k -> 
              (* FIXME for the following we need to cut the range using the oracle *)
              exec_rhs i k (Rhs1(e1,fun x -> x)) |> fun xs ->
              exec_rhs k j (Rhs1(e2,fun x -> x)) |> fun ys ->
              List.map2 (fun x y -> f(x,y)) xs ys)
          |> List.concat
        | Rhs3((e1,e2,e),f) ->
          exec_rhs i j (Rhs2((e1,e2),fun x -> x)) |> fun xs ->
          exec_rhs i j (Rhs1(e,fun x -> x)) |> fun ys ->
          List.map2 (fun (x1,x2) y -> f(x1,x2,y)) xs ys
        | Rhs4((e1,e2,e3,e),f) ->
          exec_rhs i j (Rhs3((e1,e2,e3),fun x -> x)) |> fun xs ->
          exec_rhs i j (Rhs1(e,fun x -> x)) |> fun ys ->
          List.map2 (fun (x1,x2,x3) y -> f(x1,x2,x3,y)) xs ys
        | Rhs5((e1,e2,e3,e4,e),f) ->
          exec_rhs i j (Rhs4((e1,e2,e3,e4),fun x -> x)) |> fun xs ->
          exec_rhs i j (Rhs1(e,fun x -> x)) |> fun ys ->
          List.map2 (fun (x1,x2,x3,x4) y -> f(x1,x2,x3,x4,y)) xs ys
        | Rhs6((e1,e2,e3,e4,e5,e),f) ->
          exec_rhs i j (Rhs5((e1,e2,e3,e4,e5),fun x -> x)) |> fun xs ->
          exec_rhs i j (Rhs1(e,fun x -> x)) |> fun ys ->
          List.map2 (fun (x1,x2,x3,x4,x5) y -> f(x1,x2,x3,x4,x5,y)) xs ys
        | Rhs7((e1,e2,e3,e4,e5,e6,e),f) ->
          exec_rhs i j (Rhs6((e1,e2,e3,e4,e5,e6),fun x -> x)) |> fun xs ->
          exec_rhs i j (Rhs1(e,fun x -> x)) |> fun ys ->
          List.map2 (fun (x1,x2,x3,x4,x5,x6) y -> f(x1,x2,x3,x4,x5,x6,y)) xs ys

      in  
      f
    in
    exec_nt



end
