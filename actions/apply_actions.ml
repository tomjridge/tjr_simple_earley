(*

We are given:

- types for terminals and nonterms (nonterms likely ints, but we shouldn't assume this)
- for nts, a *function* that takes an nt and provides the rhs for the rule
- for a given nonterminal X appearing as part of the rhs of a rule st

   Y -> i alpha X j beta  

   i.e., (...something about Y and the rule...); (i, alpha X beta) is reachable from start sym; (i, alpha X, j) matches the input

   we are given a function which takes (i, X, j) and produces those k such that

   k X j  where k >= i  

  Other choices for the parse info are possible here, since we know a lot more about how the rhs matched - FIXME think about what the *right* choice is

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

  val elt_case: 'a elt -> nt:('a tm -> 'b) -> tm:('a tm -> 'b) -> 'b

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

end
