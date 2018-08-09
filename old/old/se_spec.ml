(*{|scala object se_spec {|}*)
(*{|scala |}*)
(*{|scala val adoc=s"""|}*)
(* specification of Earley's algorithm *)
(*{|adoc |}*)
(*{|adoc == Earley specification (file: se_spec.ml)|}*)
(*{|adoc |}*)
(*{|adoc The spec state is just a set of nt items.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

open Se_common
open Se_common.Unstaged

type spec_item_t = nt_item

module Spec_t = struct 
  include Nt_item_set
      
  (* for < 4.02.0 *)
  let of_list: elt list -> t = (
    fun xs -> 
      List.fold_left (fun a b -> add b a) empty xs
  )

end

type spec_t = Spec_t.t
(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Given a blocked key (index,symbol) we can get a set of blocked items.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let spec_to_bitms: spec_t -> b_key_t -> bitm_t list = (
  fun s0 key -> 
    (Spec_t.elements s0)
    |> List.map (function
        | nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc At each stage, for each item, we generate (possibly-) new items.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let new_items : ctxt_t -> spec_t -> spec_item_t -> spec_item_t list = (
  fun c0 s0 nitm -> (
      let complete = (nitm.bs = []) in
      match complete with
      | true -> (
          let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
          (* let citm : citm_t = {k;sym;j} in *)
          let key = (k,sym) in
          let bitms = spec_to_bitms s0 key in
          let f bitm = cut bitm j in
          List.map f bitms)
      | false -> (
          (* blocked, so process next sym *)
          let bitm = nitm in
          let (k,sym) = (bitm.k,List.hd nitm.bs) in
          (* now look at symbol we are blocked on *)
          match sym with
          | NT nt -> (
              let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
              nitms)
          | TM tm -> (
              (* parse tm and complete with item *)
              let k = bitm.k in
              let p = c0.g0.p_of_tm tm in
              let js = p (c0.i0.str,k,c0.i0.len) in
              let f j = cut bitm j in
              List.map f js)
        )        
    )
)


(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The loop takes the existing elements, generates new items, and unions|}*)
(*{|adoc them back into the state. The loop stops when the set of items no|}*)
(*{|adoc longer changes.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
(* repeatedly apply step till no change *)
let rec spec' c0 s0 = (
  let new_itms = (
    (Spec_t.elements s0)
    |> List.map (new_items c0 s0)
    |> List.concat
    |> Spec_t.of_list)
  in
  let s1 = Spec_t.union s0 new_itms in
  if Spec_t.equal s1 s0 then s0 else spec' c0 s1)


(* construct initial context, apply spec' *)
let se_spec : ctxt_t -> nt -> Nt_item_set.t = (
  fun c0 nt ->
    let init = {nt;i=0;as_=[];k=0;bs=[NT nt]} in
    let s0 = Spec_t.of_list [init] in
    let s1 = spec' c0 s0 in
    Spec_t.remove init s1)  (* remove the dummy item *)
(*{|adoc ----|}*)
(*{|scala """|}*)
(*{|scala |}*)
(*{|scala val adoc2 = adoc.lines.map(s => |}*)
(*{|scala   if (s.startsWith("//adoc ")) s.substring(7) |}*)
(*{|scala   else if (s.startsWith("//ml ")) s.substring(5)|}*)
(*{|scala   else s).mkString("\n")|}*)
(*{|scala |}*)
(*{|scala // write doc to README.scala.adoc|}*)
(*{|scala |}*)
(*{|scala |}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala import java.io._|}*)
(*{|scala new PrintWriter("se_spec.pp.adoc") { write(se_spec.adoc2); close }|}*)
(*{|scala |}*)
