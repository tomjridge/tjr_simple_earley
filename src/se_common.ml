(*{|scala def l(s:String) = { "(l:"+s+")" }|}*)
(*{|scala |}*)
(*{|scala def nitm(nt:String,i:String,as:String,k:String,bs:String) = {|}*)
(*{|scala   s"""latexmath:[( $nt \\rightarrow {}_{$i} $as {}_{$k} . $bs)]"""|}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala def nitm_trad (nt:String,i:String,as:String,k:String,bs:String) = {|}*)
(*{|scala   s"""latexmath:[( $nt \\rightarrow $as . $bs, $i, $k)]"""|}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala val x = "X"|}*)
(*{|scala val as = "\\alpha"|}*)
(*{|scala val bs = "\\beta"|}*)
(*{|scala |}*)
(*{|scala val i = "i"|}*)
(*{|scala val k = "k"|}*)
(*{|scala val j = "j"|}*)
(*{|scala val s = "S"|}*)
(*{|scala val t = "T"|}*)
(*{|scala |}*)
(*{|scala val nt_item = "`nt_item`"|}*)
(*{|scala val tm_item = "`tm_item`"|}*)
(*{|scala |}*)
(*{|scala def titm(k:String,t:String,j:String) = {|}*)
(*{|scala   s"""latexmath:[( {}_{$k} ${t}_{$j})]"""|}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala def titm(k:String,t:String) = {|}*)
(*{|scala   s"""latexmath:[( {}_{$k} ${t}_{?})]"""|}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala def citm(k:String,s:String,j:String) = {|}*)
(*{|scala   s"""latexmath:[( {}_{$k} ${s}_{$j})]"""|}*)
(*{|scala }|}*)
(*{|scala |}*)
(*{|scala val adoc=s"""|}*)
(*{|adoc == Common definitions, all versions (file: se_common)|}*)
(*{|adoc |}*)
(*{|adoc Various types and definitions|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc FIXME move|}*)
(*{|adoc |}*)
(*{|adoc We work with `nt_items` only (terminal parses are not cached). No|}*)
(*{|adoc staging. latexmath:[O(n^3)].|}*)
(*{|adoc |}*)
(*{|adoc We define the map types that we need. From a set of|}*)
(*{|adoc complete items, we need to identify those for a given start index and|}*)
(*{|adoc symbol. This is the role of the complete map type `cm_t`. The codomain|}*)
(*{|adoc of the map is a set of integers latexmath:[j]. If latexmath:[j \\in|}*)
(*{|adoc cm(k,S)] then there is a complete item ${citm(k,s,j)}.|}*)
(*{|adoc |}*)
(*{|adoc The blocked map allows us to identify, from a set of nt items, those|}*)
(*{|adoc that are currently blocked at position latexmath:[k] waiting for a parse of|}*)
(*{|adoc symbol latexmath:[S] to complete. latexmath:[nitm \\in bm(k,S)] if latexmath:[nitm] is of the form ${nitm(x,i,as,k,s"S $bs")}.|}*)
(*{|adoc |}*)
(*{|adoc |}*)
(*{|adoc === Utility functions|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let find_with_default d f k m = try (f k m) with Not_found -> d

let rec while_not_nil: 'a list -> 's -> ('s -> 'a -> 's) -> 's = (
    fun xs s0 f -> List.fold_left f s0 xs)

(*{|adoc ----|}*)
(*{|adoc |}*)

(*{|adoc // ----------------------------------------|}*)
(*{|adoc === Common type definitions, all versions |}*)
(*{|adoc |}*)
(*{|adoc Indexes into the input|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

type k_t = int
type i_t = int
type j_t = int

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Nonterminals, terminals and symbols |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

type nt = int
type tm = int
type sym = NT of nt | TM of tm

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Items. Typically we only use `nt_item`, but the other items appear eg in `se_spec_all_items`.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;
    k: k_t;
    bs: sym list
  }

type tm_item = {
    k: k_t;
    tm: tm
  }

type sym_item = {
    k: k_t;
    sym: sym
  }

type citm_t = {
    k: k_t;
    sym: sym;
    j: j_t 
  }

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc Strings and substrings. We make the string type abstract to illustrate|}*)
(*{|adoc that we don't depend on any properties of strings (except their|}*)
(*{|adoc interaction with terminal parsers).|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

type string_t
type substring_t = (string_t * i_t * j_t)

let string_to_string_t: string -> string_t = (fun s -> Obj.magic s)
let string_t_to_string: string_t -> string = (fun s -> Obj.magic s)

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The grammar type, as a function, and the terminal parsers.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
type grammar_t = {
    nt_items_for_nt: nt -> (string_t * int) -> nt_item list;
    p_of_tm: tm -> substring_t -> k_t list
  }

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The input is a string. Since `string_t` is abstract, we also need the length.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
type input_t = {
    str: string_t;
    len: int;
  }

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc During a parse, we need the following context.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)
type ctxt_t = {
    g0: grammar_t;
    i0: input_t
  }

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|adoc The core Earley step involves cutting a blocked item against a complete item.|}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)


let cut: nt_item -> j_t -> nt_item = (
    fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm ))

(*{|adoc ----|}*)
(*{|adoc |}*)


(*{|adoc // ----------------------------------------|}*)
(*{|adoc === Blocked and complete maps? |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

module Unstaged = struct

  type bitm_t = nt_item  (* bs <> [] *)

  type b_key_t = (k_t * sym)

  let bitm_to_key: bitm_t -> b_key_t = (
      fun bitm -> (bitm.k,List.hd bitm.bs))

  type c_key_t = (k_t * sym)
               
  let citm_to_key: citm_t -> c_key_t = (
      fun citm -> (citm.k,citm.sym))

end

(*{|adoc ----|}*)
(*{|adoc |}*)

(*{|adoc // ----------------------------------------|}*)
(*{|adoc === Various sets and maps |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let comp = Pervasives.compare

module Int_set = 
  Set.Make(struct type t = int;; let compare: t -> t -> int = comp end)

module Nt_item_set = 
  Set.Make(struct type t = nt_item;; let compare: t -> t -> int = comp end)

module Sym_item_set = 
  Set.Make(struct type t = sym_item;; let compare: t -> t -> int = comp end)

module Nt_set = 
  Set.Make(struct type t = nt;; let compare: t -> t -> int = comp end)

module Map_tm =
  Map.Make(struct type t = tm;; let compare: t -> t -> int = comp end)

module Map_nt =
  Map.Make(struct type t = nt;; let compare: t -> t -> int = comp end)

module Map_int = 
  Map.Make(struct type t = int;; let compare: t -> t -> int = comp end)

(*{|adoc ----|}*)
(*{|adoc |}*)


(*{|adoc // ----------------------------------------|}*)
(*{|adoc === Debug support |}*)
(*{|adoc |}*)
(*{|adoc [source,ocaml]|}*)
(*{|adoc ----|}*)

let debug = ref false

let debug_endline = (
    fun x -> 
    if !debug then print_endline x else ())

let sym_to_string s = (match s with | NT nt -> Printf.sprintf "NT %d" nt | TM tm -> Printf.sprintf "TM %d" tm)

let sym_list_to_string ss = (ss |> List.map sym_to_string |> String.concat "," |> fun x -> "["^x^"]")

let nitm_to_string nitm = (
    Printf.sprintf "(%d %d %s %d %s)" 
                   nitm.nt nitm.i 
                   (sym_list_to_string nitm.as_) 
                   nitm.k 
                   (sym_list_to_string nitm.bs))

(*{|adoc ----|}*)
(*{|adoc |}*)
(*{|scala """|}*)
(*{|scala |}*)
(*{|scala val adoc2 = adoc.lines.map(s => |}*)
(*{|scala   if (s.startsWith("//adoc ")) s.substring(7) |}*)
(*{|scala   else if (s.startsWith("//ml ")) s.substring(5)|}*)
(*{|scala   else s).mkString("\n")|}*)
(*{|scala |}*)
(*{|scala // write doc to README.scala.adoc|}*)
(*{|scala |}*)
(*{|scala import java.io._|}*)
(*{|scala |}*)
(*{|scala new PrintWriter("se_common.pp.adoc") { write(adoc2); close }|}*)