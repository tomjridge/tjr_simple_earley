(** A simple implementation of datastructures for {!Earley_base} *)
open Misc
open Prelude

(** The {!Make} functor is parametric over nt and tm. Note that these
   types should be comparable via Pervasives.compare in order for the
   datastructure implementations (Set, Map etc) to work. *)
module type BASE_TYPES = sig
  type nt 
  type tm 
  type sym = Nt of nt | Tm of tm
  type nt_item = { nt:nt; i_:int; k_:int; bs:sym list }
(* FIXME uncomment the following to get meaningful output  
  type cuts
  val empty_cuts : cuts
  val record_cuts: (nt_item*int) list -> cuts -> cuts *)
end

module Make(Base_types:BASE_TYPES) = struct
  open Base_types          
      
  module Internal = struct


    (** Used to instantiate {!module: Earley_base.Make} *)
    module Internal_A = struct

      type i_t = int
      type k_t = int
      type j_t = int


      (* NOTE we need to be able to distinguish nonterminals from terminals *)
(*
    type nt = int (* even, say *)
    type tm = int (* odd, say *)
    type sym = int
    let even x = (x mod 2 = 0)
    let sym_case ~nt ~tm sym = 
      if even sym then nt sym else tm sym
*)

      type tm = Base_types.tm
      type nt = Base_types.nt

      type sym = Base_types.sym
      let sym_case ~nt ~tm = function
        | Nt x -> nt x
        | Tm y -> tm y

      let _NT: nt -> sym = fun x -> Nt x


      type nt_item = Base_types.nt_item

      (* Implement the accessor functions by using simple arithmetic *)
      let dot_nt nitm = nitm.nt
      let dot_i nitm = nitm.i_
      let dot_k nitm = nitm.k_
      let dot_bs nitm = nitm.bs

      let dot_bs_hd nitm = nitm |> dot_bs |> function
        | [] -> None
        | x::xs -> Some x

      let cut : nt_item -> j_t -> nt_item = 
        fun bitm j0 -> 
        assert (bitm.bs <> []);
        { bitm with k_=j0; bs=(List.tl bitm.bs)}


      (* FIXME or include in base_types *)
      type cuts = (nt_item*int) list list
      let empty_cuts = []
      let record_cuts: (nt_item*int) list -> cuts -> cuts = fun cs cuts -> cs::cuts

      (* The rest of the code is straightforward *)

      module Set_nt_item = Set.Make(struct 
          type t = nt_item 
          let compare : t -> t -> int = Pervasives.compare end)

      type ixk = i_t*nt
      module Set_ixk = Set.Make(
        struct type t = ixk let compare : t -> t -> int = Pervasives.compare end)

      module Map_nt = Map.Make(
        struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
      type 'a map_nt = 'a Map_nt.t

      module Map_int = Map.Make(
        struct type t = int let compare : t -> t -> int = Pervasives.compare end)
      type 'a map_int = 'a Map_int.t

      module Map_tm = Map.Make(
        struct type t = tm let compare : t -> t -> int = Pervasives.compare end)

      type nt_item_set = Set_nt_item.t
      let elements = Set_nt_item.elements
      let empty_nt_item_set = Set_nt_item.empty

      type bitms_lt_k = (nt_item_set map_nt) map_int

      type todo_gt_k = nt_item_set map_int

      type bitms_at_k = nt_item_set map_nt

      type ixk_done = Set_ixk.t

      type ktjs = int list Map_tm.t


      let todo_gt_k_find i t = 
        Map_int.find_opt i t |> function
        | None -> Set_nt_item.empty
        | Some set -> set


      let update_bitms_lt_k i at_k lt_k =
        lt_k |> Map_int.add i at_k


      let empty_todo_gt_k = Map_int.empty
      let empty_bitms_lt_k = Map_int.empty
      let empty_bitms_at_k = Map_nt.empty
      let empty_ixk_done = Set_ixk.empty
      let empty_ktjs = Map_tm.empty

    end

    open Internal_A

    module Earley_impl = Earley_base.Make(Internal_A)

    open Earley_impl

    let pop_todo () s = match s.todo with
      | [] -> None,s
      | x::todo -> Some x,{s with todo}

    let _or_empty = function
      | None -> Set_nt_item.empty
      | Some x -> x

    let get_bitms_at_k nt s = 
      s.bitms_at_k |> Map_nt.find_opt nt |> _or_empty |> elements
      |> fun x -> x,s

    let get_bitms_lt_k (i,nt) s =
      s.bitms_lt_k |> Map_int.find_opt i |> (function
          | None -> Set_nt_item.empty
          | Some bitms -> Map_nt.find_opt nt bitms |> _or_empty)
      |> fun x -> elements x,s

    let add_bitm_at_k itm nt s =
      s.bitms_at_k |> Map_nt.find_opt nt |> _or_empty
      |> Set_nt_item.add itm |> fun itms ->
      s.bitms_at_k |> Map_nt.add nt itms |> fun bitms_at_k ->
      (),{s with bitms_at_k}

    let add_todos_at_k itms s =
      (* FIXME can we assume that they are all not in s.todo_done? *)
      (itms,s)
      |> iter_opt (function 
          | [],_ -> None
          | itm::itms,s ->
            Set_nt_item.mem itm s.todo_done |> function
            | true -> Some(itms,s)
            | false -> Some(
                itms, {s with todo=itm::s.todo;
                              todo_done=Set_nt_item.add itm s.todo_done}))
      |> fun ([],s) -> (),s

    let add_todos_gt_k itms s =
      (itms,s.todo_gt_k)
      |> iter_opt (function
          | [],_ -> None
          | itm::itms,todo_gt_k -> 
            Some(itms,
                 todo_gt_k |> Map_int.find_opt itm.k_ |> _or_empty 
                 |> Set_nt_item.add itm 
                 |> fun itms -> Map_int.add itm.k_ itms todo_gt_k))
      |> fun ([],todo_gt_k) -> 
      (),{s with todo_gt_k}

    let add_ixk_done (i,nt) s =
      (),{s with ixk_done=s.ixk_done |> Set_ixk.add (i,nt)}

    let mem_ixk_done (i,nt) s = 
      (Set_ixk.mem (i,nt) s.ixk_done),s

    let find_ktjs tm s = 
      (s.ktjs |> Map_tm.find_opt tm),s

    let add_ktjs tm js s =
      s.ktjs |> Map_tm.add tm js |> fun ktjs ->
      (),{s with ktjs}
         
    let run_earley_parser ~grammar_etc ~record_cuts = 
      (* let record_cuts xs s = (),s  (\* FIXME *\) *)
      let record_cuts xs s = 
        (),{s with cuts=record_cuts xs s.cuts}
      in
      let at_ops = { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
                     add_todos_at_k; add_todos_gt_k; add_ixk_done;
                     mem_ixk_done; find_ktjs; add_ktjs; record_cuts }
      in
      let earley_parser = make_earley_parser ~at_ops in
      run_earley_parser ~earley_parser ~grammar_etc

    (* open Earley_base *)

    module Export : sig 
      open Base_types
      type cuts = (nt_item*int) list list
      val run_earley_parser: 
        grammar_etc:(nt,tm,nt_item,'a) grammar_etc -> 
        record_cuts:((nt_item*int)list -> cuts -> cuts) ->
        initial_nt:nt -> 
        cuts
    end = struct
      type cuts = (nt_item*int) list list
      let run_earley_parser ~grammar_etc ~record_cuts ~initial_nt:nt = 
        let initial_state = { empty_state with todo=[{nt;i_=0;k_=0;bs=[Nt nt]}] } in
        run_earley_parser ~grammar_etc ~record_cuts ~initial_state |> fun s -> s.cuts
    end

  end

  include Internal.Export
end


