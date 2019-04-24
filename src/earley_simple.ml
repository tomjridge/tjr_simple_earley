(** A simple implementation of Earley parsing datastructures, based on {!Earley_base} *)

open Misc
open Prelude


(** Construct the Earley parsing function *)
module Make(Nt_tm:NT_TM) = struct
  open Nt_tm
      
  open Spec_types

  module Internal = struct

    module Derived_types = struct
      type sym = (nt,tm) Spec_types.sym
      type nt_item = (nt,sym list) Spec_types.nt_item
    end

    open Derived_types

    (** Used to instantiate {!module: Earley_base.Make} *)
    module Base_requires = struct

      type i_t = int
      type k_t = int
      type j_t = int

      type tm = Nt_tm.tm
      type nt = Nt_tm.nt

      type nonrec sym = sym
      let sym_case ~nt ~tm = function
        | Nt x -> nt x
        | Tm y -> tm y

      let _NT: nt -> sym = fun x -> Nt x

      type nonrec nt_item = nt_item

      (* Implement the accessor functions by using simple arithmetic *)
      let dot_nt nitm = nitm.nt
      let dot_i (nitm:nt_item) = nitm.i_
      let dot_k (nitm:nt_item) = nitm.k_
      let dot_bs nitm = nitm.bs

      let dot_bs_hd nitm = nitm |> dot_bs |> function
        | [] -> None
        | x::xs -> Some x

      let cut : nt_item -> j_t -> nt_item = 
        fun bitm j0 -> 
        assert (bitm.bs <> []);
        { bitm with k_=j0; bs=(List.tl bitm.bs)}


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
      let nt_item_set_of_list = Set_nt_item.of_list

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

    end  (* Base_requires *)

    open Base_requires

    module Base_impl = Earley_base.Make(Base_requires)

    open Base_impl

    let pop_todo () s = match s.todo with
      | [] -> None,s
      | x::todo -> Some x,{s with todo; count=s.count+1}

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

    (* FIXME this is the one to optimize! *)
    let add_todos_at_k itms s =
      (itms,s.todo_done,s.todo)
      |> iter_opt (function 
          | [],_,_ -> None
          | itm::itms,todo_done,todo ->
            Set_nt_item.mem itm todo_done |> function
            | true -> Some(itms,todo_done,todo)
            | false -> Some(itms, Set_nt_item.add itm todo_done,itm::todo))
      |> fun ([],todo_done,todo) -> 
      (),{s with todo; todo_done }

    let add_todos_gt_k (itms:nt_item list) s =
      let dot_k (x:nt_item) = x.k_ in  (* some problems with usign .k_ and ocaml not distinguishing based on types... *)
      (itms,s.todo_gt_k)
      |> iter_opt (function
          | [],_ -> None
          | itm::itms,todo_gt_k -> 
            Some(itms,
                 todo_gt_k |> Map_int.find_opt (dot_k itm) |> _or_empty 
                 |> Set_nt_item.add itm
                 |> fun itms -> Map_int.add (dot_k itm) itms todo_gt_k))
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
         
    let run_earley_parser ~grammar_etc = 
      let at_ops = { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
                     add_todos_at_k; add_todos_gt_k; add_ixk_done;
                     mem_ixk_done; find_ktjs; add_ktjs }
      in
      let earley_parser = make_earley_parser ~at_ops in
      run_earley_parser ~earley_parser ~grammar_etc

    module Export : sig 
      open Nt_tm
      val run_earley_parser: 
        grammar_etc:(nt,tm,nt_item,'a) grammar_etc -> 
        initial_nt:nt -> 
        state
    end = struct
      let run_earley_parser ~grammar_etc ~initial_nt:nt = 
        let initial_state = { empty_state with todo=[{nt;i_=0;k_=0;bs=[Nt nt]}] } in
        run_earley_parser ~grammar_etc ~initial_state 
    end

  end  (* Internal *)
  
  (** NOTE this exposes the internal types for nt_item, but this is
     just an alias for a type known outside Internal, so we can export
     it. This also exposes the state type FIXME? *)
  let run_earley_parser = Internal.Export.run_earley_parser
end  (* Make *)


