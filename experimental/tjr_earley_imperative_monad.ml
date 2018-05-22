(* an experiment to see whether the imperative code (represented using
   a monad) is easier to read; probably it is *)

open Tjr_monad.Monad

module type S_ = sig  
  type i_t = int  
  type k_t = int
  type j_t = int

  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  type nt_item  

  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> i_t
  val dot_k: nt_item -> k_t
  val dot_bs: nt_item -> sym list

  val cut: nt_item -> j_t -> nt_item

  type nt_item_set
  val elements : nt_item_set -> nt_item list
end

module type M_ = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
end

module Make = functor (S:S_)(M:M_) -> struct
  (*:np:*)
  open S
  open M

  type atomic_operations = {
    get_bitms: int * nt -> nt_item list m;  (* or set? *)
    add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
    pop_todo: unit -> nt_item option m;
    add_todos: nt_item list -> unit m;
    add_ixk_done: int*nt -> unit m;
    mem_ixk_done: int*nt -> bool m;
    find_ktjs: tm -> int list option m;
    add_ktjs: tm -> int list -> unit m;
  }



  let run_earley ~at_ops ~new_items ~input ~parse_tm ~input_length ~init_nt = 

    let { get_bitms; add_bitm_at_k; pop_todo; add_todos; add_ixk_done; mem_ixk_done; find_ktjs; add_ktjs } = at_ops in

    (* step_k ------------------------------------------------------- *)

    let is_finished nitm = nitm|>dot_bs = [] in

    let step_at_k k nitm = 
      match is_finished nitm with
      | true -> (
          let (i,x) = (nitm|>dot_i,nitm|>dot_nt) in
          mem_ixk_done (i,x) >>= fun already_done ->
          match already_done with
          | true -> return ()
          | false -> (
              add_ixk_done (i,x) >>= fun _ ->
              get_bitms (i,x) >>= fun bitms ->
              add_todos (List.map (fun bitm -> cut bitm k) bitms)))
      | false -> (
          let bitm = nitm in
          let s = List.hd (bitm|>dot_bs) in
          s |> sym_case
            ~nt:(fun _Y -> 
                get_bitms (k,_Y) >>= fun bitms ->
                let bitms_empty = bitms=[] in
                add_bitm_at_k bitm _Y >>= fun _ ->
                match bitms_empty with
                | false -> (
                    mem_ixk_done (k,_Y) >>= function
                    | true -> add_todos [cut bitm k]
                    | false -> return ())
                | true -> (
                    let itms = new_items ~nt:_Y ~input ~k in
                    add_todos itms))
            ~tm:(fun tm ->
                find_ktjs tm >>= fun ktjs ->
                (match ktjs with 
                 | None -> 
                   (* we need to process kT *)
                   let js = parse_tm ~tm ~input ~k ~input_length in
                   add_ktjs tm js >>= fun _ ->
                   return js
                 | Some js -> return js) >>= fun js ->
                add_todos (List.map (fun j -> cut bitm j) js)))
    in

    (* loop_k: loop at k -------------------------------------------- *)

    (* FIXME monad syntax may make this easier to read *)
    let rec loop_at_k k = 
      pop_todo () >>= function
      | None -> return ()
      | Some itm -> step_at_k k itm >>= fun _ -> loop_at_k k
    in

    loop_at_k

  (* FIXME rest of code can be ported to monad also *)


end
