(** Simple specification of parsing. *)

module type A = sig

  type nt
  type tm

  type sym = Nt of nt | Tm of tm

  type nt_item = { nt:nt; i_:int; k_:int; bs: sym list }

  type sym_item = { i_:int; sym:sym; j_:int }               (* complete item *)

  type sym_at_k = { sym:sym; k_:int } 

  type item = 
    | Nt_item of nt_item
    | Sym_item of sym_item
    | Sym_at_k of sym_at_k
end


module Make(A:A) = struct

  include A


  module Internal = struct

    (* todo_done is really a set; we add items to todo providing they
       are not already in todo_done *)
    type state = {
      mutable todo:item list;
      todo_done:(item,unit) Hashtbl.t
    }

    type 'a m = state -> 'a * state
    let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
      fun s ->
      a s |> fun (a,s) -> 
      ab a s
    let _ = ( >>= )
    let return a = fun s -> (a,s)

    let empty_state = { todo=[]; todo_done=Hashtbl.create 100 }


    (** Main Earley routine, parameterized *)
    let _earley 
        ~expand_nt ~expand_tm ~get_blocked_items ~get_complete_items
        ~add_item ~add_items ~pop_todo
      = 

      (* process a blocked item *)
      let cut_blocked_item nt_item =
        assert(nt_item.bs<>[]);
        match nt_item.bs with
        | [] -> failwith "impossible"
        | _S::bs -> (
            let k = nt_item.k_ in
            get_complete_items (k,_S) >>= fun js ->
            js |> List.map (fun j -> Nt_item { nt_item with k_=j; bs=bs })
            |> add_items)
      in

      (* process a complete item *)
      let cut_complete_item {i_;sym;j_} = 
        get_blocked_items (i_,sym) >>= fun itms ->
        itms |> List.map (fun itm -> Nt_item {itm with k_=j_; bs=List.tl itm.bs})
        |> add_items
      in

      (* process an item *)
      let step itm =
        match itm with 
        | Nt_item itm -> (
            match itm.bs with
            | [] -> 
              (* item is complete *)
              add_item (Sym_item {sym=Nt itm.nt; i_=itm.i_; j_=itm.k_})
            | _S::bs -> (
                (* we need to record that we need to expand _S *)
                add_item (Sym_at_k { sym=_S; k_=itm.k_ }) >>= fun _ ->
                (* and we need to process the item against complete items *)
                cut_blocked_item itm))
        | Sym_item itm -> (cut_complete_item itm)
        | Sym_at_k {sym;k_} -> (
            match sym with
            | Nt nt -> expand_nt (nt,k_)
            | Tm tm -> expand_tm (tm,k_))        
      in

      (* loop until no items left to process *)
      let rec loop () = 
        pop_todo () >>= function
        | None -> return ()
        | Some itm -> step itm >>= fun _ -> loop ()
      in
      loop ()

    let _ = _earley
  end

  open Internal

  let earley ~expand_nt ~expand_tm = 
    let get_blocked_items (k,_S) s = 
      let blocked = ref [] in
      s.todo_done |> Hashtbl.iter (fun itm _ ->
          match itm with 
          | Nt_item {nt;i_;k_;bs} when k_=k && bs<>[] && List.hd bs=_S -> (blocked:={nt;i_;k_;bs}::!blocked)
          | _ -> ());
      !blocked,s
    in
    let get_complete_items (k,_S) s = 
      let complete = ref [] in
      s.todo_done |> Hashtbl.iter (fun itm _ ->
          match itm with 
          | Sym_item {i_;sym;j_} when (i_=k && sym=_S) -> (
              complete:=j_::!complete)
          | _ -> ());
      !complete,s
    in
    let _add_item itm s =
      match Hashtbl.mem s.todo_done itm with
      | true -> ()
      | false -> 
        Hashtbl.add s.todo_done itm ();
        s.todo<-itm::s.todo;        
        ()
    in
    let add_items itms s = 
      itms |> List.iter (fun itm -> _add_item itm s);
      (),s
    in
    let add_item itm s = 
      _add_item itm s;
      (),s        
    in
    let expand_nt (nt,i) = 
      add_items (expand_nt (nt,i) |> List.map (fun itm -> Nt_item itm))
    in
    let expand_tm (tm,i_) =
      expand_tm (tm,i_) 
      |> List.map (fun j_ -> Sym_item{i_;sym=Tm tm;j_})
      |> add_items
    in
    let pop_todo () s = match s.todo with
      | [] -> None,s
      | x::todo -> Some x,{s with todo}
    in
    fun ~initial_nt:nt ->
      { Internal.empty_state with todo=[Nt_item{nt;i_=0;k_=0;bs=[Nt nt]}] }
      |> Internal._earley 
        ~expand_nt ~expand_tm ~get_blocked_items ~get_complete_items
        ~add_item ~add_items ~pop_todo
      |> fun ((),s) -> s.todo_done |> Hashtbl.to_seq_keys |> List.of_seq

  (* FIXME expand_nt and expand_tm should be outside the monad *)
          
  let _ :
expand_nt:(nt * int -> nt_item list) ->
expand_tm:(tm * int -> int list) -> initial_nt:nt -> item list
= earley

end
