(** A simple specification of general parsing (not only Earley). *)
open Prelude


(** Required by the {!Make} functor *)
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

  (* For debugging *)
  (* val note_item: nt_item -> unit *)
end


(** Construct the parse function. Use
   {!Internal_example_parse_function} to avoid functors. *)
module Make(A:A) = struct

  include A


  module Internal = struct

    (** This is independent of the state type; we can reuse the code for
        an efficient unstaged version of Earley *)
    module Make_with_state_type(B:sig type state end) = struct

      open B

      type 'a m = state -> 'a * state
      let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
        fun s ->
        a s |> fun (a,s) -> 
        ab a s
      let _ = ( >>= )
      let return a = fun s -> (a,s)

      (** Main Earley routine, parameterized *)
      let _earley 
          ~expand_nt ~expand_tm ~get_blocked_items ~get_complete_items
          ~add_item ~add_items ~pop_todo
        = 

        let mark = !spec_mark_ref in
        let count = ref 0 in

        (* process a blocked item *)
        let cut_blocked_item = function
          | { nt; i_; k_; bs=_S::bs } as itm -> 
            mark "am";
            get_complete_items (k_,_S) >>= fun js ->
            mark "ap";
            (* js |> List.iter (fun j -> note_cut itm j); *)
            js |> List.map (fun j -> Nt_item { itm with k_=j; bs=bs })
            |> add_items >>= fun _ ->
            mark "as";
            return ()              
          | _ -> failwith "impossible"
        in

        (* process a complete item *)
        let cut_complete_item {i_;sym;j_} = 
          mark "bm";
          get_blocked_items (i_,sym) >>= fun itms ->
          mark "bp";
          (* itms |> List.iter (fun itm -> note_cut itm j_); *)
          itms |> List.map (fun itm -> Nt_item {itm with k_=j_; bs=List.tl itm.bs})
          |> add_items >>= fun _ ->
          mark "bs";
          return ()
        in

        (* process an item *)
        let step itm =
          count:=!count+1;
          mark "em";
          match itm with 
          | Nt_item itm -> (
              (* note_item itm;                                  (\* tracing *\) *)
              match itm.bs with
              | [] -> 
                (* item is complete *)
                add_item (Sym_item {sym=Nt itm.nt; i_=itm.i_; j_=itm.k_}) >>= fun _ ->
                mark "ep";
                return ()
              | _S::bs -> (
                  (* we need to record that we need to expand _S *)
                  mark "er";
                  add_item (Sym_at_k { sym=_S; k_=itm.k_ }) >>= fun _ ->
                  mark "eu";
                  (* and we need to process the item against complete items *)
                  cut_blocked_item itm >>= fun _ ->
                  mark "ev";
                  return ()))
          | Sym_item itm -> (
              cut_complete_item itm >>= fun _ ->
              mark "ga";
              return ())
          | Sym_at_k {sym;k_} -> (
              match sym with
              | Nt nt -> expand_nt (nt,k_) >>= fun _ -> mark "ha"; return ()
              | Tm tm -> expand_tm (tm,k_) >>= fun _ -> mark "hb"; return ())        
        in

        (* loop until no items left to process *)
        let rec loop () = 
          pop_todo () >>= function
          | None -> return !count
          | Some itm -> step itm >>= fun _ -> loop ()
        in
        loop ()

      let _ = _earley

      (** Adjust the types of expand_nt and expand_tm *)
      let earley ~expand_nt ~expand_tm ~add_items =
        let expand_nt (nt,i) = 
          add_items (expand_nt (nt,i) |> List.map (fun itm -> Nt_item itm))
        in
        let expand_tm (tm,i_) =
          expand_tm (tm,i_) 
          |> List.map (fun j_ -> Sym_item{i_;sym=Tm tm;j_})
          |> add_items
        in
        _earley ~expand_nt ~expand_tm ~add_items
    end

    module State_type = struct

      (* todo_done is really a set; we add items to todo providing they
         are not already in todo_done *)
      type state = {
        mutable todo:item list;
        todo_done:(item,unit) Hashtbl.t
      }

      let empty_state = { todo=[]; todo_done=Hashtbl.create 100 }
    end
    include State_type
    include Make_with_state_type(State_type)
  end


  open Internal

  (** The (executable) specification of parsing. Returns a list of
     items (FIXME?). Implementations such as Earley should return an
     identical set of items (for nt_items at least). NOTE that the
     parameters are independent of the input (in that the input is not
     present as an argument). *)
  let earley_spec ~expand_nt ~expand_tm = 
    let get_blocked_items (k,_S) s = 
      let blocked = ref [] in
      s.todo_done |> Hashtbl.iter (fun itm _ ->
          match itm with 
          | Nt_item {nt;i_;k_;bs} 
            when k_=k && bs<>[] && List.hd bs=_S -> (
              blocked:={nt;i_;k_;bs}::!blocked)
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
    let pop_todo () s = match s.todo with
      | [] -> None,s
      | x::todo -> Some x,{s with todo}
    in
    fun ~initial_nt:nt ->
      { empty_state with todo=[Nt_item{nt;i_=0;k_=0;bs=[Nt nt]}] }
      |> earley 
        ~expand_nt ~expand_tm ~get_blocked_items ~get_complete_items
        ~add_item ~add_items ~pop_todo
      |> fun (count,s) -> 
      (* let sym_to_sym' = function Nt nt -> `Nt nt | Tm tm -> `Tm tm in  *)
      let complete = lazy begin
        fun (i,_S) -> 
          get_complete_items (i,_S) s |> fun (n,_) -> n
      end
      in
      let items = lazy begin
        s.todo_done 
        |> Hashtbl.to_seq_keys
        |> List.of_seq
          end
      in
      let complete_items = fun (i,_S) -> (Lazy.force complete) (i,_S) in
      { count;
        items;
        complete_items }

  let _ :
expand_nt:(nt * int -> nt_item list) ->
expand_tm:(tm * int -> int list) -> initial_nt:nt -> ('b,'c) parse_result
= earley_spec


end

(** An example parse function which is polymorphic over symbols; no
   functors involved. *)
module Internal_example_parse_function = struct

(** An (executable) parsing specification polymorphic over
   nonterminals and terminals *)
let earley_spec (type nt tm) ~expand_nt ~expand_tm  =
  let module A = struct
    type nonrec nt = nt
    type nonrec tm = tm
    type sym = Nt of nt | Tm of tm
    type nt_item = { nt:nt; i_:int; k_:int; bs: sym list }
    type sym_item = { i_:int; sym:sym; j_:int }
    type sym_at_k = { sym:sym; k_:int } 
    type item = 
      | Nt_item of nt_item
      | Sym_item of sym_item
      | Sym_at_k of sym_at_k

  end
  in
  let module B = Make(A) in
  let open B in
  let sym'_to_sym = function `Nt nt -> Nt nt | `Tm tm -> Tm tm in
  let sym_to_sym' = function Nt nt -> `Nt nt | Tm tm -> `Tm tm in 
  let expand_nt (nt,i) = 
    expand_nt (nt,i) |> List.map (fun bs ->
        bs |> List.map sym'_to_sym |> fun bs ->
        {nt;i_=i;k_=i;bs})
  in
  fun ~initial_nt ->
    let res = B.earley_spec ~expand_nt ~expand_tm ~initial_nt in
    res |> fun {count;items;complete_items} -> 
    let items = 
      lazy begin
        Lazy.force items 
        |> List.map (function
            | Nt_item {nt;i_;k_;bs} -> 
              `Nt_item(nt,i_,k_,List.map sym_to_sym' bs)
            | Sym_item {i_;sym;j_} -> 
              `Sym_item(i_,sym_to_sym' sym,j_)
            | Sym_at_k {sym;k_} ->
              `Sym_at_k(sym_to_sym' sym,k_))
      end
    in
    let complete_items = fun (i,_S) -> complete_items (i,sym'_to_sym _S) in
    {count;items;complete_items}

end
