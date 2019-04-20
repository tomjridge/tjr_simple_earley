
(** Use {!Earley_spec} to produce an efficient O(n^3) parser. *)

open Earley_spec

(** Construct the parse function. *)
module Make(A:A) = struct

  module Internal = struct
    open A

    module State_type = struct 
      (* todo_done is really a set; we add items to todo providing they
         are not already in todo_done *)
      type state = {
        mutable todo:item list;
        todo_done:(item,unit) Hashtbl.t;
        blocked:((int*sym),(nt_item,unit)Hashtbl.t) Hashtbl.t;
        complete:((int*sym),(int,unit)Hashtbl.t) Hashtbl.t;
      }

      let empty_state = { 
        todo=[]; 
        todo_done=Hashtbl.create 100;
        blocked=Hashtbl.create 100;
        complete=Hashtbl.create 100
      }
    end
    open State_type

    module B = Earley_spec.Make(A)
    module C = B.Internal.Make_with_state_type(State_type)
        
    open C

    let earley ~expand_nt ~expand_tm = 
      let get_blocked_items (k,_S) s = 
        Hashtbl.find_opt s.blocked (k,_S) 
        |> (function
            | None -> []
            | Some tbl -> 
              Hashtbl.to_seq_keys tbl |> List.of_seq)
        |> fun x -> x,s
      in
      let get_complete_items (k,_S) s = 
        Hashtbl.find_opt s.complete (k,_S) 
        |> (function
            | None -> []
            | Some tbl -> 
              Hashtbl.to_seq_keys tbl |> List.of_seq)
        |> fun x -> x,s
      in
      let _add_item itm s =
        match Hashtbl.mem s.todo_done itm with
        | true -> ()
        | false -> 
          let _ = Hashtbl.add s.todo_done itm () in
          (* update blocked and complete *)
          let _ = 
            match itm with 
            | Nt_item {nt;i_;k_;bs=_S::bs} ->
              let tbl = 
                Hashtbl.find_opt s.blocked (k_,_S) |> function
                | None -> (
                    Hashtbl.create 100 |> fun tbl ->
                    Hashtbl.add s.blocked (k_,_S) tbl;
                    tbl)
                | Some tbl -> tbl
              in
              let _ = Hashtbl.add tbl {nt;i_;k_;bs=_S::bs} () in
              ()
            | Sym_item{i_;sym=_S;j_} -> 
              let tbl = 
                Hashtbl.find_opt s.complete (i_,_S) |> function
                | None -> (
                    Hashtbl.create 100 |> fun tbl ->
                    Hashtbl.add s.complete (i_,_S) tbl;
                    tbl)
                | Some tbl -> tbl
              in
              let _ = Hashtbl.add tbl j_ () in
              ()
            | _ -> ()
          in
          let _ = s.todo<-itm::s.todo in
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
        |> fun ((),s) -> s.todo_done |> Hashtbl.to_seq_keys |> List.of_seq

  end

  open A
  let earley_unstaged : 
expand_nt:(nt * int -> nt_item list) ->
expand_tm:(tm * int -> int list) -> initial_nt:nt -> item list
 = Internal.earley

end
