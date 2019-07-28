
(** Use {!Earley_spec} to produce an efficient O(n^3) parser. *)

(* open Prelude *)
open Misc


(** Construct the parse function. *)
module Make(Nt_tm:Earley_intf.NT_TM) = struct

  module Internal = struct

    
    module Simple_items = Earley_intf.Simple_items(Nt_tm)
    open Simple_items

    module Extended_items = struct 
      type sym_item = { i_:int; sym:sym; j_:int }
      type sym_at_k = { sym:sym; k_:int } 
                      
      type item = 
        | Nt_item of nt_item
        | Sym_item of sym_item
        | Sym_at_k of sym_at_k
    end
    open Extended_items

    module State_type = struct 
      (* todo_done is really a set; we add items to todo providing they
         are not already in todo_done *)
      type state = {
        count:int;
        mutable todo:item list;
        todo_done:(item,unit) Hashtbl.t;
        blocked:((int*sym),(nt_item,unit)Hashtbl.t) Hashtbl.t;
        (* complete:((int*sym),(int,unit)Hashtbl.t) Hashtbl.t; *)
        complete:((int*sym),Int_set.t) Hashtbl.t;  
        complete2: ((int * sym * sym list), Int_set.t) Hashtbl.t
      }
      (* prefer to use an Int_set for complete so that we interact
         nicely with actions; FIXME now using complete2, so complete
         can revert to using hashtbl *)


      let empty_state = { 
        count=0;
        todo=[]; 
        todo_done=Hashtbl.create 100;
        blocked=Hashtbl.create 100;
        complete=Hashtbl.create 100;
        complete2=Hashtbl.create 100
      }
    end
    open State_type


    module Assembly = struct 
      include Nt_tm
      include Simple_items 
      include Extended_items 
      include State_type 
    end

    module Earley_spec' = Earley_spec.Internal(Assembly) 

    let earley = Earley_spec'.earley

    let earley ~expand_nt ~expand_tm = 
      let incr_count () s = (),{s with count=s.count+1 } in
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
            | Some set -> 
              Int_set.elements set)
        |> fun x -> x,s
      in
      let mark = fun _s -> () in (* FIXME *)
      (* let mark x = () in *)
      let _add_item (itm:item) s =
        mark "xa";
        let tbl = s.todo_done in
        mark "xb";
        match Hashtbl.mem tbl itm with
        | true -> mark "xc";()
        | false ->
          mark "xf";
          let _ = Hashtbl.add tbl itm () in
          mark "xg";
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
              let set = 
                Hashtbl.find_opt s.complete (i_,_S) |> function
                | None -> Int_set.empty
                | Some set -> set
              in
              Hashtbl.replace s.complete (i_,_S) (Int_set.add j_ set);
              ()
            | _ -> ()
          in
          mark "xw";
          let _ = s.todo<-itm::s.todo in
          mark "xy";
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
      let note_blocked_cuts (itm:nt_item) js s = 
        match itm with 
        | {nt;i_;k_;bs=_S::bs} -> (
            let set = 
              Hashtbl.find_opt s.complete2 (k_,_S,bs) |> function
              | None -> Int_set.empty
              | Some s -> s
            in
            let set' = Int_set.union (Int_set.of_list js) set in
            Hashtbl.replace s.complete2 (k_,_S,bs) set';
            (),s)
        | _ -> failwith "impossible"
      in
      let note_complete_cuts itms j s = 
        itms |> List.iter (fun {nt;i_;k_;bs=_S::bs} -> 
            let set =
              Hashtbl.find_opt s.complete2 (k_,_S,bs) |> function
              | None -> Int_set.empty
              | Some s -> s
            in
            let set' = Int_set.add j set in
            Hashtbl.replace s.complete2 (k_,_S,bs) set');
        (),s
      in          
      fun ~initial_nt:nt ->
        { empty_state with todo=[Nt_item{nt;i_=0;k_=0;bs=[Nt nt]}] }
        |> earley
          ~expand_nt ~expand_tm ~incr_count ~get_blocked_items ~get_complete_items
          ~add_item ~add_items ~pop_todo
          ~note_blocked_cuts ~note_complete_cuts
        |> fun ((),s) -> 
        let items = lazy (
            s.todo_done 
            |> Hashtbl.to_seq_keys
            |> List.of_seq) 
        in
        (* let get_complete_items_as_set s (k,_S) = Hashtbl.find_opt s.complete (k,_S) in *)
        (* let complete_items = get_complete_items_as_set s in *)
        let complete_items (i,_S,bs) = 
          Hashtbl.find_opt s.complete2 (i,_S,bs) |> function
          | None -> Int_set.empty
          | Some set -> set
        in
        Earley_intf.{ count=s.count;items;complete_items;debug=s.complete2 }
        (* s.todo_done |> Hashtbl.to_seq_keys |> List.of_seq *)

  end (* Internal *)

  open Nt_tm
  let earley_unstaged : 
expand_nt:(nt * int -> 'nt_item list) ->
expand_tm:(tm * int -> int list) -> initial_nt:nt -> ('b,'c,'d)Earley_intf.parse_result
    = Internal.earley

end
