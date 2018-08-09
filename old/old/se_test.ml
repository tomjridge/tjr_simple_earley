(* testing ---------------------------------------- *)

open Se_common

open Se_spec
open Se_spec_all_items
open Se_simple
open Se_staged

open Se_examples


let spec_rs c nt = (se_spec c nt) |> Nt_item_set.elements


let sai_rs c nt =
  se_spec_all_items c nt
  |> Spec_t.elements
  |> List.map (function | NTITM  x -> [x] | _ -> [])
  |> List.concat


let simple_rs c nt = 
  se_simple c nt
  |> (fun x -> x.todo_done)
  |> Nt_item_set.elements


let staged_rs c nt = 
  se_staged c nt
  |> (fun x -> x.all_done) 
  |> (List.fold_left (fun acc itms -> Nt_item_set.union acc itms) Nt_item_set.empty)
  |> Nt_item_set.elements


let test c nt = (
  let spec_rs = spec_rs c nt in
  assert (spec_rs = sai_rs c nt);
  assert (spec_rs = simple_rs c nt);
  assert (spec_rs = staged_rs c nt))
  

let _ = (
  let f str = (
    E_EEE.(test (c0 str) e');
    Aho_s.(test (c0 str) s');
    Aho_sml.(test (c0 str) s');
    S_xsx.(test (c0 str) s'))
  in
  [0;1;9;10;11;20]
  |> List.map (fun x -> String.make x 'x')
  |> List.map f)

