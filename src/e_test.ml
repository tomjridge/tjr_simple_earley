open E_common

open E_bc (* spec *)
open E_cd
open E_cn
open E_fg (* impl *)

open E_examples


let bc_rs c nt = (spec c nt) |> Nt_item_set.elements


let cd_rs c nt =
  cd_earley c nt
  |> E_cd.Spec_t.elements
  |> List.map (function | NTITM  x -> [x] | _ -> [])
  |> List.concat


let cn_rs c nt = 
  cn_earley c nt
  |> (fun x -> x.todo_done)
  |> Nt_item_set.elements


let fg_rs c nt = 
  fg_earley c nt
  |> (fun x -> x.all_done) 
  |> (List.fold_left (fun acc itms -> Nt_item_set.union acc itms) Nt_item_set.empty)
  |> Nt_item_set.elements


let test c nt = (
  let bc_rs = bc_rs c nt in
  assert (bc_rs = cd_rs c nt);
  assert (bc_rs = cn_rs c nt);
  assert (bc_rs = fg_rs c nt))
  

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

