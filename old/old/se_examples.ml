(* various examples *)

open Se_common

let eps = TM 1

let parse_eps = (fun (s,i,j) -> if i<=j then [i] else [])

let x = TM 3

let parse_x = (fun (s,i,j) ->
    (* this terminal parser requires to know string_t *)
    let (s:string) = string_t_to_string s in  
    if i < j && i < String.length s && String.get s i = 'x' then 
      [i+1]
    else
      [])

let p_of_tm = (
  fun tm -> 
    if TM tm=eps then parse_eps
    else if TM tm=x then parse_x
    else failwith "p_of_tm")

let i0 str = (
  let len = String.length str in
  let str : string_t = string_to_string_t str in
  { str; len })



(* E -> E E E | "x" | eps *)
module E_EEE = struct

  let e' = 2
  let e = NT e'

  let nt_items_for_nt=(fun nt (s,i) ->
      let _ = assert(nt=e') in
      let as_ = [] in
      let k = i in
      [{nt;i;as_;k;bs=[e;e;e]};
       {nt;i;as_;k;bs=[x]};
       {nt;i;as_;k;bs=[eps]}])

  let g0 = {nt_items_for_nt; p_of_tm}

  let c0 str = {g0;i0=(i0 str)}

end


(* S -> "x" S S | eps *)
module Aho_s = struct

  let s' = 2
  let s = NT s'

  let nt_items_for_nt=(fun nt (str,i) ->
      let _ = assert(nt=s') in
      let as_ = [] in
      let k = i in
      [{nt;i;as_;k;bs=[x;s;s]};
       {nt;i;as_;k;bs=[eps]}])

  let g0 = {nt_items_for_nt; p_of_tm}

  let c0 str = {g0;i0=(i0 str)}

end


(* S -> S S "x" | eps *)
module Aho_sml = struct

  let s' = 2
  let s = NT s'
      
  let nt_items_for_nt=(fun nt (str,i) ->
      let _ = assert(nt=s') in
      let as_ = [] in
      let k = i in
      [{nt;i;as_;k;bs=[s;s;x]};
       {nt;i;as_;k;bs=[eps]}])

  let g0 = {nt_items_for_nt; p_of_tm}

  let c0 str = {g0;i0=(i0 str)}

end


(* S -> "x" S "x" | "x" *)
module S_xsx = struct

  let s' = 2
  let s = NT s'

  let nt_items_for_nt=(fun nt (str,i) ->
      let _ = assert(nt=s') in
      let as_ = [] in
      let k = i in
      [{nt;i;as_;k;bs=[x;s;x]};
       {nt;i;as_;k;bs=[x]}])

  let g0 = {nt_items_for_nt; p_of_tm}

  let c0 str = {g0;i0=(i0 str)}

end
