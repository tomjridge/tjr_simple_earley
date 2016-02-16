(** Earley, staged. *)

open E_common


type ixk_t = (i_t * nt)  (* i X k *)


module Ixk_set =
  Set.Make(
  struct
    type t = ixk_t
    let compare: t -> t -> int = Pervasives.compare
  end)


module Blocked_map =
  Map.Make(
  struct
    type t = (k_t * nt)
    let compare: t -> t -> int = Pervasives.compare
  end)


(* state at k *)
type state_t = {
  k: int;
  todo: nt_item list;
  todo_done: Nt_item_set.t;
  todo_gt_k: Nt_item_set.t Int_map.t;
  ixk_done: Ixk_set.t;  (* i X k *)
  ktjs: int list option Map_tm.t;  (* k T j *)
  bitms_lt_k: Nt_item_set.t Blocked_map.t;
  bitms_at_k: Nt_item_set.t Map_nt.t;  (* bitms blocked at k,X *)
  all_done: Nt_item_set.t;
}


let wrap find k m default = (
  try find k m with Not_found -> default)


let bitms: state_t -> (k_t * nt) -> Nt_item_set.t = (
  fun s0 (k,x) ->
    match (k=s0.k) with
    | true -> (wrap Map_nt.find x s0.bitms_at_k Nt_item_set.empty)
    | false -> (wrap Blocked_map.find (k,x) s0.bitms_lt_k Nt_item_set.empty))


let pop_todo s0 = (
  match s0.todo with
  | x::xs -> (x,{s0 with todo=xs})
  | _ -> (failwith "pop_todo"))


let cut: nt_item -> j_t -> nt_item = (
  fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm
    )
)


(* k is the current stage *)
let add_todo: nt_item -> state_t -> state_t = 
  fun nitm s0 ->
    let k = s0.k in
    match nitm.k > k with
    | true -> (
        let nitms =
          wrap Int_map.find nitm.k s0.todo_gt_k Nt_item_set.empty
        in
        let nitms = Nt_item_set.add nitm nitms in
        { s0 with
          todo_gt_k=(Int_map.add nitm.k nitms s0.todo_gt_k)})
    | false -> (
        match Nt_item_set.mem nitm s0.todo_done with
        | true -> s0
        | false -> (
            { s0 with
              todo=(nitm::s0.todo);
              todo_done=Nt_item_set.add nitm s0.todo_done}))
      

let add_ixk_done: ixk_t -> state_t -> state_t =
  fun ix s0 ->
    { s0 with ixk_done=(Ixk_set.add ix s0.ixk_done)}


let mem_ixk_done: ixk_t -> state_t -> bool =
  fun ix s0 -> Ixk_set.mem ix s0.ixk_done 


(* nt_item blocked on nt at k *)
let add_bitm_at_k: nt_item -> nt -> state_t -> state_t =
  fun nitm nt s0 ->
    { s0 with
      bitms_at_k = (
        let m = s0.bitms_at_k in
        let s = wrap Map_nt.find nt m Nt_item_set.empty in
        let s' = Nt_item_set.add nitm s in
        let m' = Map_nt.add nt s' m in
        m' ) }


let find_ktjs: tm -> state_t -> int list option =
  fun t s0 ->
    wrap Map_tm.find t s0.ktjs None


let debug_endline = (fun x -> ()) (* print_endline *)


let step_k: ctxt_t -> state_t -> state_t = (
  fun c0 s0 ->
let _ = debug_endline "XXXstep_k" in
let k = s0.k in    
let bitms = bitms s0 in
let (nitm,s0) = pop_todo s0 in
let complete = nitm.bs = [] in
match complete with
| true -> (
    let (i,x) = (nitm.i,nitm.nt) in
    (* possible NEW COMPLETE (i,X,k) *)
    let already_done = mem_ixk_done (i,x) s0 in
    match already_done with
    | true -> s0
    | false -> (
        let _ = debug_endline "processing complete" in
        let s0 = add_ixk_done (i,x) s0 in
        let fm bitm s1 = (add_todo (cut bitm k) s1) in
        let bitms = bitms (i,x) in
        Nt_item_set.fold fm bitms s0))
| false -> (
    (* NEW BLOCKED X -> i as k (S bs') on k S*)
    let bitm = nitm in
    let s = List.hd bitm.bs in
    match s with
    | NT y -> (
        (* have we already processed k Y? *)
        let bitms = bitms (k,y) in
        let bitms_empty = Nt_item_set.is_empty bitms in
        (* record blocked FIXME we may already have processed k Y *)
        let s0 = add_bitm_at_k bitm y s0 in
        match bitms_empty with
        | false ->
          (* already processed k Y *)
          s0
        | true -> (
            (* need to process Y *)
            (* do we have an item (k,Y,k) ? *)
            let complete = Ixk_set.mem (k,y) s0.ixk_done in
            let s0 = (
              match complete with
              | true -> (add_todo (cut bitm k) s0)
              | false -> s0)
            in
            (* expand y *)
            let new_itms = c0.g0.nt_items_for_nt y (c0.i0.str,k) in
            let fn s1 nitm = add_todo nitm s1 in
            List.fold_left fn s0 new_itms) )
    | TM t -> (
        (* have we already processed k T ? *)
        let ktjs = find_ktjs t s0 in
        let (js,s0) = (
          match ktjs with
          | None -> (
              (* process k T *)
              let _ = debug_endline "processing k T" in
              let p = c0.g0.p_of_tm t in
              let js = p (c0.i0.str,k,c0.i0.len) in
              let s0 = { s0 with ktjs=(Map_tm.add t (Some js) s0.ktjs) } in
              (js,s0))
          | Some js -> (js,s0))
        in
        (* process blocked *)
        let fo s1 j = add_todo (cut bitm j) s1 in
        List.fold_left fo s0 js)
  )            
)


(* loop at k *)
let loop_k: ctxt_t -> state_t -> state_t = 
  fun ctxt -> (
      let step_k = step_k ctxt in
      let rec f s0 = (
        match s0.todo with
        | [] -> s0
        | _ -> (f (step_k s0)))
      in
      f)


(* outer loop: repeatedly process items at stage k, then move to stage
   k+1 *)
let loop: ctxt_t -> state_t -> state_t = (
  fun c0 -> (
      let loop_k = loop_k c0 in
      let rec f s0 = (
        match s0.k > c0.i0.len + 1 with
        | true -> s0
        | false -> (
            (* process items *)
            let s0 = loop_k s0 in
            let old_k = s0.k in
            let k = s0.k+1 in
            let todo =
              wrap Int_map.find k s0.todo_gt_k Nt_item_set.empty
            in
            let todo_done = todo in
            let todo = Nt_item_set.elements todo in
            let todo_gt_k = Int_map.remove k s0.todo_gt_k in
            let ixk_done = Ixk_set.empty in
            let ktjs = Map_tm.empty in
            let bitms_lt_k = (
              let b_init = s0.bitms_lt_k in
              let b2 = s0.bitms_at_k in
              let f
                  (key:nt)
                  (v:Nt_item_set.t)
                  (a:Nt_item_set.t Blocked_map.t)
                = (
                  Blocked_map.add (old_k,key) v a)
              in
              Map_nt.fold f b2 b_init)
            in
            let bitms_at_k = Map_nt.empty in
            let all_done = Nt_item_set.union s0.all_done s0.todo_done in
            let s1 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done}
            in
            f s1
          )
      )
      in
      f
    )
)

(* construct initial context, apply loop *)
let earley c0 nt = (
  let todo = (c0.g0.nt_items_for_nt nt (c0.i0.str,0)) in
  let k = 0 in
  let todo_done = Nt_item_set.of_list todo in
  let todo_gt_k = Int_map.empty in
  let ixk_done = Ixk_set.empty in
  let ktjs = Map_tm.empty in
  let bitms_lt_k = Blocked_map.empty in
  let bitms_at_k = Map_nt.empty in
  let all_done = Nt_item_set.empty in
  let s0 = {k;todo;todo_done;todo_gt_k;ixk_done;ktjs;bitms_lt_k;bitms_at_k;all_done} in
  loop c0 s0
)

(* list of nt_items *)
let earley_to_list s0 = Nt_item_set.elements s0.all_done

let earley_as_list c0 nt = (
  earley c0 nt |> earley_to_list
)

let _ = str := (String.make 200 '1')  
let _ = earley_as_list (c0 ()) e'
let _ = print_endline "Finished"
    
(* Sample time for string length 200: 
real	0m1.212s
user	0m1.212s
sys	0m0.001s
*)
