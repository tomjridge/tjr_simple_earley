(** Apply actions to the result of an Earley parse *)


module Internal(T:sig
    type nt
    type tm
    type sym
    type uni_val
end) = struct
  
  open T

  (** Apply actions for a given symbol, between two indices. *)
  let apply_actions 
      (* (type nt tm sym uni_val) *)
      ~is_nt ~dest_nt ~dest_tm ~get_rhss
      ~cut ~apply_tm 
      (* ~nt_to_string *)
    =
    let rec for_sym ~(sym:sym) ~i ~j ctxt =
      match is_nt sym with
      | true -> for_nt ~nt:(dest_nt sym) ~i ~j ctxt
      | false -> for_tm ~tm:(dest_tm sym) ~i ~j ctxt
    and for_tm ~(tm:tm) ~i ~j ctxt : uni_val option = 
      let r = apply_tm ~tm ~i ~j in
      (* assert (r<>None);  *)
      (* NOTE r may be None in the case that we have a rule like X ->
         "x" and we try to match X to i,i+1, but where input.(i) is
         not x; this happens because we are searching for the first
         rhs that matches; if we knew the rhs directly, we could
         assert that r was never None. FIXME is it worth tracking
         rules by ids during Earley parsing? This would slightly
         complicate the Earley code, but make this code quicker and
         possibly simpler *)
      r
    and for_nt ~(nt:nt) ~i ~j ctxt : uni_val option =
      (* (ctxt |> List.map (fun (i,_,j) -> (string_of_int i,string_of_int j) |> fun (x,y) -> x^","^y) |> String.concat ";" |> print_endline); *)
      match List.mem (i,nt,j) ctxt with 
      | true -> None
      | false -> begin
          let ctxt = (i,nt,j)::(ctxt |> List.filter (fun (i',_,j') -> (i,j)=(i',j'))) in
          (* get a list (sequence?) of rhs for the nt *)
          get_rhss ~nt |> fun rhss -> 
          (* now find the first one that parses i,j exactly *)
          rhss |> Misc.iter_till_some (fun (rhs,act) ->
              for_syms ~syms:rhs ~i ~j ctxt |> function
              | None -> None
              | Some vs -> Some (act(vs)))
        end
    and for_syms ~syms ~i ~j ctxt : uni_val list option = 
      match syms with
      | [] -> None
      (* NOTE by special casing the [sym] case, we can ensure cut is
         never called with empty syms *)
      | [sym] -> (
          for_sym ~sym ~i ~j ctxt |> function
          | None -> None
          | Some v -> Some[v])
      | sym::syms -> 
        (* NOTE cut (E,syms) should not return k=j if there is already
           a parse for i,E,j "in progress" *)
        cut (i:int) (sym,syms) (j:int) |> fun ks -> 
        ks |> Misc.iter_till_some (fun k -> 
            for_sym ~sym ~i ~j:k ctxt |> function
            | None -> None
            | Some (v:uni_val) ->
              (* FIXME have to take care if syms is [] *)
              assert(syms<>[]);
              for_syms ~syms ~i:k ~j ctxt |> function
              | None -> None
              | Some vs -> Some (v::vs))
    in
    (for_nt : nt:nt -> i:'a -> j:'a -> (int*nt*int) list -> uni_val option)

  let _ = apply_actions


  let _ :
is_nt:(sym -> bool) ->
dest_nt:(sym -> nt) ->
dest_tm:(sym -> tm) ->
get_rhss:(nt:nt -> (sym list * (uni_val list -> uni_val)) list) ->
cut:(int -> sym * sym list -> int -> int list) ->
apply_tm:(tm:tm -> i:int -> j:int -> uni_val option) ->
(* nt_to_string:'a -> *)
nt:nt -> i:int -> j:int -> (int * nt * int) list -> uni_val option
    = apply_actions

  (** 

So we need:
  - the grammar and actions, in the form of get_rhss
  - the parse results, in the form of cut and apply_tm
    - apply_tm can just return a string, and we can then retrict
      actions to nts only (although this seems like a proper
      restriction for not much extra work)
  - the initial nonterminal S
  - the span i,j  (where S should match i,j exactly, and i is
    presumably 0) 
    - for this we need to be able to call something like max_extent
      (0,S) to get j

*)
  

end

(** Apply actions, given various pieces of information about the parse
   (see {!Internal} for further comments). *)
let apply_actions (type nt tm sym uni_val) ~is_nt = 
  let module T = struct 
    type nonrec nt = nt
    type nonrec tm = tm
    type nonrec sym = sym 
    type nonrec uni_val = uni_val 
  end
  in
  let module I = Internal(T) in
  I.apply_actions ~is_nt

let _ = apply_actions
