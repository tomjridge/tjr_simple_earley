module Log = struct 
    let log (x:unit Lazy.t) = Lazy.force x
end

module Misc = struct 
    let iter_opt (f:'a -> 'a option) = 
      let rec loop x = 
        match f x with
        | None -> x
        | Some x -> loop x
      in
      fun x -> loop x
    
    
    let rev_filter_map f xs =
      ([],xs) |> iter_opt (function
          | _,[] -> None
          | xs',x::xs -> 
            f x |> function
            | None -> Some(xs',xs)
            | Some y -> Some(y::xs',xs))
      |> fun (xs',[]) -> xs'
    
    let _ = rev_filter_map
    
    
    let string_matches_at ~string ~sub ~pos =
      let len = String.length sub in
      try
        String.sub string pos len = sub 
      with Invalid_argument _ -> false
    
    
    
    (* iterate over a list until the first Some x; return this (or None if no such elt *)
    let iter_till_some (f: 'a -> 'b option) xs =
      (None,xs) |> iter_opt (fun (ret,xs) ->
          match ret with 
          | Some x -> None
          | None -> (
              match xs with 
              | [] -> None
              | x::xs -> 
                f x |> function
                | None -> Some (None,xs)
                | Some ret -> Some(Some ret,[])))
      |> function (ret,_) -> ret
    
    let _ : ('a -> 'b option) -> 'a list -> 'b option = iter_till_some
    
    
    module Int_set = Set.Make(
      struct type t = int let compare: t -> t -> int = Pervasives.compare end)
end

module Prelude = struct 
    
    (** {2 Some profiling parameters, set later in bin/} *)
    
    (* let _ = Log.log @@ lazy (Printf.printf "%s: various mark_ref globals\n%!" __FILE__) *)
    
    let base_mark_ref : (string -> unit) ref = ref (fun s -> ())
    
    let spec_mark_ref : (string -> unit) ref = ref (fun s -> ())
    
    let unstaged_mark_ref : (string -> unit) ref = ref (fun s -> ())
    
    
    (** {2 Type for inputs} *)
    
    type 'a input = {
      input:'a;
      input_length: int
    }
    
    
    (** {2 Type for grammars} *)
    
    (** In the interface to Earley, the user has to know the structure of
       the sym type; so it is useful to have this known outside *)
    type ('nt,'tm) generic_sym = Nt of 'nt | Tm of 'tm
    
    (** A simple representation of a grammar. We might want to generalize this. *)
    type ('nt,'tm) simple_grammar = {
      nt_to_rhss: 'nt -> ('nt,'tm) generic_sym list list
    }
    
    type ('nt,'tm,'a) input_dependent_grammar = {
      nt_input_to_rhss: nt:'nt -> input:'a input -> pos:int -> ('nt,'tm)generic_sym list list
    }
    
    let simple_to_input_dependent_grammar { nt_to_rhss } = {
      nt_input_to_rhss=fun ~nt ~input ~pos ->
        nt_to_rhss nt |> fun syms_list -> syms_list
    }
    
    
    (** {2 Types for terminal parsers} *)
    
    type ('tm,'a) terminal_input_matcher = {
      parse_tm: tm:'tm -> input:'a input -> pos:int -> int list
    }
    
    (*
    (** Provided by user; NOTE that this exposes the [nt_item] type, but
       we could instead go for a list of sym. Then we might also just
       require a map from nt to rhs list *)
    type ('nt,'tm,'nt_item,'input) grammar_etc = {
      new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
      parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
      input:'input;
      input_length:int;
    }
    *)
    
    
    (** {2 Parsing result type} *)
    
    (** Result of parse; complete items is something like (i,sym) -> j set *)
    type ('b,'c,'d) parse_result = {
      count: int;
      items:'b;
      complete_items:'c;
      debug: 'd;
    }
    
    
    
    (** {2 Common required interface} *)
    
    (** We often parameterize over nt,tm *)
    module type NT_TM = sig  type nt type tm end
    
    (** What is often required by the [Make] functor *) 
    module type REQUIRED = sig
    
      type nt
      type tm
      type sym
    
      val is_nt: sym -> bool
      val dest_nt: sym -> nt
      val dest_tm: sym -> tm
      val _NT: nt -> sym
      val _TM: tm -> sym
    
      type sym_list
      val syms_nil: sym_list -> bool
      val syms_hd: sym_list -> sym
      val syms_tl: sym_list -> sym_list
    
      type nt_item  
      val dot_nt: nt_item -> nt
      val dot_i: nt_item -> int
      val dot_k: nt_item -> int
      val dot_bs: nt_item -> sym_list
    
      val cut: nt_item -> int -> nt_item
    
      (* type 'input grammar_etc' = (nt,tm,nt_item,'input) grammar_etc *)
    end  (* REQUIRED *)
    
    (** Simple instantiation of basic types *)
    module Simple_items(A:sig type nt type tm end) = struct
      open A
    
      type sym = (nt,tm)generic_sym
      let is_nt = function Nt x -> true | Tm x -> false
      let dest_nt = function Nt x -> x | _ -> failwith "dest_nt"
      let dest_tm = function Tm x -> x | _ -> failwith "dest_tm"
      let _NT (x:nt) = Nt x
      let _TM (x:tm) = Tm x
    
      let sym_case ~nt ~tm = function
        | Nt x -> nt x
        | Tm y -> tm y
    
      type sym_list = sym list
      let syms_nil (xs:sym_list) = match xs with [] -> true | _ -> false
      let syms_hd (xs:sym_list) = List.hd xs
      let syms_tl (xs:sym_list) = List.tl xs
    
      type nt_item = { nt:nt; i_:int; k_:int; bs:sym_list }
      let dot_nt x = x.nt
      let dot_i x = x.i_
      let dot_k x = x.k_
      let dot_bs x = x.bs
      let mk_nt_item nt i bs = { nt; i_=i; k_=i; bs }
    
      let cut itm j = {itm with k_=j; bs=List.tl itm.bs}
    
      (* type sym_item = { i_:int; sym:sym; j_:int } *)
      (* type sym_at_k = { sym:sym; k_:int }  *)
    end
    
    
    
    (*
    (** Generic type of items (for spec?) *)
    type ('a,'b,'c) generic_item = 
      | Nt_item of 'a
      | Sym_item of 'b
      | Sym_at_k of 'c
    *)
    
    (*
      let filter_sort_items itms = 
        itms
        |> Misc.rev_filter_map (function
            | Nt_item x -> Some x
            | _ -> None)
        |> List.sort (fun itm1 itm2 -> 
            let f {nt;i_;k_;bs} = nt,i_,k_,List.length bs,bs in
            Pervasives.compare (f itm1) (f itm2))
    *)
    
end

module Wip = struct 
    (*
    (** A simple specification of general parsing (not only Earley). For
       the implementation, see {!Earley_unstaged}. *)
    
    open Prelude
    
    module type REQUIRED = sig
      include Prelude.REQUIRED
      type sym_item = { i_:int; sym:sym; j_:int }
      type sym_at_k = { sym:sym; k_:int } 
    
      type item = 
        | Nt_item of nt_item
        | Sym_item of sym_item
        | Sym_at_k of sym_at_k
    end
    
    module Make_extra_items(A:sig type sym type nt_item end) = struct
      open A
      type sym_item = { i_:int; sym:sym; j_:int }
      type sym_at_k = { sym:sym; k_:int } 
    
      type item = 
        | Nt_item of nt_item
        | Sym_item of sym_item
        | Sym_at_k of sym_at_k
    end
    
    (** Internal implementation *)
    module Internal(S:sig include REQUIRED type state end) = struct
      open S
    
      module M = struct
        type 'a m = state -> 'a * state
        let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
          fun s ->
          a s |> fun (a,s) -> 
          ab a s
        let _ = ( >>= )
        let return a = fun s -> (a,s)
      end
      open M
    
      (** Main Earley routine, parameterized *)
      let _earley 
          ~(expand_nt:nt*int->unit m) ~(expand_tm:tm*int->unit m) 
          ~incr_count
          ~(get_blocked_items:int*sym->nt_item list m)
          ~(get_complete_items:int*sym -> int list m)
          ~(add_item:item -> unit m)
          ~(add_items:item list -> unit m)
          ~(pop_todo: unit-> item option m)
          ~(note_blocked_cuts: nt_item -> int list -> unit m)
          ~(note_complete_cuts: nt_item list -> int -> unit m)
        = 
    
        let mark = !spec_mark_ref in
    
        (* process a blocked item *)
        let cut_blocked_item = fun itm -> 
          let k_,_S = (itm|>dot_k,itm|>dot_bs|>syms_hd) in
          mark "am";
          get_complete_items (k_,_S) >>= fun js ->
          mark "ap";
          note_blocked_cuts itm js >>= fun () ->
          js |> List.map (fun j -> Nt_item (cut itm j))
          |> add_items >>= fun _ ->
          mark "as";
          return ()              
        in
    
        (* process a complete item *)
        let cut_complete_item =
          fun {i_;sym;j_} -> 
            mark "bm";
            get_blocked_items (i_,sym) >>= fun itms ->
            mark "bp";
            note_complete_cuts itms j_ >>= fun () ->
            itms |> List.map (fun itm -> Nt_item (cut itm j_))
            |> add_items >>= fun () ->
            mark "bs";
            return ()
        in
    
        (* process an item *)
        let step itm =
          mark "em";
          match itm with 
          | Nt_item itm -> (
              incr_count() >>= fun () -> 
              let bs = itm|>dot_bs in
              match bs|>syms_nil with
              | true -> 
                (* item is complete *)
                let itm = Sym_item {sym=_NT (itm|>dot_nt); i_=(itm|>dot_i); j_=(itm|>dot_k)} in
                add_item itm >>= fun () ->
                mark "ep";
                return ()
              | false -> (
                  let _S,bs = syms_hd bs, syms_tl bs in
                  (* we need to record that we need to expand _S *)
                  mark "er";
                  add_item (Sym_at_k { sym=_S; k_=(itm|>dot_k)}) >>= fun () ->
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
              match is_nt sym with
              | true -> expand_nt (dest_nt sym,k_) >>= fun () -> mark "ha"; return ()
              | false -> expand_tm (dest_tm sym,k_) >>= fun () -> mark "hb"; return ())        
        in
    
        let _ = step in
    
        (* loop until no items left to process *)
        let rec loop () = 
          pop_todo () >>= function
          | None -> return ()
          | Some itm -> step itm >>= fun _ -> loop ()
        in
        loop ()
    
      let _ :
        expand_nt:(nt * int -> unit m) ->
        expand_tm:(tm * int -> unit m) ->
        incr_count:(unit -> unit m) ->
        get_blocked_items:(int * sym -> nt_item list m) ->
        get_complete_items:(int * sym -> int list m) ->
        add_item:(item -> unit m) ->
        add_items:(item list -> unit m) ->
        pop_todo:(unit -> item option m) -> 
        note_blocked_cuts:(nt_item -> int list -> unit m) ->
        note_complete_cuts:(nt_item list -> int -> unit m) ->
        unit m
        = _earley
    
      (** expand_... are in the monad; adjust the types of expand_nt and
         expand_tm so that they return a list of items and a list of ints
         *)
      let earley ~expand_nt ~expand_tm ~add_items =
        let expand_nt (nt,i) = 
          add_items (expand_nt (nt,i) |> List.map (fun itm -> Nt_item itm))
        in
        let expand_tm (tm,i_) =
          expand_tm (tm,i_) 
          |> List.map (fun j_ -> (Sym_item{i_;sym=_TM tm;j_}))
          |> add_items
        in
        _earley ~expand_nt ~expand_tm ~add_items
    
      let _ : 
        expand_nt:(nt * int -> nt_item list) ->
        expand_tm:(tm * int -> int list) ->
        add_items:(item list -> unit m) ->
        incr_count:(unit -> unit m) ->
        get_blocked_items:(int * sym -> nt_item list m) ->
        get_complete_items:(int * sym -> int list m) ->
        add_item:(item -> unit m) -> pop_todo:(unit -> item option m) -> 
        note_blocked_cuts:(nt_item -> int list -> unit m) ->
        note_complete_cuts:(nt_item list -> int -> unit m) ->
        unit m = earley
    
    end
    
    (** Refine the state type; for the spec, we use an extremely
       inefficient state type (for which the implementation of the util
       functions are hopefully correct); see {!Earley_unstaged} for an
       efficient version *)
    module Internal_with_inefficient_spec_state(Req:REQUIRED) = struct
      open Req
    
      module State_type = struct
    
        (* todo_done is really a set; we add items to todo providing they
           are not already in todo_done *)
        type state = {
          count:int;
          mutable todo: item list;
          todo_done:(item,unit) Hashtbl.t
        }
        let empty_state = { count=0; todo=[]; todo_done=Hashtbl.create 100 }
      end
      open State_type
    
      module Internal=Internal(struct include Req include State_type end)
      open Internal
    
      (** The (executable) specification of parsing. Returns a list of
          items (FIXME?). Implementations such as Earley should return an
          identical set of items (for nt_items at least). NOTE that the
          parameters are independent of the input (in that the input is not
          present as an argument). *)
      let earley_spec ~expand_nt ~expand_tm = 
        let incr_count () s = (),{s with count=s.count+1} in
        let get_blocked_items (k,_S) s = 
          let blocked = ref [] in
          s.todo_done |> Hashtbl.iter (fun itm _ ->
              match itm with 
              | Nt_item itm -> (
                let k_,bs = itm|>dot_k,itm|>dot_bs in
                match k_=k && not (syms_nil bs) && (syms_hd bs=_S) with
                | true -> (blocked:=itm::!blocked)
                | false -> ())
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
        let note_blocked_cuts itm js s = (),s in
        let note_complete_cuts itms j s = (),s in
        fun ~initial_nt:(nt:Req.nt) ->
          { empty_state with todo=(expand_nt (nt,0)|>List.map (fun x -> Nt_item x)) }
          |> earley
            ~expand_nt ~expand_tm ~incr_count ~get_blocked_items ~get_complete_items
            ~add_item ~add_items ~pop_todo
            ~note_blocked_cuts ~note_complete_cuts
          |> fun ((),s) -> 
          let complete_items = 
            fun (i,_S) -> 
              get_complete_items (i,_S) s |> fun (n,_) -> n
          in
          let items = lazy begin
            s.todo_done 
            |> Hashtbl.to_seq_keys
            |> List.of_seq
          end
          in
          { count=s.count;items;complete_items }
    
      (* This exposes the nt_item type to the user; might prefer to use rhs list *)
      let earley_spec : 
        expand_nt:(nt * int -> Req.nt_item list) ->
        expand_tm:(tm * int -> int list) -> 
        initial_nt:nt -> ('b,'c) parse_result
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
          include Prelude.Simple_items(struct 
              type nonrec nt = nt
              type nonrec tm = tm
            end)
          include Make_extra_items(struct
              type nonrec sym = sym
              type nonrec nt_item = nt_item
            end)
        end
        in
        let open A in
        let module B = Internal_with_inefficient_spec_state(A) in
        (* let open B in *)
        (* let sym_to_sym = function `Nt nt -> Nt nt | `Tm tm -> Tm tm in *)
        (* let sym_to_sym = function Nt nt -> `Nt nt | Tm tm -> `Tm tm in  *)
        let expand_nt (nt,i) = 
          expand_nt (nt,i) |> List.map (fun bs ->
              (* bs |> List.map sym_to_sym |> fun bs -> *)
              {nt;i_=i;k_=i;bs})
        in
        fun ~initial_nt ->
          let res = B.earley_spec ~expand_nt ~expand_tm ~initial_nt in
          {res with items=()}  (* can't expose general items *)
    end
    *)
end

module Examples_with_actions = struct 
    (** Some examples, with actions *)
    
    (**
    
    {%html:
    
    <pre>
        let _EEE = 
          grammar 
            ~name:"EEE"
            ~descr:"Very ambiguous grammar, for testing Earley"
            ~initial_nt:_E
            ~rules:[
              _E -->_3 (nt _E,nt _E,nt _E) (fun (x,y,z) -> x+y+z);
              _E -->_1 one (fun _ -> 1);
              _E -->_1 eps (fun _ -> 0);
            ]
        in
        let aho_s = 
          grammar
            ~name:"aho_s"
            ~descr:"Aho et al. example grammar"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,nt _S,nt _S) (fun (x,y,z) -> "x"^y^z);
              _S -->_1 eps (fun _ -> "")
            ]
        in
    </pre>
    
    %}
    *)
    
    module Internal0 = struct
      module type INTERNAL_TYPED_SYMS = sig
        type 'a nt
        type 'a sym
        type 'a tm
        val _E : int nt
        val _S : string nt
        val a: string -> string tm
      end
    
      module type INTERNAL_REQS = sig 
        include INTERNAL_TYPED_SYMS
        type 'a rhs 
        type rule 
        val _1: 'a sym -> ('a -> 'b) -> 'b rhs
        val _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs
        val _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs
    
        val ( --> ) : 'a nt -> 'a rhs -> rule
        val nt : 'a nt -> 'a sym
        val tm : 'a tm -> 'a sym
    
        (* type u_nt  (\* untyped nt *\) *)
    
        type 'a grammar 
        val grammar: name:string -> descr:string -> initial_nt:'a nt ->
          rules:rule list -> 'a grammar
      end
    
      module type INTERNAL2_REQS = sig 
        type 'a nt 
        type u_nt
        val nt2u : 'a nt -> u_nt
        type 'a sym 
        type u_sym
        val sym2u : 'a sym -> u_sym
        type uni_val 
      end
    
    (*
      module type INTERNAL2_REQS = sig 
        include INTERNAL_ABSTRACT_TYPED_SYMS
    
        val u_nt2string: u_nt -> string
        val u_sym2string: u_sym -> string
      end
    *)
    
    end
    
    open Internal0
    
    (** Internal: grammmars defined abstractly *)
    module Internal(A:INTERNAL_REQS) = struct
    
      open A
    
      let example_grammars =
        let [one;eps;x] : string sym list = List.map a ["1";"";"x"] |> List.map tm in
        let _EEE = 
          grammar 
            ~name:"EEE"
            ~descr:"Very ambiguous grammar, for testing Earley"
            ~initial_nt:_E
            ~rules:[
              _E -->_3 (nt _E,nt _E,nt _E) (fun (x,y,z) -> x+y+z);
              _E -->_1 one (fun _ -> 1);
              _E -->_1 eps (fun _ -> 0);
            ]
        in
        let aho_s = 
          grammar
            ~name:"aho_s"
            ~descr:"Aho et al. example grammar"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,nt _S,nt _S) (fun (x,y,z) -> "x"^y^z);
              _S -->_1 eps (fun _ -> "")
            ]
        in
        (_EEE,aho_s)
    
      let _ = example_grammars
    end
    
    (** Internal2: translate grammars to untyped representation *)
    module Internal2(B:INTERNAL2_REQS) = struct
      open B
    
      type u_rhs = u_sym list * (uni_val list -> uni_val)
      type rule = u_nt * u_rhs
    
      type 'a rhs = u_rhs
    
      let _1 (_S:'a sym) (f:'a -> 'b) : 'b rhs = 
        ([sym2u _S], fun [v] -> Obj.magic (f (Obj.magic v)))
    
      let _2: ('a sym * 'b sym) -> ('a * 'b -> 'c) -> 'c rhs = 
        fun (s1,s2) f ->
        ([sym2u s1; sym2u s2], fun[v1;v2] -> Obj.magic (f (Obj.magic v1, Obj.magic v2)))
    
      let _3: ('a sym * 'b sym * 'c sym) -> ('a * 'b * 'c -> 'd) -> 'd rhs =
        fun (s1,s2,s3) f ->
        [sym2u s1; sym2u s2; sym2u s3],
        fun [v1;v2;v3] -> Obj.magic (f (Obj.magic v1, Obj.magic v2, Obj.magic v3))
    
      let mk_rule : 'a nt -> 'a rhs -> rule = fun nt rhs -> (nt2u nt,rhs)
      let ( --> ) = mk_rule
    
      type 'a grammar = {
        name:string;
        descr:string;
        initial_nt:'a nt;
        rules:u_nt -> u_rhs list
      } 
    
      let grammar: name:string -> descr:string -> initial_nt:'a nt ->
        rules:rule list -> 'a grammar 
        =
        fun ~name ~descr ~initial_nt ~rules -> 
        let tbl = Hashtbl.create 100 in
        List.rev rules |> List.iter (fun (nt,rhs) -> 
            Hashtbl.find_opt tbl nt |> function
            | None -> Hashtbl.replace tbl nt [rhs]
            | Some rhss -> Hashtbl.replace tbl nt (rhs::rhss));
        let rules nt = Hashtbl.find_opt tbl nt |> function
          | None -> []
          | Some rhss -> rhss
        in
        { name; descr; initial_nt; rules }
    end
    
    
    module Internal3 = struct
    
      (* Implementation of INTERNAL2_REQS *)
      module Internal2_reqs : sig
        type 'a nt
    
        (** For debugging, make u_nt a string *)
        type u_nt = string 
        val nt2u : 'a nt -> u_nt
    
        type 'a tm
        type u_tm = string
    
    
        type 'a sym
        type u_sym = (u_nt,u_tm)Prelude.generic_sym
        val sym2u : 'a sym -> u_sym
        type uni_val
        val nt : 'a nt -> 'a sym
        val tm: 'a tm -> 'a sym
        val _E : int nt
        val _S : string nt
        val a : string -> string tm
      end = struct
        type 'a nt = string
        type u_nt = string
        let nt2u : 'a nt -> u_nt = fun x -> x
    
        type 'a tm = string
        type u_tm = string
    
        type u_sym = (u_nt,u_tm)Prelude.generic_sym
        type 'a sym = u_sym
        let sym2u : 'a sym -> u_sym = fun x -> x
        type uni_val
    
        let u_nt2string x = x
        let u_sym2string x = x
    
        (* from A ; FIXME *)
        let nt : 'a nt -> 'a sym = fun x -> Nt x
        let tm : 'a tm -> 'a sym = fun x -> Tm x
    
        let _E : int nt ="E"
        let _S : string nt = "S"
        let a s = s
      end
    
      (** NOTE use Internal2 *)
      module C = Internal2(Internal2_reqs)
    
      module A2 = struct
        include Internal2_reqs
        include C
      end
    
      module D : INTERNAL_REQS = A2
    
      (** NOTE use Internal *)
      module F = Internal(A2)
    
      (** Export abstract interface. *)
      module Export : sig
        type 'a grammar = 'a C.grammar
        val _EEE : int grammar
        val aho_s : string grammar
      end = struct
        type 'a grammar = 'a C.grammar
        let example_grammars = F.example_grammars
        let _EEE,aho_s = example_grammars
      end
      
    end
    
    open Internal3
    
    (* include Internal3.Internal2_reqs *)
    
    (** NOTE following types are present in the ['a grammar] type *)
    
    (** Just a string *)
    type u_nt = Internal3.Internal2_reqs.u_nt 
    
    type 'a nt = 'a Internal3.Internal2_reqs.nt
    let nt2u: 'a nt -> u_nt = Internal3.Internal2_reqs.nt2u
    
    (** Just a string *)
    type u_tm = Internal3.Internal2_reqs.u_tm 
    
    (** A [generic_sym] over u_nt and u_tm *)
    type u_sym = Internal2_reqs.u_sym
    
    (** Used for actions *)
    type uni_val = Internal2_reqs.uni_val
    
    (** A list of u_sym, and an action *)
    type u_rhs = Internal3.C.u_rhs
    
    
    type 'a grammar = 'a Internal3.C.grammar
    
    
    let _EEE : int grammar = Internal3.Export._EEE
    let aho_s : string grammar = Internal3.Export.aho_s
    
    
end

module Examples = struct 
    (** Some examples *)
    
    
    (** Example grammar
       names are: EEE, aho_s, aho_sml, brackets, S_xSx 
    
    {%html:
    
    <pre>
        let _EEE = 
          p#grammar 
            ~name:"EEE"
            ~descr:"Very ambiguous grammar, for testing Earley"
            ~initial_nt:_E
            ~rules:[
              _E -->_3 (_E,_E,_E);
              _E -->_1 one;
              _E -->_1 eps;
            ]
        in
        let aho_s = 
          p#grammar
            ~name:"aho_s"
            ~descr:"Aho et al. example grammar"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,_S,_S);
              _S -->_1 eps
            ]
        in
        let aho_sml = 
          p#grammar
            ~name:"aho_sml"
            ~descr:"Aho et al. example grammar 2"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (_S,_S,x);
              _S -->_1 eps
            ]
        in
        let brackets = 
          p#grammar
            ~name:"brackets"
            ~descr:
              "Well-bracketed expressions, in a particular nasty form for parsing"
            ~initial_nt:_E
            ~rules:[
              _E -->_2 (_E,_E);
              _E -->_3 (a"(",_E,a")");
              _E -->_1 eps
            ]
        in
        let _S_xSx = 
          p#grammar 
            ~name:"S_xSx"
            ~descr:"Unambiguous grammar that favours right-most parsers"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,_S,x);
              _S -->_1 x
            ]
        in
    </pre>
    
    %}
    
    *)
    
    
    (** Internal: grammmars defined abstractly. NOTE the following assumes
       nt, tm and sym are all the same type. *)
    module Internal = struct
    
      let example_grammars p =
        let ( --> ) = p#make_rule in
        let _1,_2,_3 = p#_1,p#_2,p#_3 in
        let _E,_S,a = p#_E,p#_S,p#a in
        let [one;eps;x] = List.map a ["1";"";"x"] in
        let _EEE = 
          p#grammar 
            ~name:"EEE"
            ~descr:"Very ambiguous grammar, for testing Earley"
            ~initial_nt:_E
            ~rules:[
              _E -->_3 (_E,_E,_E);
              _E -->_1 one;
              _E -->_1 eps;
            ]
        in
        let aho_s = 
          p#grammar
            ~name:"aho_s"
            ~descr:"Aho et al. example grammar"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,_S,_S);
              _S -->_1 eps
            ]
        in
        let aho_sml = 
          p#grammar
            ~name:"aho_sml"
            ~descr:"Aho et al. example grammar 2"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (_S,_S,x);
              _S -->_1 eps
            ]
        in
        let brackets = 
          p#grammar
            ~name:"brackets"
            ~descr:
              "Well-bracketed expressions, in a particular nasty form for parsing"
            ~initial_nt:_E
            ~rules:[
              _E -->_2 (_E,_E);
              _E -->_3 (a"(",_E,a")");
              _E -->_1 eps
            ]
        in
        let _S_xSx = 
          p#grammar 
            ~name:"S_xSx"
            ~descr:"Unambiguous grammar that favours right-most parsers"
            ~initial_nt:_S
            ~rules:[
              _S -->_3 (x,_S,x);
              _S -->_1 x
            ]
        in
        [_EEE;aho_s;aho_sml;brackets;_S_xSx]
    
      let _ = example_grammars
    end
    
    
    
    (** Example instantiation with strings for symbols *)
    module Internal_example_instantiation = struct
    
      (** A named tuple for tagging grammars in a slightly more digestible
          form than a plain tuple *)
      type ('a,'b) grammar = {
        name:string;
        descr:string;
        initial_nt:'a;
        rules:'b
      }
    
    
      open Prelude
    
      type nt = string
      type tm = string
      type sym = (nt,tm) Prelude.generic_sym
      type rule = nt * sym list
    
      (** Hack to determine nt/tm based on string repr starting with a
          capital letter *)
      let is_nt nt = nt <> "" && (String.get nt 0 |> function
        | 'A' .. 'Z' -> true
        | _ -> false)
    
      let string_to_sym s = match is_nt s with 
        | true -> Nt s
        | false -> Tm s
    
      let make_rule nt rhs = (nt,rhs|>List.map string_to_sym)
    
      let _1 s = [s]
      let _2 (s1,s2) = [s1;s2]
      let _3 (s1,s2,s3) = [s1;s2;s3]
    
      let _E = "E"
      let _S = "S"
      let a s = s
      let eps = ""
      let one = "1"
      let x = "x"
    
      let grammar ~name ~descr ~initial_nt ~rules = {name;descr;initial_nt;rules}
    
      let example_grammars = 
        let p = object
          method _1 = _1
          method _2 = _2
          method _3 = _3
          method make_rule = make_rule
          method grammar = grammar
          method _E = _E
          method _S = _S
          method a = a
          method eps = eps
          method one = one
          method x = x
          end
        in
        Internal.example_grammars p
    
      let _ = example_grammars
    
      module Export = struct
    
        (** NOTE nonterminals and terminals are represented by strings *)
    
        let grammar_names = ["EEE";"aho_s";"aho_sml";"brackets";"S_xSx"]
    
        let get_grammar_by_name name : (nt,tm) Prelude.simple_grammar * nt = 
          let g = example_grammars |> List.find (fun g -> g.name = name) in
          let tbl = Hashtbl.create 100 in
          List.rev g.rules |> List.iter (fun (nt,rhs) -> 
            Hashtbl.find_opt tbl nt |> function
            | None -> Hashtbl.replace tbl nt [rhs]
            | Some rhss -> Hashtbl.replace tbl nt (rhs::rhss));
          let nt_to_rhss nt = Hashtbl.find_opt tbl nt |> function
          | None -> []
          | Some rhss -> rhss
          in
          Prelude.({nt_to_rhss},g.initial_nt)
    
    
      end
      
    end
    
    include Internal_example_instantiation.Export
end

module Earley_spec = struct 
    (** A simple specification of general parsing (not only Earley). For
       the implementation, see {!Earley_unstaged}. *)
    
    open Prelude
    
    module type REQUIRED = sig
      include Prelude.REQUIRED
      type sym_item = { i_:int; sym:sym; j_:int }
      type sym_at_k = { sym:sym; k_:int } 
    
      type item = 
        | Nt_item of nt_item
        | Sym_item of sym_item
        | Sym_at_k of sym_at_k
    end
    
    module Make_extra_items(A:sig type sym type nt_item end) = struct
      open A
      type sym_item = { i_:int; sym:sym; j_:int }
      type sym_at_k = { sym:sym; k_:int } 
    
      type item = 
        | Nt_item of nt_item
        | Sym_item of sym_item
        | Sym_at_k of sym_at_k
    end
    
    (** Internal implementation *)
    module Internal(S:sig include REQUIRED type state end) = struct
      open S
    
      module M = struct
        type 'a m = state -> 'a * state
        let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
          fun s ->
          a s |> fun (a,s) -> 
          ab a s
        let _ = ( >>= )
        let return a = fun s -> (a,s)
      end
      open M
    
      (** Main Earley routine, parameterized *)
      let _earley 
          ~(expand_nt:nt*int->unit m) ~(expand_tm:tm*int->unit m) 
          ~incr_count
          ~(get_blocked_items:int*sym->nt_item list m)
          ~(get_complete_items:int*sym -> int list m)
          ~(add_item:item -> unit m)
          ~(add_items:item list -> unit m)
          ~(pop_todo: unit-> item option m)
          ~(note_blocked_cuts: nt_item -> int list -> unit m)
          ~(note_complete_cuts: nt_item list -> int -> unit m)
        = 
    
        let mark = !spec_mark_ref in
    
        (* process a blocked item *)
        let cut_blocked_item = fun itm -> 
          let k_,_S = (itm|>dot_k,itm|>dot_bs|>syms_hd) in
          mark "am";
          get_complete_items (k_,_S) >>= fun js ->
          mark "ap";
          note_blocked_cuts itm js >>= fun () ->
          js |> List.map (fun j -> Nt_item (cut itm j))
          |> add_items >>= fun _ ->
          mark "as";
          return ()              
        in
    
        (* process a complete item *)
        let cut_complete_item =
          fun {i_;sym;j_} -> 
            mark "bm";
            get_blocked_items (i_,sym) >>= fun itms ->
            mark "bp";
            note_complete_cuts itms j_ >>= fun () ->
            itms |> List.map (fun itm -> Nt_item (cut itm j_))
            |> add_items >>= fun () ->
            mark "bs";
            return ()
        in
    
        (* process an item *)
        let step itm =
          mark "em";
          match itm with 
          | Nt_item itm -> (
              incr_count() >>= fun () -> 
              let bs = itm|>dot_bs in
              match bs|>syms_nil with
              | true -> 
                (* item is complete *)
                let itm = Sym_item {sym=_NT (itm|>dot_nt); i_=(itm|>dot_i); j_=(itm|>dot_k)} in
                add_item itm >>= fun () ->
                mark "ep";
                return ()
              | false -> (
                  let _S,bs = syms_hd bs, syms_tl bs in
                  (* we need to record that we need to expand _S *)
                  mark "er";
                  add_item (Sym_at_k { sym=_S; k_=(itm|>dot_k)}) >>= fun () ->
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
              match is_nt sym with
              | true -> expand_nt (dest_nt sym,k_) >>= fun () -> mark "ha"; return ()
              | false -> expand_tm (dest_tm sym,k_) >>= fun () -> mark "hb"; return ())        
        in
    
        let _ = step in
    
        (* loop until no items left to process *)
        let rec loop () = 
          pop_todo () >>= function
          | None -> return ()
          | Some itm -> step itm >>= fun _ -> loop ()
        in
        loop ()
    
      let _ :
        expand_nt:(nt * int -> unit m) ->
        expand_tm:(tm * int -> unit m) ->
        incr_count:(unit -> unit m) ->
        get_blocked_items:(int * sym -> nt_item list m) ->
        get_complete_items:(int * sym -> int list m) ->
        add_item:(item -> unit m) ->
        add_items:(item list -> unit m) ->
        pop_todo:(unit -> item option m) -> 
        note_blocked_cuts:(nt_item -> int list -> unit m) ->
        note_complete_cuts:(nt_item list -> int -> unit m) ->
        unit m
        = _earley
    
      (** expand_... are in the monad; adjust the types of expand_nt and
         expand_tm so that they return a list of items and a list of ints
         *)
      let earley ~expand_nt ~expand_tm ~add_items =
        let expand_nt (nt,i) = 
          add_items (expand_nt (nt,i) |> List.map (fun itm -> Nt_item itm))
        in
        let expand_tm (tm,i_) =
          expand_tm (tm,i_) 
          |> List.map (fun j_ -> (Sym_item{i_;sym=_TM tm;j_}))
          |> add_items
        in
        _earley ~expand_nt ~expand_tm ~add_items
    
      let _ : 
        expand_nt:(nt * int -> nt_item list) ->
        expand_tm:(tm * int -> int list) ->
        add_items:(item list -> unit m) ->
        incr_count:(unit -> unit m) ->
        get_blocked_items:(int * sym -> nt_item list m) ->
        get_complete_items:(int * sym -> int list m) ->
        add_item:(item -> unit m) -> pop_todo:(unit -> item option m) -> 
        note_blocked_cuts:(nt_item -> int list -> unit m) ->
        note_complete_cuts:(nt_item list -> int -> unit m) ->
        unit m = earley
    
    end
    
    (** Refine the state type; for the spec, we use an extremely
       inefficient state type (for which the implementation of the util
       functions are hopefully correct); see {!Earley_unstaged} for an
       efficient version *)
    module Internal_with_inefficient_spec_state(Req:REQUIRED) = struct
      open Req
    
      module State_type = struct
    
        (* todo_done is really a set; we add items to todo providing they
           are not already in todo_done *)
        type state = {
          count:int;
          mutable todo: item list;
          todo_done:(item,unit) Hashtbl.t
        }
        let empty_state = { count=0; todo=[]; todo_done=Hashtbl.create 100 }
      end
      open State_type
    
      module Internal=Internal(struct include Req include State_type end)
      open Internal
    
      (** The (executable) specification of parsing. Returns a list of
          items (FIXME?). Implementations such as Earley should return an
          identical set of items (for nt_items at least). NOTE that the
          parameters are independent of the input (in that the input is not
          present as an argument). *)
      let earley_spec ~expand_nt ~expand_tm = 
        let incr_count () s = (),{s with count=s.count+1} in
        let get_blocked_items (k,_S) s = 
          let blocked = ref [] in
          s.todo_done |> Hashtbl.iter (fun itm _ ->
              match itm with 
              | Nt_item itm -> (
                let k_,bs = itm|>dot_k,itm|>dot_bs in
                match k_=k && not (syms_nil bs) && (syms_hd bs=_S) with
                | true -> (blocked:=itm::!blocked)
                | false -> ())
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
        let note_blocked_cuts itm js s = (),s in
        let note_complete_cuts itms j s = (),s in
        fun ~initial_nt:(nt:Req.nt) ->
          { empty_state with todo=(expand_nt (nt,0)|>List.map (fun x -> Nt_item x)) }
          |> earley
            ~expand_nt ~expand_tm ~incr_count ~get_blocked_items ~get_complete_items
            ~add_item ~add_items ~pop_todo
            ~note_blocked_cuts ~note_complete_cuts
          |> fun ((),s) -> 
          let complete_items = 
            fun (i,_S) -> 
              get_complete_items (i,_S) s |> fun (n,_) -> n
          in
          let items = lazy begin
            s.todo_done 
            |> Hashtbl.to_seq_keys
            |> List.of_seq
          end
          in
          { count=s.count;items;complete_items; debug=() }
    
      (* This exposes the nt_item type to the user; might prefer to use rhs list *)
      let earley_spec : 
        expand_nt:(nt * int -> Req.nt_item list) ->
        expand_tm:(tm * int -> int list) -> 
        initial_nt:nt -> ('b,'c,'d) parse_result
        = earley_spec
    
    end
    
    
    
    (** An example parse function which is polymorphic over symbols; no
       functors involved. *)
    module Internal_example_parse_function = struct
      
      (** An (executable) parsing specification polymorphic over
          nonterminals and terminals *)
      let earley_spec (type nt tm) ~expand_nt ~(expand_tm:tm*int -> int list)  =
        let module A = struct
          type nonrec nt = nt
          type nonrec tm = tm      
          include Prelude.Simple_items(struct 
              type nonrec nt = nt
              type nonrec tm = tm
            end)
          include Make_extra_items(struct
              type nonrec sym = sym
              type nonrec nt_item = nt_item
            end)
        end
        in
        let open A in
        let module B = Internal_with_inefficient_spec_state(A) in
        (* let open B in *)
        (* let sym_to_sym = function `Nt nt -> Nt nt | `Tm tm -> Tm tm in *)
        (* let sym_to_sym = function Nt nt -> `Nt nt | Tm tm -> `Tm tm in  *)
        let expand_nt (nt,i) = 
          expand_nt nt |> List.map (fun bs ->
              (* bs |> List.map sym_to_sym |> fun bs -> *)
              {nt;i_=i;k_=i;bs})
        in
        fun ~initial_nt ->
          let res = B.earley_spec ~expand_nt ~expand_tm ~initial_nt in
          {res with items=()}  (* can't expose general items *)
    end
end

module Earley_base = struct 
    (** Internal Earley implementation; see {!Earley_simple} for the
        external usable version.
    
        This is the main (internal) Earley implementation, based on
       processing items at index k in the input, before moving to
       k+1. This version of the code tries to assume as little as possible
       about the representation of the underlying structures.  *)
    
    open Prelude
    
    
    (** What is required by the [Make] functor *) 
    module type REQUIRED_BY_BASE = sig
    
      type i_t = int  
      type k_t = int
      type j_t = int
    
      type nt
      type tm
      type sym
    
      val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
      val _NT: nt -> sym
      val _TM: tm -> sym
    
      type nt_item  
      val dot_nt: nt_item -> nt
      val dot_i: nt_item -> i_t
      val dot_k: nt_item -> k_t
      val dot_bs: nt_item -> sym list
      val cut: nt_item -> j_t -> nt_item
      val mk_nt_item : nt -> int -> sym list -> nt_item
    
      type nt_item_set
      val empty_nt_item_set: nt_item_set
      val elements : nt_item_set -> nt_item list
      val nt_item_set_of_list: nt_item list -> nt_item_set
    
      (** int -> bitms_at_k  FIXME implement as hashtbl *)
      type bitms_lt_k  
    
      (* int -> nt_item_set *)
      type todo_gt_k 
    
    
      (** NOTE following are per k *)
    
      (** nt -> nt_item_set *)
      type bitms_at_k 
    
      (** (int*nt) set *)
      type ixk_done  
    
      (** tm -> j list option *)
      type ktjs  
    
    
      val todo_gt_k_find: int -> todo_gt_k -> nt_item_set
      val update_bitms_lt_k: int -> bitms_at_k -> bitms_lt_k -> bitms_lt_k (* FIXME what for? *)
    
      val empty_todo_gt_k: todo_gt_k
      val empty_bitms_lt_k: bitms_lt_k
      val empty_bitms_at_k: bitms_at_k
      val empty_ixk_done: ixk_done
      val empty_ktjs: ktjs
    
    end  (* REQUIRED_BY_BASE *)
    
    
    (** Construct the Earley parsing implementation *)
    module Make(A:REQUIRED_BY_BASE) = struct
    
      (** {2 Provided by user} *)
    
      (* to make doc self-contained *)
      include A
    
      (** {2 Content of Make proper starts here} *)
    
      type state = {
        count: int;
        todo: nt_item list;
        todo_done: nt_item_set;
        todo_gt_k: todo_gt_k;
        bitms_lt_k: bitms_lt_k;
        bitms_at_k: bitms_at_k;
        ixk_done: ixk_done;
        ktjs:ktjs;
        (* cuts:cuts *)
      }
    
    
      let empty_state = {
        count=0;
        todo=[];
        todo_done=empty_nt_item_set;
        todo_gt_k=empty_todo_gt_k;
        bitms_lt_k=empty_bitms_lt_k;
        bitms_at_k=empty_bitms_at_k;
        ixk_done=empty_ixk_done;
        ktjs=empty_ktjs;
        (* cuts=empty_cuts *)
      }
        
    
      type 'a m = state -> 'a * state
      let ( >>= ) (a:'a m) (ab:'a -> 'b m) : 'b m = 
        fun s ->
          a s |> fun (a,s) -> 
          ab a s
      let _ = ( >>= )
      let return a = fun s -> (a,s)
    
    
      (* FIXME these expose the state type via _ m *)
      (** 
         - pop_todo can be implemented directly
         - get_bitms_at_k needs bitms_at_k, and a way to map nt_item_set to list (which we have: elements)
         - add_bitm_at_k is fine
         - add_todos_at_k is fine
         - add_todos_gt_k is fine
         - rest are fine
    
         NOTE if we use a mutable impl of state, we can avoid having to
         lookup via nt, update set, and update via nt
      *)
      type atomic_operations = {
        pop_todo: unit -> nt_item option m;
        (* FIXME this op is already known now we know 'a m *)
    
        get_bitms_at_k: nt -> nt_item list m;  (* or set? *)
        get_bitms_lt_k: int * nt -> nt_item list m;  (* or set? *)
        add_bitm_at_k: nt_item -> nt -> unit m;  (* FIXME don't need nt *)
        (* these three could be implemented as funs t -> t, then lifted to the monad *)
    
        add_todos_at_k: nt_item list -> unit m;
        add_todos_gt_k: nt_item list -> unit m;
        (* FIXME these two could be implemented here, since we know the state impl type *)
    
        add_ixk_done: int*nt -> unit m;
        mem_ixk_done: int*nt -> bool m;
        (* FIXME these, and others, could be implemented outside the monad
           as a funtion t->t, then injected into the monad now we know
           what 'a m is *)
    
        find_ktjs: tm -> int list option m;
        add_ktjs: tm -> int list -> unit m;
    
        (* record_cuts: (nt_item * int) list -> unit m; *)
      }
    
    
      (** Hide the following defns from the user *)
      module Internal2 = struct
    
        let run_earley (*~item_ops*) ~at_ops = 
          let { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
                add_todos_at_k; add_todos_gt_k; add_ixk_done;
                mem_ixk_done; find_ktjs; add_ktjs } = at_ops
          in
          let with_state: (state -> state) -> unit m = fun f -> 
            fun s -> ((),f s)
          in
          let image = List.map in
          let is_finished nitm = nitm|>dot_bs = [] in
          let module Let_syntax = struct 
            let bind a ~f = a >>= f 
          end
          in
          fun ~grammar ~parse_tm ~input ->
            let mark = !base_mark_ref in
            let {parse_tm}=parse_tm in
            let {nt_input_to_rhss}=grammar in
            let new_items ~nt ~input ~pos = 
              nt_input_to_rhss ~nt ~input ~pos |> fun rhss -> 
              rhss |> List.map (fun rhs -> 
                  let rhs = rhs |> List.map (function Nt nt -> _NT nt | Tm tm -> _TM tm) in
                  mk_nt_item nt pos rhs)
            in
            begin
    
              (* 
    
    Explanation of step_at_k code which follows:
    
    The basic Earley step is:
    
    X -> i as k',S bs     k' S k
    ----------------------------
    X -> i as S k bs
    
    In the code, there are labels of the form (*:am:*). The following
    discussion is indexed by these labels
    
    - af: 
      - the item nitm is complete, ie of the form Y -> k',as,k,[]
      - aj,al: has (k',Y,k) been encountered before? if so, do nothing
      - am: if not encountered before, k' Y k is cut with blocked X ->
        ... and new todo items are added
    
    - ax: 
      - item is not complete ie of form _ -> i,as,k,S bs
    
    - ax/ce: 
      - S is nonterm Y
      - add bitm to blocked items at (k,Y)
      - check if we have seen (k,Y) before (bitms_empty)
      - co: if we have, check if k Y k; cut bitm with k Y k if so
      - cw: if we haven't, generate new items from (k,Y)
    
    - ax/ec:
      - S is terminal tm
      - attempt to retrieve (k,tm,j) set from ktjs
      - ek: if we haven't already met (k,tm) then parse (k,tm), update
        ktjs and pass on js
      - otherwise, just reuse js from previously
      - el: given the set of js (which are all >= k)
      - partition into >k, and =k
      - for j > k, cut bitm with j, and add to todos
        - note that if this is the first time we meet (k,tm), then there
          are no other items blocked on (k,tm); if this is not the first
          time, then we have already processed items blocked on (k,tm); in
          either case, we do not need to do anything more with items
          blocked on (k,tm); in fact, we don't even need to record such
          items
      - em: if k is in js (ie tm matched the empty string) cut bitm with k
    
    *)
    
              let step_at_k k nitm = 
                mark "aa";
                let get_bitms (i,x) =
                  if i=k then get_bitms_at_k x else
                    get_bitms_lt_k (i,x)
                in
    
                match is_finished nitm with 
                | true -> (                                                       (*:af:*)
                    mark "af";
                    let (k',_Y) = (nitm|>dot_i,nitm|>dot_nt) in  
                    let%bind already_done = mem_ixk_done (k',_Y) in               (*:aj:*)
                    mark "ak";
                    match already_done with     
                    | true -> mark "al"; return ()                                (*:al:*)
                    | false -> (                                                  (*:am:*)
                        mark "am";                
                        add_ixk_done (k',_Y) >>= fun _ ->                   
                        mark "ap";
                        get_bitms (k',_Y) >>= fun bitms ->                  
                        mark "ar";
                        (* record_cuts (List.map (fun bitm -> (bitm,k)) bitms) >>= fun _ -> *)
                        (* mark "as"; *)
                        (* NOTE the following image is guaranteed not to
                           contain duplicates... (?) *)
                        let new_todos_at_k = image (fun bitm -> cut bitm k) bitms in
                        (* Printf.printf "debug: %d %d\n%!" (List.length bitms) (nt_item_set_of_list new_todos_at_k |> elements |> List.length); *)
                        mark "at";
                        add_todos_at_k new_todos_at_k >>= fun _ ->
                        mark "au"; return ()))
                | false -> (                                                      (*:ax:*)
                    mark "ax";
                    let bitm = nitm in   
                    let _S = List.hd (bitm|>dot_bs) in 
                    _S |> sym_case  
                      ~nt:(fun _Y ->                                              (*:ce:*)
                          mark "ce";
                          get_bitms_at_k _Y >>= fun bitms ->     
                          mark "ch";
                          let bitms_empty = bitms=[] in     
                          add_bitm_at_k bitm _Y >>= fun _ ->     
                          mark "ck";
                          match bitms_empty with  
                          | false -> (                                            (*:co:*)
                              mark "co";
                              mem_ixk_done (k,_Y) >>= function    
                              | true -> 
                                (* record_cuts [(bitm,k)] >>= fun _ -> *)
                                add_todos_at_k [cut bitm k] >>= fun _ ->
                                mark "cr";
                                return ()
                              | false -> return ())    
                          | true -> (                                             (*:cw:*)
                              mark "cw";
                              let itms = new_items ~nt:_Y ~input ~pos:k in
                              add_todos_at_k itms >>= fun _ ->
                              mark "cz";
                              return ()
                            ))  
                      ~tm:(fun tm ->                                              (*:ec:*)
                          mark "ec";
                          find_ktjs tm >>= fun ktjs ->     
                          (match ktjs with
                           | None -> (
                               (* we need to process kT *)                        (*:ek:*)
                               let js = parse_tm ~tm ~input ~pos:k in 
                               add_ktjs tm js >>= fun _ ->  
                               return js) 
                           | Some js -> return js) >>= fun js -> 
                          (* there may be a k in js, in which case we have a 
                             new todo at the current stage *)
                          let (xs,js) = List.partition (fun j -> j=k) js in       (*:el:*)
                          (* record_cuts (List.map (fun j -> (bitm,j)) js) >>= fun _ ->  *)
                          add_todos_gt_k (image (fun j -> cut bitm j) js) >>= fun _ ->
                          match xs with                                           (*:em:*)
                          | [] -> return ()     
                          | _ -> add_todos_at_k [cut bitm k]))
              in 
    
    
              (* FIXME monad syntax may make this easier to read *)
              let rec loop_at_k k = 
                (* print_endline "loop_at_k"; *)
                pop_todo () >>= function
                | None -> return ()
                | Some itm -> step_at_k k itm >>= fun _ -> loop_at_k k
              in
    
              let rec loop k = 
                (* Printf.printf "loop %d\n" k; *)
                match k > input.input_length with  
                | true -> return ()
                | false -> 
                  (* process items *)
                  loop_at_k k >>= fun _ ->
                  let k' = k+1 in
                  (* 
               todo and todo_done are updated with todo_gt_k[k'];
               bitms_lt_k is updated: bitms_lt_k[k]=bitms_at_k
               bitms_at_k is reset;
               ixk_done and ktjs are reset *)
                  with_state (fun s ->
                      let todo' = todo_gt_k_find k' s.todo_gt_k in
                      let todo = elements todo' in
                      (* Printf.printf "elements: %d" (List.length todo); *)
                      { count=s.count;
                        todo;
                        todo_done=todo';
                        todo_gt_k=s.todo_gt_k;
                        bitms_lt_k=(update_bitms_lt_k k s.bitms_at_k s.bitms_lt_k);
                        bitms_at_k=empty_bitms_at_k;
                        ixk_done=empty_ixk_done;
                        ktjs=empty_ktjs;
                        (* cuts=s.cuts; *)
                      }) >>= fun _ ->
                  loop k'
              in
    
    
              loop 0
            end (* run_earley *)
    
        let _ : 
    at_ops:atomic_operations ->
    grammar:(nt, tm, 'a) input_dependent_grammar ->
    parse_tm:(tm, 'a) terminal_input_matcher -> input:'a input -> unit m
    = run_earley
    
      end  (* Internal2 *)
    
    
      module Export : sig
        
        (** Abstract type of Earley parsers *)
        type earley_parser
    
        (** Construct a generic Earley parser (independent of grammar) *)
        val make_earley_parser: 
          at_ops:atomic_operations ->
          earley_parser
    
        (** Execute the Earley parser on a given grammar and
           input. Returns the final state. We don't mind exposing the
           state type because this is an internal library, intended to be
           used eg by {!Earley_simple}. *)
        val run_earley_parser:
          earley_parser:earley_parser -> 
          grammar:(nt, tm, 'a) input_dependent_grammar ->
          parse_tm:(tm, 'a) terminal_input_matcher -> 
          input:'a input -> 
          initial_state:state ->
          state
      end = struct
        type earley_parser = {
          run_parser: 
            'a. grammar:(nt, tm, 'a) input_dependent_grammar ->
            parse_tm:(tm, 'a) terminal_input_matcher -> 
            input:'a input -> 
            unit m
        }
    
        let make_earley_parser ~at_ops =
          {run_parser=(fun ~grammar ~parse_tm ~input -> 
               Internal2.run_earley ~at_ops ~grammar ~parse_tm ~input)}
    
        let _run_earley_parser ~earley_parser ~grammar ~parse_tm ~input = 
          earley_parser.run_parser ~grammar ~parse_tm ~input
    
        (**FIXME we can hide the earley_parser type, and just return a
           function that takes grammar_etc...; finally, we can return
           interesting parts of the state separately, so that the
           functionality does not depend on any types we define above *)
        let run_earley_parser ~earley_parser ~grammar ~parse_tm ~input ~initial_state =
          _run_earley_parser ~earley_parser ~grammar ~parse_tm ~input initial_state
          |> fun ((),s) -> s
      end
    
      include Export
    
    end
end

module Actions = struct 
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
end

module Earley_simple = struct 
    (** A simple implementation of Earley parsing datastructures, based on
       {!Earley_base}. A more efficient version would not use {!Prelude.Simple_items}. FIXME *)
    
    open Misc
    open Prelude
    
    
    (** Construct the Earley parsing function.  *)
    module Make(Nt_tm:NT_TM) = struct
      module Internal = struct
    
        module Derived_types = struct
          include Prelude.Simple_items(Nt_tm)
        end
    
        (** Used to instantiate {!module: Earley_base.Make} *)
        module Base_requires = struct
    
          type i_t = int
          type k_t = int
          type j_t = int
    
          type tm = Nt_tm.tm
          type nt = Nt_tm.nt
    
          include Derived_types
    
          let dot_bs_hd nitm = nitm |> dot_bs |> function
            | [] -> None
            | x::xs -> Some x
    
          (* The rest of the code is straightforward *)
    
          module Set_nt_item = Set.Make(struct 
              type t = nt_item 
              let compare : t -> t -> int = Pervasives.compare end)
    
          type ixk = i_t*nt
          module Set_ixk = Set.Make(
            struct type t = ixk let compare : t -> t -> int = Pervasives.compare end)
    
          module Map_nt = Map.Make(
            struct type t = nt let compare : t -> t -> int = Pervasives.compare end)
          type 'a map_nt = 'a Map_nt.t
    
          module Map_int = Map.Make(
            struct type t = int let compare : t -> t -> int = Pervasives.compare end)
          type 'a map_int = 'a Map_int.t
    
          module Map_tm = Map.Make(
            struct type t = tm let compare : t -> t -> int = Pervasives.compare end)
    
          type nt_item_set = Set_nt_item.t
          let elements = Set_nt_item.elements
          let empty_nt_item_set = Set_nt_item.empty
          let nt_item_set_of_list = Set_nt_item.of_list
    
          type bitms_lt_k = (nt_item_set map_nt) map_int
    
          type todo_gt_k = nt_item_set map_int
    
          type bitms_at_k = nt_item_set map_nt
    
          type ixk_done = Set_ixk.t
    
          type ktjs = int list Map_tm.t
    
    
          let todo_gt_k_find i t = 
            Map_int.find_opt i t |> function
            | None -> Set_nt_item.empty
            | Some set -> set
    
    
          let update_bitms_lt_k i at_k lt_k =
            lt_k |> Map_int.add i at_k
    
    
          let empty_todo_gt_k = Map_int.empty
          let empty_bitms_lt_k = Map_int.empty
          let empty_bitms_at_k = Map_nt.empty
          let empty_ixk_done = Set_ixk.empty
          let empty_ktjs = Map_tm.empty
    
        end  (* Base_requires *)
    
        open Base_requires
    
        module Base_impl = Earley_base.Make(Base_requires)
    
        open Base_impl
    
        let pop_todo () s = match s.todo with
          | [] -> None,s
          | x::todo -> Some x,{s with todo; count=s.count+1}
    
        let _or_empty = function
          | None -> Set_nt_item.empty
          | Some x -> x
    
        let get_bitms_at_k nt s = 
          s.bitms_at_k |> Map_nt.find_opt nt |> _or_empty |> elements
          |> fun x -> x,s
    
        let get_bitms_lt_k (i,nt) s =
          s.bitms_lt_k |> Map_int.find_opt i |> (function
              | None -> Set_nt_item.empty
              | Some bitms -> Map_nt.find_opt nt bitms |> _or_empty)
          |> fun x -> elements x,s
    
        let add_bitm_at_k itm nt s =
          s.bitms_at_k |> Map_nt.find_opt nt |> _or_empty
          |> Set_nt_item.add itm |> fun itms ->
          s.bitms_at_k |> Map_nt.add nt itms |> fun bitms_at_k ->
          (),{s with bitms_at_k}
    
        (* FIXME this is the one to optimize! *)
        let add_todos_at_k itms s =
          (itms,s.todo_done,s.todo)
          |> iter_opt (function 
              | [],_,_ -> None
              | itm::itms,todo_done,todo ->
                Set_nt_item.mem itm todo_done |> function
                | true -> Some(itms,todo_done,todo)
                | false -> Some(itms, Set_nt_item.add itm todo_done,itm::todo))
          |> fun ([],todo_done,todo) -> 
          (),{s with todo; todo_done }
    
        let add_todos_gt_k (itms:nt_item list) s =
          let dot_k (x:nt_item) = x.k_ in  (* some problems with usign .k_ and ocaml not distinguishing based on types... *)
          (itms,s.todo_gt_k)
          |> iter_opt (function
              | [],_ -> None
              | itm::itms,todo_gt_k -> 
                Some(itms,
                     todo_gt_k |> Map_int.find_opt (dot_k itm) |> _or_empty 
                     |> Set_nt_item.add itm
                     |> fun itms -> Map_int.add (dot_k itm) itms todo_gt_k))
          |> fun ([],todo_gt_k) -> 
          (),{s with todo_gt_k}
    
        let add_ixk_done (i,nt) s =
          (),{s with ixk_done=s.ixk_done |> Set_ixk.add (i,nt)}
    
        let mem_ixk_done (i,nt) s = 
          (Set_ixk.mem (i,nt) s.ixk_done),s
    
        let find_ktjs tm s = 
          (s.ktjs |> Map_tm.find_opt tm),s
    
        let add_ktjs tm js s =
          s.ktjs |> Map_tm.add tm js |> fun ktjs ->
          (),{s with ktjs}
             
        let run_earley_parser ~grammar ~parse_tm ~input = 
          let at_ops = { get_bitms_at_k; get_bitms_lt_k; add_bitm_at_k; pop_todo;
                         add_todos_at_k; add_todos_gt_k; add_ixk_done;
                         mem_ixk_done; find_ktjs; add_ktjs }
          in
          let earley_parser = make_earley_parser ~at_ops in
          run_earley_parser ~earley_parser ~grammar ~parse_tm ~input
    
        module Export : sig 
          open Nt_tm
          val run_earley_parser: 
            grammar:(nt, tm, 'a) input_dependent_grammar ->
            parse_tm:(tm, 'a) terminal_input_matcher ->
            input:'a input -> 
            initial_nt:nt -> 
            state
        end = struct
          let run_earley_parser ~grammar ~parse_tm ~input ~initial_nt:nt = 
            let initial_state = { empty_state with todo=[{nt;i_=0;k_=0;bs=[Nt nt]}] } in
            run_earley_parser ~grammar ~parse_tm ~input ~initial_state 
        end
    
      end  (* Internal *)
      
      (** NOTE this exposes the internal types for nt_item, but this is
         just an alias for a type known outside Internal, so we can export
         it. This also exposes the state type FIXME? *)
      let run_earley_parser = Internal.Export.run_earley_parser
    end  (* Make *)
    
    
end

module Earley_unstaged = struct 
    
    (** Use {!Earley_spec} to produce an efficient O(n^3) parser. *)
    
    open Prelude
    open Misc
    
    
    (** Construct the parse function. *)
    module Make(Nt_tm:Prelude.NT_TM) = struct
    
      module Internal = struct
    
        
        module Simple_items = Prelude.Simple_items(Nt_tm)
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
          let mark = !unstaged_mark_ref in
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
            { count=s.count;items;complete_items;debug=s.complete2 }
            (* s.todo_done |> Hashtbl.to_seq_keys |> List.of_seq *)
    
      end (* Internal *)
    
      open Nt_tm
      let earley_unstaged : 
    expand_nt:(nt * int -> 'nt_item list) ->
    expand_tm:(tm * int -> int list) -> initial_nt:nt -> ('b,'c,'d)parse_result
        = Internal.earley
    
    end
end

