# Graph of refinement dependencies


(e_bc e_spec: spec; nt_items only; single set of items
  
  (e_cd e_spec_all_items: single set, NITM CITM SITM
    
    spec_item_t = NTITM of nt_item | CITM of citm_t | SITM of sym_item

    an abstract model, with all types of items; refines e_bc; single set of items; no staging;
    O(2^n); useful because it makes the full set of items explicit

    (e_cn e_simple: a copy of ../simple_earley.ml, but will be changed to avoid tmitem, and integrated with
      e_common.ml)

    
    (e_de??  X
      
      refinement of e_cd, with blocked and complete maps but no staging; simple_earley but with more
      things memoized; but more complicated because more items; O(n^3)

      don't bother!)

    (e_ef??  X
      
      refinement of e_cd, with maps and staging; don't bother - use staged with nt_item only))
  
  
  (e_fg e_staged: staged; blocked, complete; most efficient?)
  (spec.ml: ignore; use e_cd
    
    spec_item_t = CITM of citm_t | PITM of (nt_item|tm_item)

    refinement of e_bc; single set of items; not ideal if we have SITM, since no advantage over e_cd
    
    (simple_earley: blocked, complete; not staged; NTITM TMITM
      
      item = NTITM of nt_item | TMITM of tm_item, but can avoid TMITM by looking at blocked or
      complete FIXME

      state: todo_done; todo; blocked; complete
      
      refinement of spec.ml; **no CITM**, but CITM simulated by looking at complete map; no (k Y)
      item, but again simulated by looking at blocked map; O(n^3)

      could be useful as a non-staged O(n^3) Earley-like parser;  
      )))
