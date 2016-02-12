# Graph of refinement dependencies


(e_bc "
an abstract model of Earley parsing

nt_items only; single set of items"
      
      (e_cd "
an abstract model, with all types of items; refines e_bc

spec_item_t = NTITM of nt_item | CITM of citm_t | SITM of sym_item

single set of items; no staging"

            (e_de?? "
refinement of e_cd, with blocked and complete maps but no staging

simple_earley but with more things memoized

O(n^3)")

            (e_ef?? "
refinement of e_cd, with maps and staging"))
      
      
      (spec.ml "
refinement of e_bc

spec_item_t = CITM of citm_t | PITM of (nt_item|tm_item)

single set of items

not ideal if we have SITM, since no advantage over e_cd"
               
               (simple_earley "
refinement of spec.ml

item = NTITM of nt_item | TMITM of tm_item

**no CITM**, but CITM simulated by looking at complete map

no (k Y) item, but again simulated by looking at blocked map

state: todo_done; todo; blocked; complete

O(n^3)")))
