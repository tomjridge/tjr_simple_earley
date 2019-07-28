type sym_item = { i:int; sym: sym; j:int }
type sym_at_k = { k:int; sym: sym }
type item = Nt_item of nt_item | Sym_item of sym_item | Sym_at_k of sym_at_k