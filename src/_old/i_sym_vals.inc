val is_nt: sym -> bool
val dest_nt: sym -> nt
val dest_tm: sym -> tm
val _NT: nt -> sym
val _TM: tm -> sym

val syms_nil: sym_list -> bool
val syms_hd: sym_list -> sym
val syms_tl: sym_list -> sym_list
