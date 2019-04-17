type ('nt,'tm,'nt_item,'input) grammar_etc = {
  new_items: nt:'nt -> input:'input -> pos:int -> 'nt_item list;
  parse_tm: tm:'tm -> input:'input -> pos:int -> input_length:int -> int list;
  input:'input;
  input_length:int;
}
