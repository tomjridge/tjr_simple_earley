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
  
