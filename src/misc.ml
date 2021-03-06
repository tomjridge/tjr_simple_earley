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
  struct type t = int let compare: t -> t -> int = Int.compare end)


(** {2 Logging} 

NOTE logging is enabled/disabled by a ppx_optcomp flag.
*)

[%%import "earley_optcomp_config.ml"]

[%%if LOGGING_ENABLED]
let log = fun (x:unit Lazy.t) -> Lazy.force x [@@inline]
[%%else]
let log = fun (x:unit Lazy.t) -> () [@@inline]
[%%endif]



