(* profiling ---------------------------------------- *)

let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let now () = 
  Core.Time_stamp_counter.(
    now () |> to_int63 |> Core.Int63.to_int |> dest_Some)

module P = struct  (* timing points *)

  let ab = 1
  let ac = 13
  let bc = 2
  let cd = 3
  let de = 4
  let ef = 5
  let fg = 6
  let gh = 7
  let hi = 8
  let ij = 9
  let jk = 10

  let p2s i =
    match i with
    | _ when i = ab -> "ab"
    | _ when i = ac -> "ac"
    | _ when i = bc -> "bc"
    | _ when i = cd -> "cd"
    | _ when i = de -> "de"
    | _ when i = ef -> "ef"
    | _ when i = fg -> "fg"
    | _ when i = gh -> "gh"
    | _ when i = hi -> "hi"
    | _ when i = ij -> "ij"
    | _ when i = jk -> "jk"
    | _ -> "FIXME"
end

let ts = ref []

let log p = (ts := (p,now())::!ts; true)

open P

let print_logs () =
  let f last prev = 
    let (p2,t2) = last in
    let (p1,t1) = prev in
    let d = t2 - t1 in
    let s = Printf.sprintf "(%s,%s) %d" (p2s p1) (p2s p2) d in
    print_endline s;
    prev
  in
  let _ = List.fold_left f (List.hd !ts) !ts in
  true



