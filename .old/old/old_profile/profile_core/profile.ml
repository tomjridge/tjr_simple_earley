module P = Profile_waypoints

let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let now () = 
  Core.Time_stamp_counter.(
    now () |> to_int63 |> Core.Int63.to_int |> dest_Some)

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



