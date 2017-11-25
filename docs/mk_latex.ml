(*
#!/usr/bin/env ocaml

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
#use "topfind";;
#thread;;
#require "omacro";;
*)

(* latex ------------------------------------------------------------ *)

let head = {|
\documentclass{article}
\usepackage{verbatim}
\begin{document}
|}

let foot = {|
\end{document}
|}


(* macros ----------------------------------------------------------- *)

open Omacro

(* given a string and a position (assumed to be just after an opening
   bracket) this scans until it finds the closing bracket; it fails if
   there is no closing bracket - this is an error *)

let scan_to_closing_bracket s i = 
  let rec f ~i ~unclosed = 
    (* print_endline (string_of_int i); *)
    match String.get s i with
    | '(' -> f ~i:(i+1) ~unclosed:(unclosed+1)
    | ')' -> 
      if unclosed = 1 
      then i+1 
      else f ~i:(i+1) ~unclosed:(unclosed-1)
    | _ -> f ~i:(i+1) ~unclosed
  in
  f ~i ~unclosed:1


(* a simple macro looks like: \name(...); the action takes a string
   ..., and processes it fully before returning it - so no more macros
   are executed on the string *)
let simple_macro ~name ~action = 
  let name' = "\\" ^ name ^ "(" in
  {
    name=name';
    action=(fun x -> 
        x |> drop name' |> fun (x,y) ->
        scan_to_closing_bracket y 0 |> fun i ->
        Tjr_string.split_at y i |> fun (yarg,yrest) ->
        (* drop last char which is the close of the bracket *)
        let yarg = String.sub yarg 0 (i-1) in
        (x^(action yarg),yrest))
  }


let title = ref None
let author = ref None
let date = ref None

let section = simple_macro ~name:"section" ~action:(fun s -> "\\section{"^s^"}")

let subsection = 
  simple_macro
    ~name:"subsection" 
    ~action:(fun s -> "\\subsection{"^s^"}")  

let subsubsection = 
  simple_macro 
    ~name:"subsubsection" 
    ~action:(fun s -> "\\subsubsection{"^s^"}")  

let ocaml_include = 
  simple_macro
    ~name:"ocaml_include"
    ~action:(fun s -> 
        Tjr_file.read_file s |> fun s ->
        "\\begin{verbatim}\n" ^ s ^ "\n\\end{verbatim}")

let verbatim = 
  simple_macro
    ~name:"verbatim"
    ~action:(fun s -> 
        "\\begin{verbatim}\n" ^ s ^ "\n\\end{verbatim}")



(* drop from // to the end of the line *)
let comment = 
  {
    name="// ";
    action=(fun (x,y) -> 
        Tjr_string.index y '\n' |> fun i ->
        (x,Tjr_string.drop i y))
  }




let title = 
  simple_macro
    ~name:"title"
    ~action:(fun s -> title:=Some s; "")

let author =
  simple_macro
    ~name:"author"
    ~action:(fun s -> author:=Some s; "")

let date =
  simple_macro
    ~name:"date"
    ~action:(fun s -> date:=Some s; "")


let tweak_latex_math = 
  let action s = 
    Tjr_string.replace_all ~sub:"->" ~rep:"\\rightarrow{}" s
  in
  let name = "$(" in
  {
    name="$(";
    action=(fun x -> 
        x |> drop name |> fun (x,y) ->
        scan_to_closing_bracket y 0 |> fun i ->
        Tjr_string.split_at y i |> fun (yarg,yrest) ->
        (* drop last char which is the close of the bracket *)
        let yarg = String.sub yarg 0 (i-1) in
        let yrest = String.sub yrest 1 (String.length yrest -1) in
        (x^"$"^(action yarg)^"$",yrest))
  }
        


let macros = [
  section;
  subsection;
  subsubsection;
  ocaml_include;
  verbatim;
  comment;
  title; author;date;
  tweak_latex_math
]


let _ = 
  Tjr_file.read_file "tjr_earley.md" |> fun s ->
  run_macros macros s |> print_endline

;;

