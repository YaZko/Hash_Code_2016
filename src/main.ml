open Batteries
open Input_output
open Score
open Types

let get_sol data = 
  { bar = 1 }

let _ =
  let file =
    (try Sys.argv.(1) with _ -> failwith ("Specify a file name, please.")) in
  let d = parse file in
  Printf.printf "%s\n" (print_input d);
  let sol = get_sol d in
  out_sol file sol


