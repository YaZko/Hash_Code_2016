open Batteries
open Input_output

let get_sol data = 
  { bar = 1 }

let _ =
   let file =
     (try Sys.argv.(1) with _ -> "input") in
   let d = parse file in
   let sol = get_sol d in
   out_sol file sol
		 
(*c'est nous qu'on va gagner*)
