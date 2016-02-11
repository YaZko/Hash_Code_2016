open Batteries
open Input_output
open Score
open Types

let get_sol data = 
  { sol = Array.init data.nb_drones (fun _ -> []) }

let sq x = x * x

let dist (r1,c1) (r2,c2) =
  ceil ((sq (r1 - r2)) + (sq (c1 - c2)))
  
let _ =
  let file =
    (try Sys.argv.(1) with _ -> failwith ("Specify a file name, please.")) in
  let d = parse file in
  let sol = get_sol d in
  out_sol file sol

