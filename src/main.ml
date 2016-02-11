open Batteries
open Input_output
open Score
open Types

let get_sol data = 
  { sol = Array.init data.nb_drones (fun _ -> []) }

let init_clients data =
  (0--(data.nb_orders - 1)) |> List.of_enum
  
    
let _ =
  let file =
    (try Sys.argv.(1) with _ -> failwith ("Specify a file name, please.")) in
  let d = parse file in
  let sol = get_sol d in
  out_sol file sol


