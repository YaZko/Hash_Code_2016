open Batteries
open Input_output
open Score
open Types

let get_sol data = 
  { sol = Array.init data.nb_drones (fun _ -> []) }

let init_clients data =
  (0--(data.nb_orders - 1)) |> List.of_enum
  
let sq x = x * x

let dist (r1,c1) (r2,c2) =
 ceil (sqrt (float_of_int ((sq (r1 - r2)) + (sq (c1 - c2)))))

let init_fleet (d: data): fleet =
  let pos = d.position_warehouse.(0) 
  in  
  Array.make d.nb_drones
	     (let drone =
		{ pos = pos ; 
		  stock = Array.make d.nb_prod_types 0 ;
		  state = Idle ;
		  poids = 0
		} in 
	      drone)
			  

(** Given a single client, a current state of the fleet and the current time, compute a sequence of orders fullfiling the client **)
let solve (d:data) (c: client) (f: fleet) (s: sol): sol =
  (* let nb_d = drones_needed d. *)
  s

(** Compute a whole solution from an input d **)
let solution1 (d: data): sol =
  let rec aux (drones: fleet) (todo: clients) =
    match todo with
    | [] -> ()
    | client::todo ->
       let s = solve d client drones 
       in aux drones todo
  in
  aux drones (init_clients d)
         
let _ =
  let file =
    (try Sys.argv.(1) with _ -> failwith ("Specify a file name, please.")) in
  let d = parse file in
  let sol = get_sol d in
  out_sol file sol

