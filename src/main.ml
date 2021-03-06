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

(* (\** Compute a whole solution from an input d **\) *)
(* let solution1 (d: data): sol = *)
(*   let rec aux (drones: fleet) (todo: clients) = *)
(*     match todo with *)
(*     | [] -> () *)
(*     | client::todo -> *)
(*        let s = solve d client drones  *)
(*        in aux drones todo *)
(*   in *)
(*   aux drones (init_clients d) *)

let init_warehouse (data: data) =
  data.available_products

let _ =
  let d = parse "inputs/ex.in" in
  let sol = Array.init d.nb_drones
  		       (fun id_d ->
  			match id_d with
  			  0 -> Load (0,1,0)::Load(1,1,0)::Deliver(0,1,0)
  			       :: Load (2,1,1) :: Deliver (2,1,0)::[]
  			| 1 -> Load (2,1,1) :: Deliver (2,1,2)::
  				 Load (0,1,0):: Deliver (0,1,1)::[]
  			| _ -> []
  		       )
  in
  assert (score sol (parse "inputs/ex.in") = 194);
  let file =
    (try Sys.argv.(1) with _ -> failwith ("Specify a file name, please.")) in
  let d = parse file in
  let sol = Pwsol.naivesol (init_fleet d) d (init_warehouse d) in
  let sc = score sol d in
  Printf.fprintf stdout "Score: %d\n" sc;
  out_sol file {sol}

