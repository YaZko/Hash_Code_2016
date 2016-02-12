open Batteries
open Input_output
open Score
open Types

let init_clients (data: data) =
  (0--(data.nb_orders - 1)) |> List.of_enum

let earliest_available_drone (f:fleet) : id_d =
  Random.int (Array.length f - 1)
	     
(* f |> Array.mapi (fun i v -> (i,v)) *)
(* |> Array.to_list *)
(* |> List.sort (fun (id1,d1) (id2,d2) -> *)
(* 		match d1.state, d2.state with *)
(* 		  Idle, _ -> -1 *)
(* 		| _, Idle -> -1 *)
(* 		| Order(t1,_,_), Order (t2, _, _) -> compare t1 t2) *)
(* |> List.hd |> fst *)

let rec group l =
  List.group compare l |> List.map (fun l -> (List.hd l, List.length l))
				   
let naive_fetch ware_stock prod q =
  try
    let w = ware_stock |> Array.findi (fun p -> p.(prod) >= q) in
    ware_stock.(w).(prod) <-     ware_stock.(w).(prod) - q;
    w
  with _ -> failwith "unfindable warehouse"

let salvage_warehouse w (list_prod : (id_p * int) list) ware_stock =
  list_prod |>
    List.fold_left (fun (list_ord) (pt,q) ->
		    if ware_stock.(w).(pt) >= q
		    then
		      begin ware_stock.(w).(pt) <- ware_stock.(w).(pt) - q;
			    (Load (pt,q,w) :: list_ord)
		      end
		    else list_ord
		   ) []

let weight_order data o =
  o |>  Array.fold_left (fun acc pt -> acc + data.weights.(pt)) 0

let print_state oc =
  function
    Idle -> Printf.fprintf oc "Idle"
  | Order(t,_,_) -> Printf.fprintf oc "Ready at time %d" t
				   
let print_drone oc id_d {pos; stock; state; poids} =
  Printf.fprintf oc "Drone %d : pos (%d,%d); state : %a" id_d (fst pos) (snd pos)
		 print_state state
		 
let print_fleet oc (f:fleet) =
  f |> Array.iteri
	 (fun id_d (drone:drone) ->
	  print_drone oc id_d drone;
	  Printf.fprintf oc "\n";
	 )

let command_print oc =
  function
    Load(idp,q,idw) -> Printf.fprintf oc "Load (%d,%d,%d)" idp q idw
  | _ -> failwith "bla"

let print_wstock oc wstock =
  wstock |> Array.iteri
	      (fun id_p q ->
	       Printf.fprintf oc "Type %d : quantity %d\n" id_p q
	      )

let print_warehouses oc warehouses =
  warehouses |> Array.iteri
		  (fun w wstock ->
		   Printf.fprintf oc "Warehouse %d\n%a" w print_wstock wstock)

let print_order oc (order: id_p array) =
  Array.print Int.print oc order

let sort_orders data =
  data.order_type |> Array.mapi (fun id_c o -> (id_c,o))
  |> Array.to_list
  |> List.sort (fun (c1,o1) (c2,o2) ->
		compare (Array.length o1) (Array.length o2))
	       
let naivesol (f: fleet) (data: data) wstock =
  let sol = Array.init data.nb_drones (fun i -> []) in
  let nb_gave_up = ref 0 in
  let nb_done = ref 0 in
  (* data.order_type *)
  (sort_orders data) |>
    List.iter
      (fun (id_c,o) ->
       (* Printf.fprintf stdout "Order %d: %a\nFleet:\n%a\nStock:\n%a" *)
       (* 		      id_c *)
       (* 		      print_order o *)
       (* 		      print_fleet f *)
       (* 		      print_warehouses wstock; *)
       if weight_order data o >= data.max_load 
       then (Printf.printf "The weight of order %d is too high (%d > %d)\n" id_c (weight_order data o)
			   (data.max_load))
       else begin
	   let drone = earliest_available_drone f in
	   (* Printf.printf "Earliest available drone is %d\n" drone; *)
	   let list_prod : (id_p*int) list = o |> Array.to_list |> group in
	   let ol = ref [] in
	   try begin
	       let rec aux list_prod =
		 if list_prod = [] then (Printf.printf "list_prod: list is empty: done\n")
		 else
		   let (pt,q) = List.hd list_prod in
		   let w = naive_fetch wstock pt q in
		   (* Printf.printf "Found product type %d in warehouse %d: salvaging...\n" pt w; *)
		   ol := Load (pt, q, w) :: !ol;
		   let sw = (salvage_warehouse w (List.tl list_prod) wstock) in
		   (* Printf.fprintf stdout "Salvaging %d produced : %a\n" w (List.print command_print) sw; *)
		   ol := sw @ !ol;
		   let list_prod =
	   	     (List.tl list_prod) |>
	   	       List.filter
			 (fun (pt',_) ->
			  not (List.mem pt'
	   				(List.map (function
	   					      Load(idp,_,_) -> idp
	   					    | _ -> failwith "BLAAA") sw))) in
		   aux list_prod
	       in
	       aux list_prod;
	       let cur_sol = List.rev !ol @
			       (list_prod |> List.map (fun (pt,q) -> Deliver (pt,q,id_c))) in
	       let (t,ok) = begin match f.(drone).state with
				    Idle -> (0,true)
				  | Order (t,_,_) -> (t, t + List.length cur_sol < data.deadline)
			    end in
	       if ok
	       then
		 begin
		   sol.(drone) <- sol.(drone) @ cur_sol;
		   f.(drone) <- { f.(drone) with
				  pos = data.order_address.(id_c);
				  state = Order (t + List.length cur_sol, data.order_address.(id_c), List.last cur_sol) }
				  
		 end
	       else ();
	       nb_done := !nb_done + 1;		   
	     end
	   with _ -> (
	     (* Printf.printf "Couldn't fetch some product from any warehouse. Giving up.\n"; *)
	     nb_gave_up := !nb_gave_up + 1;
	   );


		     (* list_prod |> List.iter (fun (pt,q) -> *)
		     (*     let w = naive_fetch wstock pt q in *)
		     (*     ol := Load (pt, q, w) :: !ol; *)
		     (* 			  ); *)
		     
		     
	 end
      );
  Printf.printf "Gave up on %d. Succeeded on %d\n" !nb_gave_up !nb_done;
  sol
    
    
    
