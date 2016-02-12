open Batteries
open Input_output
open Score
open Types

let sq x = x * x

let dist (r1,c1) (r2,c2) =
  int_of_float (ceil (sqrt (float_of_int ((sq (r1 - r2)) + (sq (c1 - c2))))))
	       
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

let print_wstock oc wstock =
  wstock |> Array.iteri
	      (fun id_p q ->
	       Printf.fprintf oc "Type %d : quantity %d\n" id_p q
	      )

let print_warehouses oc warehouses =
  warehouses |> Array.iteri
		  (fun w wstock ->
		   Printf.fprintf oc "Warehouse %d\n%a" w print_wstock wstock)

let print_pt oc (elt: id_p*int) =
  Printf.fprintf oc "%d [%d]" (fst elt) (snd elt)
		 
let print_order oc (order: (id_p*int) list) =
  List.print print_pt oc order

let init_clients (data: data) =
  (0--(data.nb_orders - 1)) |> List.of_enum

let earliest_available_drone (f:fleet) : (id_d*int) =
  let d = f |> Array.mapi (fun i v -> (i,v))
	  |> Array.to_list
	  |> List.map (fun (id,d) ->
		       match d.state with
			 Idle -> (id,0)
		       | Order (t,_,_) -> (id,t))
	  |> List.sort (fun d1 d2 -> compare (snd d1) (snd d2))
	  |> List.hd
  in d

let rec group l =
  List.group compare l |> List.map (fun l -> (List.hd l, List.length l))
				   
let naive_fetch data drone_pos ware_stock prod q =
  let dist_w w =
    dist (data.position_warehouse.(w)) drone_pos
  in
  let lwh = (0 -- (Array.length ware_stock - 1))
	    |> List.of_enum
	    |> List.sort (fun w1 w2 -> compare (dist_w w1) (dist_w w2))
  in
  lwh |>
    List.fold_left
      (fun found w ->
       match found with
	 Some v -> Some v
       | None ->
	  if ware_stock.(w).(prod) >= q
	  then (    ware_stock.(w).(prod) <- ware_stock.(w).(prod) - q; Some w)
	  else None
      ) None

let salvage_warehouse data w (list_prod : (id_p * int) list) ware_stock cur_weight =
  let ret_lp = ref [] in
  let order_list =
    list_prod |>
      List.fold_left
	(fun list_ord (pt,q) ->
	 let nb = min ((data.max_load - !cur_weight) / data.weights.(pt)) q in
	 if ware_stock.(w).(pt) >= nb && nb > 0
	 then
	   begin ware_stock.(w).(pt) <- ware_stock.(w).(pt) - nb;
		 cur_weight := !cur_weight + nb * data.weights.(pt);
		 if nb <> q then ret_lp := (pt,q-nb)::!ret_lp;
		 (Load (pt,nb,w) :: list_ord)
	   end
	 else (ret_lp := (pt,q)::!ret_lp; list_ord)
	) []
  in (order_list, !ret_lp)

let weight_order data (o : (id_p*int) list) =
  o |> List.fold_left (fun acc (pt,q) -> acc + q*data.weights.(pt)) 0

module OrdM =
  struct
    type t = id_c*((id_p*int) list)
    let compare a b = compare (List.length (snd a),fst a) (List.length (snd b),fst b)
  end
    
module OSet = Set.Make (OrdM)

let sort_orders' data : (id_c * (id_p * int) list) list =
  data.order_type
  |> Array.mapi (fun idc o -> (idc, o |> Array.to_list |> group))
  |> Array.to_list

let closest_drone f pos =
  f
  |> Array.mapi (fun id_d drone -> (id_d, dist pos drone.pos))
  |> Array.to_list
  |> List.sort (fun a b -> compare (snd a) (snd b))
  |> List.map fst
  |> List.hd
       
let naivesol (f: fleet) (data: data) wstock =
  (* the solution being built *)
  let sol = Array.init data.nb_drones (fun i -> []) in
  (* the remaining orders to deliver: orders are lists of pairs (idp,q) : q
  items of product type idp *)
  let todo = ref (OSet.of_list (sort_orders' data)) in
  while not (OSet.is_empty !todo) do
    (* we get an order to process *)
    let ((id_c,list_prod),todo') = OSet.pop_min !todo in
    todo := todo';
    (* we select the next available drone and record its available time *)
    let (drone,time_available) = earliest_available_drone f in
    let cur_weight = ref 0 in
    let loads = ref [] in
    begin
	let rec aux list_prod =
	  let old_weight = ref !cur_weight in
	  if list_prod = [] then ()
	  else
	    let (pt,q) = List.hd list_prod in
	    let nb = min q ((data.max_load - !cur_weight) / data.weights.(pt)) in
	    let list_prod = if nb = q then List.tl list_prod
			    else List.tl list_prod@[(pt,q-nb)] in
	    if nb = 0 then (todo := OSet.add (id_c, list_prod) !todo)
	    else 
	      begin
		let w = match naive_fetch data f.(drone).pos wstock pt nb with
		    Some w -> w
		  | None -> -1
		in
		loads := Load (pt, nb, w) :: !loads;
		cur_weight := !cur_weight + nb * data.weights.(pt);
		let (sw,list_prod) = salvage_warehouse data w list_prod wstock cur_weight in
		loads := sw @ !loads;
		if !cur_weight = !old_weight
		then todo := OSet.add (id_c,list_prod) !todo
		else aux list_prod
	      end
	in
	aux list_prod;
	let dl = !loads
		 |> List.filter_map (function Load(pt,nb,w) -> Some (pt,nb)
					    | _ -> None)
		 |> List.group (fun (pt1,q1) (pt2,q2) -> compare pt1 pt2)
		 |> List.map (List.reduce (fun (pt1,q1) (pt2, q2) -> (pt1,q1+q2)))
		 |> List.map (fun (pt,q) -> Deliver (pt,q,id_c)) in
	let cur_sol = List.rev !loads @ dl in
	let time' = time_available
		    + List.length cur_sol
		    + dist (f.(drone).pos) data.order_address.(id_c) in
	if time' < data.deadline
	then
	  begin
	    sol.(drone) <- sol.(drone) @ cur_sol;
	    f.(drone) <- { f.(drone) with
			   pos = data.order_address.(id_c);
			   state = Order (time',
					  data.order_address.(id_c),
					  List.last cur_sol) }
	  end
	else (Printf.printf "Delivering order %d would take too much time. Aborting\n" id_c);
      end
  done;
  sol
    
    
    
