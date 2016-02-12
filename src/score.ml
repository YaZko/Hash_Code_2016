open Batteries
open Types
open Input_output

let rec group l =
  List.group compare l |> List.map (fun l -> (List.hd l, List.length l))

(* orders.(o) : couples (produit, quantité) manquantes pour la commande o *)
let orders data : ((id_p*int) list) array =
  data.order_type
  |> Array.map (fun o ->
		( o |> Array.to_list |> group))


let sq x = x * x

let dist (r1,c1) (r2,c2) =
  int_of_float (ceil (sqrt (float_of_int ((sq (r1 - r2)) + (sq (c1 - c2))))))

let print_items oc (m : ((id_p*int) list)) =
  m |> List.iter
	 (fun (idp,q) ->
	  Printf.fprintf oc "%d [%d]; " idp q)
	       
let print_missing oc (m : ((id_p*int) list) array) =
  m |> Array.iteri
	 (fun idc l ->
	  Printf.fprintf oc "Client %d : %a\n" idc print_items l
	 )

let command_print oc =
  function
  | Load(idp,q,idw) -> Printf.fprintf oc "Load (%d,%d,%d)" idp q idw
  | Unload(idp,q,idw) -> Printf.fprintf oc "Unload (%d,%d,%d)" idp q idw
  | Deliver(idp,q,idc) -> Printf.fprintf oc "Deliver (%d,%d,%d)" idp q idc
  | Wait w -> Printf.fprintf oc "Wait %d" w


	 
let simulate sol data = 
  let delivery_done = Array.make data.nb_orders None in
  let o = orders data in
  let missing = Array.copy o in
  sol |> Array.iteri
	   (fun id_d comlist ->
	    let t = ref 0 in
	    let pos = ref data.position_warehouse.(0) in
	    comlist |>
	      List.iter
		(function
		    Load (idp,q,idw) ->
		    let distance = dist !pos data.position_warehouse.(idw) in
		    t := !t + distance + 1;
		    pos := data.position_warehouse.(idw);
		  | Unload (idp,q,idw) ->
		     let distance = dist !pos data.position_warehouse.(idw) in
		     t := !t + distance + 1;
		     pos := data.position_warehouse.(idw);
		  | Deliver (idp,q,idc) ->
		     let distance = dist !pos data.order_address.(idc) in
		     t := !t + distance + 1;
		     pos := data.order_address.(idc);
		     missing.(idc) <- List.modify idp (fun m -> m - q) missing.(idc);
		     if List.assoc idp missing.(idc) = 0
		     then missing.(idc) <- List.remove_assoc idp missing.(idc);
		     if missing.(idc) = []
		     then delivery_done.(idc) <- Some (!t - 1);
		  | Wait w -> t := !t + w));
  delivery_done


let score solution data = 
  let times = simulate solution data in
  let deadline = data.deadline in
  let mini_score time = 
    match time with
    | None -> 0 (* score pour une livraison fait en temps time*)
    | Some t ->
       let numerator = float_of_int (100 * (deadline - t)) in
       let denominator = float_of_int (deadline) in
       int_of_float (ceil  (numerator /. denominator))
  in
  Array.fold_left (fun sum time -> sum + (mini_score time)) 0 times
