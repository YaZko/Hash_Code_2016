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
		     
let naivesol (f: fleet) (data: data) wstock =
  let sol = Array.init data.nb_drones (fun i -> []) in
  data.order_type |>
    Array.iteri
      (fun id_c o ->
       let weight = o |>  Array.fold_left (fun acc pt -> acc +
							   data.weights.(pt)
					  ) 0 in
       if weight >= data.max_load 
       then ()
       else begin
	   let drone = earliest_available_drone f in
	   let list_prod : (id_p*int) list = o |> Array.to_list |> (List.map (fun pt -> (pt,1))) in
	   let ol = ref [] in
	   try begin
	       let rec aux list_prod =
		 if list_prod = [] then ()
		 else
		   let (pt,q) = List.hd list_prod in
		   
		   let w = naive_fetch wstock pt q in
		   ol := Load (pt, q, w) :: !ol;
		   let sw = (salvage_warehouse w (List.tl list_prod) wstock) in
		   ol := !ol @ sw;
		   let list_prod =
	   	     list_prod |>
	   	       List.filter
			 (fun (pt,_) ->
	   		  not (List.mem pt
	   				(List.map (function
	   					      Load(idp,_,_) -> idp
	   					    | _ -> failwith "BLAAA") sw))) in
		   aux list_prod
	   in
	   aux list_prod
	     end
	   with _ -> ();


	   (* list_prod |> List.iter (fun (pt,q) -> *)
	   (*     let w = naive_fetch wstock pt q in *)
	   (*     ol := Load (pt, q, w) :: !ol; *)
	   (* 			  ); *)
	   
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
	   else ()
	 end
      );
  sol
    
    
    
