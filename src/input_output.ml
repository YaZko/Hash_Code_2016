open Batteries
open Types
       
type data =
{
  nb_rows: int;
  nb_columns: int;
  nb_drones: int;
  deadline: int;
  max_load: int;
  nb_prod_types: int;
  weights: int array;
  nb_warehouses: int;
  position_warehouse: (int*int) array;
  (* available_products.(w).(p) : number of items of product type p in warehouse w *)
  available_products: int array array;
  nb_orders: int;
  order_address: (int*int) array;
  order_length: int array;
  (* order_type.(o).(i) : ith "type" of order o
   
   order_length.(o) taille order_type.(o)
   *)
  order_type: int array array;
}


  
let parse file: data =
  let f = BatScanf.Scanning.from_file file in
  Scanf.bscanf
    f "%d %d %d %d %d\n" 
    (fun nb_rows nb_columns nb_drones deadline max_load ->
     Scanf.bscanf
       f "%d\n" 
       (fun nb_prod_types ->
	let weights =
	  Array.init
	    nb_prod_types
	    (fun i ->
	     if i = nb_prod_types - 1
	     then Scanf.bscanf f "%d\n" (fun w -> w)
	     else Scanf.bscanf f "%d " (fun w -> w)) in

	Scanf.bscanf
       f "%d\n" 
       (fun nb_warehouses ->
	let position_warehouse = Array.init nb_warehouses (fun i -> (-1,-1)) in
	let available_products = Array.init nb_warehouses
					    (fun i ->
					     Array.init nb_prod_types (fun i -> -1)) in

	for w = 0 to nb_warehouses - 1 do
	  Scanf.bscanf f "%d %d\n" (fun r c -> position_warehouse.(w) <- (r,c));
	  for pt = 0 to nb_prod_types - 1 do
	    if pt = nb_prod_types - 1 
	    then Scanf.bscanf f "%d\n" (fun nb -> available_products.(w).(pt) <- nb)
	    else Scanf.bscanf f "%d " (fun nb -> available_products.(w).(pt) <- nb)
	  done
	done;
	Scanf.bscanf
	  f "%d\n"
	  (fun nb_orders ->
	   let order_address = Array.init nb_orders (fun i -> (-1,-1)) in
	   let order_length = Array.init nb_orders (fun i -> -1) in
	   let order_type = Array.init nb_orders (fun i ->
						  Array.init 0 (fun i -> -1)) in
	   for o = 0 to nb_orders - 1 do
	     Scanf.bscanf
	       f "%d %d\n"
	       (fun r c ->
		order_address.(o) <- (r,c);
		Scanf.bscanf
		  f "%d\n"
		  (fun len ->
		   order_length.(o) <- len;
		   order_type.(o) <-
		     Array.init
		       len
		       (fun i ->
			if i = len - 1 
			then Scanf.bscanf f "%d\n" (fun t -> t)
			else Scanf.bscanf f "%d " (fun t -> t)
		       )))
	   done;
		
     {
       nb_rows;
       nb_columns;
       nb_drones;
       deadline;
       max_load;
       nb_prod_types;
       weights;
       nb_warehouses;
       position_warehouse;
       available_products;
       nb_orders;
       order_address;
       order_length;
       order_type
     }
    ))))

let print_input {
       nb_rows;
       nb_columns;
       nb_drones;
       deadline;
       max_load;
       nb_prod_types;
       weights;
       nb_warehouses;
       position_warehouse;
       available_products;
       nb_orders;
       order_address;
       order_length;
       order_type
     } =
  let s = ref (Printf.sprintf
	    "%d %d %d %d %d\n%d\n"
	    nb_rows nb_columns nb_drones deadline max_load nb_prod_types) in
  for i = 0 to nb_prod_types - 1 do
    try s := !s^(Printf.sprintf "%d" weights.(i));
	if i <> nb_prod_types - 1 then s:=!s^" "
    with _ -> failwith "error on weights.(i)"
  done;
  s := !s^"\n"^(Printf.sprintf "%d\n" nb_warehouses);
  for w = 0 to nb_warehouses - 1 do
    let (r,c) = try position_warehouse.(w)
		with _ -> failwith "error on pos_ware" in
    s := !s^(Printf.sprintf "%d %d\n" r c);
    for i = 0 to nb_prod_types - 1 do
      try s := !s^(string_of_int available_products.(w).(i));
	  if i <> nb_prod_types - 1 then s:=!s^" "
      with _ -> failwith "error on av_prod"
    done;
    s := !s^"\n";
  done;
  s:=!s^(string_of_int nb_orders)^"\n";
  for o = 0 to nb_orders - 1 do
    let (r,c) = try order_address.(o)
		with _ -> failwith "error on ord_addr" in
    s := !s^(Printf.sprintf "%d %d\n%d\n" r c
			    (try order_length.(o)
			     with _ -> failwith "error on ord_len")
			    );
    for i = 0 to order_length.(o) - 1 do
      try s := !s^(Printf.sprintf "%d" order_type.(o).(i));
	  if i <> order_length.(o) - 1 then s:=!s^" ";
      with _ -> failwith "error on order_type"
    done;
    s := !s^"\n";    
  done;
  !s


   
type sol =
{
  sol : (command list) array;
}


let out_sol file sol =
  (try Unix.mkdir "outputs" 0o755 with _ -> ());
  let (_, file) = String.split file "inputs/" in  
  let (file, _) = String.split file ".in" in  
  let oc = open_out ("outputs/" ^ file ^ ".out") in
  Printf.fprintf oc "%d\n" (Array.fold_left (fun acc com_list -> acc + List.length com_list) 0 sol.sol);
  Array.iteri
    (fun id_d com_list ->
     List.iter (fun com ->
		match com with
		| Load (idp,q,idw) ->
		   Printf.fprintf oc "%d L %d %d %d\n" id_d idw idp q
		| Unload (idp,q,idw) ->
		   Printf.fprintf oc "%d U %d %d %d\n" id_d idw idp q
		| Deliver (idp,q,idc) ->
		   Printf.fprintf oc "%d D %d %d %d\n" id_d idc idp q
		| Wait w ->
		   Printf.fprintf oc "%d W %d\n" id_d w
	       ) com_list
    ) sol.sol;
  close_out oc
