open Batteries

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
  (* order_type.(o).(i) : ith item of order o *)
  order_type: int array array;
}

type sol =
{
  bar: int
}

let parse file: data =
  { foo = 0 }

let out_sol file sol =
  (try Unix.mkdir "outputs" 0o755 with _ -> ());
  let (_, file) = String.split file "inputs/" in  
  let (file, _) = String.split file ".in" in  
  let oc = open_out ("outputs/" ^ file ^ ".out") in
  close_out oc
