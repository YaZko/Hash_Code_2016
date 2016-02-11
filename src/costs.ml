open Batteries
open Input_output
open Types

let drones_needed quantity max_load =
  if quantity mod max_load = 0 then
    quantity / max_load
  else
    1 + (quantity / max_load)

let sum = List.fold_left (+) 0

let compute_order_by_type data =
  let orders = Array.init data.nb_orders
    (fun order -> Array.make data.nb_prod_types 0)
  in
  for order = 0 to data.nb_orders - 1 do
    for i = 0 to data.order_length.(order) do
      let t = data.order_type.(order).(i) in
      orders.(order).(t) <- orders.(order).(t) + 1
    done
  done;
  orders

let orders_cost data orders order_id =
  Array.fold_lefti
    (fun s i elt ->
      s + drones_needed orders.(order_id).(i)
      data.max_load)
    0
    orders.(order_id)

let compute_orders_cost data =
  let orders = compute_order_by_type data in
  Array.init
    data.nb_orders
    (fun order -> orders_cost data orders order)

