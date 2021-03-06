open Batteries
open Input_output
open Types

let drones_needed quantity max_load =
  if quantity mod max_load = 0 then
    quantity / max_load
  else
    1 + (quantity / max_load)

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

(** returns order -> number of drones needed *)
let compute_orders_cost data =
  let orders = compute_order_by_type data in
  Array.init
    data.nb_orders
    (fun order -> orders_cost data orders order)

(** sorts orders by number of drones needed *)
let better_orders_order data =
  let orders_cost = compute_orders_cost data in
  let l = Array.fold_lefti
    (fun l i o -> (o, orders_cost.(o)) :: l)
    [] orders_cost
  in
  List.sort (fun o1 o2 -> compare (snd o2) (snd o1)) l

exception Exit of int

(** renvoie w disponible et fleet, w = -1 si pas de w *)
let compute_storage data drone id_p quantity fleet wstock =
  try begin for w = 0 to Array.length wstock - 1 do
    if data.available_products.(w).(id_p) >= quantity then
      raise (Exit w)
  done;
  -1 (* aucun *)
  end
  with
  | Exit w ->
      wstock.(w).(id_p) <- wstock.(w).(id_p) - quantity;
      fleet.stock.(id_p) <- fleet.stock.(id_p) - quantity;
      w

