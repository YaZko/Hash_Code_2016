open Batteries
open Types
open Input_output


let simulate sol data = 
	let delivery_done = Array.make data.nb_orders None
	in
	
	
	delivery_done


let score solution data = 
	let times = simulate solution data
	in
	let deadline = data.deadline
	in
	
	let mini_score time = 
		match time with
			|None -> 0 (* score pour une livraison fait en temps time*)
			|Some t -> let numerator = 100. *. float_of_int (deadline - t)
					   and denominator = float_of_int (deadline)
					   in
					   int_of_float (ceil  (numerator /. denominator))
	in
	
	Array.fold_left (fun sum time -> sum + (mini_score time)) 0 times
