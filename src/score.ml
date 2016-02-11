open Batteries
open Types
open Input_output


let simulate solution data = 
	[] (* TODO: pour l'instant on renvoie rien, plus tard on renvoie que les temps de livraison *)
	


let score solution data = 
	let times = simulate solution data
	in
	
	let mini_score time =  (* score pour une livraison fait en temps time*)
		let deadline = data.deadline
		in
		let numerator = 100. *. float_of_int (deadline - time)
		and denominator = float_of_int (deadline)
		in
		int_of_float (ceil  (numerator /. denominator))
	in
	
	List.fold_left (fun sum time -> sum + (mini_score time)) 0 times
