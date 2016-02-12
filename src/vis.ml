open Batteries
open Types
open Input_output
open Graphics

let vis data =
  let sz = 10 in
  open_graph "";
  resize_window (sz*data.nb_columns) (sz*data.nb_rows);
  set_color red;
  data.position_warehouse |>
    Array.iter (fun (r,c) ->
		fill_rect (c*sz) (r*sz) sz sz
	       );
  set_color green;
  data.order_address |>
    Array.iter (fun (r,c) ->
		fill_rect (c*sz) (r*sz) sz sz
	       );
  ignore (wait_next_event [Key_pressed])
