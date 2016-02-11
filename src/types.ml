type id_p = int
type id_d = int
type id_w = int
type id_c = int
type stock = (id_p, int) Hashtbl.t 
type ware_stock = (id_w, stock) Hashtbl.t 
type t = int (* time *)
type pos = int * int
type command =
  | Load of id_p * int * id_w * id_d
  | Deliver of id_p * int * id_c
  | Unload of id_p * int * id_w
  | Wait of int
type state = Idle | Order of t * pos * command
type drone = {
  pos : int * int;
  stock : stock;
  state : state;
  poids : int;
}
type fleat = (id_d, drone) Hashtbl.t
type client = (id_p, int) Hashtbl.t
type clients = (id_c * client) list
