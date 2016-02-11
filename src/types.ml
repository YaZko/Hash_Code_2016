type id_p = int
type id_d = int
type id_w = int
type id_c = int
type stock = (id_p * int) array
type ware_stock = (id_w * stock) array
type t = int (* time *)
type pos = int * int
type command =
  | Load of id_p * int * id_w
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
type fleat = (id_d * drone) array
type client = (id_p * int) array
type clients = (id_c * client) list
