

module Session = Session.Bare
module MyUsers = Map.Make(String)
open List

let catalogo = ["Remeras","Pantalones","Camisas"]
let cantidades = [10,5,8]
let int_of_bool b = if b then 1 else 0


let rec op_client ep x =
  let ep = Session.send x ep in
  let result, ep = Session.receive ep in
  print_int (int_of_bool result);
  op_client ep x

let rec carrito kart ep =
  let x, ep = Session.receive ep in
  let esta = (mem x catalogo) in
  let ep = Session.send esta ep in
  print_int (int_of_bool esta);
  carrito kart ep
  (*carrito (x::kart) ep else carrito kart ep*)

  
                
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let _ = Thread.create (carrito []) a in (* spawns service in its own thread *)
  print_int (int_of_bool (op_client b 1))
