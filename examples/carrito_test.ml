

module Session = Session.Bare
module Catalog = Map.Make(String)
open List

let catalogo = "Remeras" :: "Pantalones" :: "Camisas" :: []
let cantidades = Catalog.empty
let cantidades = Catalog.add "Remeras" 10 cantidades
let cantidades = Catalog.add "Pantalones" 15 cantidades
let cantidades = Catalog.add "Camisas" 8 cantidades
let int_of_bool b = if b then 1 else 0


let rec op_client ep x cant =
  let ep = Session.send x ep in
  let ep = Session.send cant ep in
  let result, ep = Session.receive ep in
  print_int (int_of_bool result);
  op_client ep x cant

let rec carrito kart cantidades ep=
  let x, ep = Session.receive ep in
  let cant, ep = Session.receive ep in
  let cantInMap = Catalog.find x cantidades in
  let esta = ((mem x catalogo) && (cantInMap >= cant)) in
  let ep = Session.send esta ep in
  let newkart = if (Catalog.mem x kart) then Catalog.add x ((Catalog.find x kart) + cant) kart else Catalog.add x cant kart in
  let _ = if esta then carrito newkart (Catalog.add x (cantInMap - cant) cantidades) ep else carrito kart cantidades ep;;

  
                
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let _ = Thread.create (carrito Catalog.empty cantidades) a in (* spawns service in its own thread *)
  print_int (int_of_bool (op_client b "Remeras" 1))
