
module Session = Session.Bare
module Map = Map.Make(String)
open List

(* catalogo de ejemplo de items con su respectivo stock *)
let catalogo = "Remeras" :: "Pantalones" :: "Camisas" :: []
let cantidades = Map.empty
let cantidades = Map.add "Remeras" 10 cantidades
let cantidades = Map.add "Pantalones" 15 cantidades
let cantidades = Map.add "Camisas" 8 cantidades
let int_of_bool b = if b then 1 else 0


let rec pagar ep = 
  let ep = Session.select (fun x -> `Pay x) ep in
  match Session.branch ep with
    `Accept ep -> Session.close ep
    | `Deny ep -> pagar ep

let abort ep =
  let ep = Session.select(fun x -> `Abort x) ep in
  let msg, ep = Session.receive ep in
  print_endline msg;
  Session.close ep


(* Cliente que envia "times" veces un <producto,cantidad> y recive una respuesta del cliente.*)
(* Luego, intenga pagar hasta que se le accepte el pago *)
let rec op_client ep producto cant times =
  let ep = Session.select (fun x -> `Add x) ep in
  let ep = Session.send producto ep in
  let ep = Session.send cant ep in
  let kart, ep = Session.receive ep in
  if Map.mem producto kart then print_int (Map.find producto kart) else print_endline "no_esta";
  if times <> 0 then op_client ep producto cant (times - 1) else pagar ep


(* Este es un carrito que cumple con el funcionamiento del punto 1*)
(* Permite 3 comportamientos: que el cliente agregue un elemento al carrito, pagar o hacer abort*)
(* El stock es una variable global: cantidades*)
(* Se va modificando a traves de la ejecucion*)
(* El kart es el estado actual del carrito del cliente*)
let rec carrito kart cantidades ep =
  match Session.branch ep with
   `Add ep ->  let producto, ep = Session.receive ep in
               let cant, ep = Session.receive ep in
               let cantInStock = Map.find producto cantidades in
               let hayDisponible = ((mem producto catalogo) && (cantInStock >= cant)) in
               let ep = Session.send kart ep in
               let newkart = if (Map.mem producto kart) then Map.add producto ((Map.find producto kart) + cant) kart else Map.add producto cant kart in
               if hayDisponible then carrito newkart (Map.add producto (cantInStock - cant) cantidades) ep else carrito kart cantidades ep

  | `Pay ep -> Random.self_init();
               let ans = Random.int 2 in
               if ans = 0 then print_endline "DENIED" else print_endline "ACCEPTED";
               if ans = 0 then 
                    let ep = Session.select (fun x -> `Deny x) ep in
                    carrito kart cantidades ep 
                    else
                    let ep = Session.select (fun x -> `Accept x) ep in
                    Session.close ep

  | `Abort ep -> let ep = Session.send "Session aborted" ep in
                 Session.close ep


(* Main Function *)
(* Crea el proceso carrito y lo manda a un thread. El cliente corre localmente *)
(* Este cliente de ejemplo solo intenta agregar 5 veces una remera (tengo stock suficiente) *)
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let _ = Thread.create (carrito Map.empty cantidades) a in (* spawns service in its own thread *)
  op_client b "Remeras" 1 1
  (* SOLO CAMBIAMOS ACA: si times es 1 se finaliza la compra con 1 solo producto de cantidad 1*)
