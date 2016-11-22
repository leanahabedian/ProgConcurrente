
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
  let precio, ep = Session.receive ep in
  let pagosEp, ep = Session.receive ep in
  let pagosEp = Session.send precio pagosEp in
  let pagosEp = Session.send "tarjeta" pagosEp in
  let result, _ = Session.receive pagosEp in
                     
  if result = 0 then
  let ep = Session.select (fun x -> `Deny x) ep in
  pagar ep (* reintenta el pago*)
  else
  let ep = Session.select (fun x -> `Accept x) ep in

  Session.close ep


let abort ep =
  let ep = Session.select(fun x -> `Abort x) ep in
  let msg, ep = Session.receive ep in
  print_endline msg;
  Session.close ep

(* Cliente que envia "times" veces un <producto,cantidad> y recive una respuesta del cliente.*)
(* Luego, intenga pagar hasta que se le accepte el pago *)
let rec op_client ep x cant times =
  let ep = Session.select (fun x -> `Add x) ep in
  let ep = Session.send x ep in
  let ep = Session.send cant ep in
  let result, ep = Session.receive ep in
  if Map.mem x result then print_int (Map.find x result) else print_endline "no_esta";
  if times <> 0 then op_client ep x cant (times - 1) else pagar ep 


(* Este es un carrito que cumple con el funcionamiento del punto 4*)
(* Permite 3 comportamientos: que el cliente agregue un elemento al carrito, pagar o hacer abort*)
(* El stock es una variable global: cantidades*)
(* Se va modificando a traves de la ejecucion*)
(* El kart es el estado actual del carrito del cliente*)
(* Posee 2 endpoints, uno para comunicarse con el servidor de pagos y otro para el cliente*)
let rec carrito kart cantidades clientEp pagosEp =
  match Session.branch clientEp with
   `Add clientEp ->  let x, clientEp = Session.receive clientEp in
               let cant, clientEp = Session.receive clientEp in
               let cantInMap = Map.find x cantidades in
               let esta = ((mem x catalogo) && (cantInMap >= cant)) in
               let clientEp = Session.send kart clientEp in
               let newkart = if (Map.mem x kart) then Map.add x ((Map.find x kart) + cant) kart else Map.add x cant kart in
               if esta then carrito newkart (Map.add x (cantInMap - cant) cantidades) clientEp pagosEp else carrito kart cantidades clientEp pagosEp

  | `Pay clientEp -> Random.self_init(); (*Genero un precio acorde al carrito de productos*)
                    let precio = Random.int 1000 in
                    let clientEp = Session.send precio clientEp in
                    let clientEp = Session.send pagosEp clientEp in
                    match Session.branch clientEp with
                        `Accept clientEp -> Session.close clientEp
                        | `Deny clientEp -> carrito kart cantidades clientEp pagosEp
                   

  | `Abort clientEp -> let clientEp = Session.send "Session aborted" clientEp in
                 Session.close clientEp

(* El servidor de pagos esta todo el tiempo escuchando pedidos*)
(* Cuando recibe uno devuelve aleatoreamente si acceptado o no. Luego sigue esperando*)
let rec pagos ep =
            let _, ep = Session.receive ep in (* recibo el precio*)
            let _, ep = Session.receive ep in (* recibo los datos de la tarjeta*)
            Random.self_init();
            let ans = Random.int 2 in
            print_endline "";
            if ans = 0 then print_string "DENIED" else print_string "ACCEPTED";
            let ep = Session.send ans ep in
            pagos ep



(* Main Function *)
(* Crea el proceso carrito y pagos y los manda a diferentes threads. El cliente corre localmente *)
(* Este cliente de ejemplo solo intenta agregar 5 veces una remera (tengo stock suficiente) *)
(* Genera 4 endpoints que representan las 2 comunicaciones  Cliente <-> Servidor <-> Pagos*)
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let c, d = Session.create () in   (* create session with endpoints c and d *)
  let _ = Thread.create (carrito Map.empty cantidades a) c in (* spawns service in its own thread *)
  let _ = Thread.create (pagos) d in
  op_client b "Remeras" 1 5
