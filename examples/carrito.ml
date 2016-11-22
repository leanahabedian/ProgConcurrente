

module Session = Session.Bare
module Catalog = Map.Make(String)
module Ran = Random
open List

let catalogo = "Remeras" :: "Pantalones" :: "Camisas" :: []
let cantidades = Catalog.empty
let cantidades = Catalog.add "Remeras" 10 cantidades
let cantidades = Catalog.add "Pantalones" 15 cantidades
let cantidades = Catalog.add "Camisas" 8 cantidades
let int_of_bool b = if b then 1 else 0


let rec pagar ep = 
  let ep = Session.select (fun x -> `Pay x) ep in
  let ans, ep = Session.receive ep in
  if ans <> 0 then Session.close ep else pagar ep
  (* print_endline "PAGUE BIEN"; *)
  


let rec op_client ep x cant times =
  let ep = Session.select (fun x -> `Add x) ep in
  let ep = Session.send x ep in
  let ep = Session.send cant ep in
  let result, ep = Session.receive ep in
  if Catalog.mem x result then print_int (Catalog.find x result) else print_endline "no_esta";
  if times <> 0 then op_client ep x cant (times - 1) else pagar ep

let rec carrito kart cantidades ep=
  match Session.branch ep with
   `Add ep -> let x, ep = Session.receive ep in
                let cant, ep = Session.receive ep in
                let cantInMap = Catalog.find x cantidades in
                let esta = ((mem x catalogo) && (cantInMap >= cant)) in
                let ep = Session.send kart ep in
                let newkart = if (Catalog.mem x kart) then Catalog.add x ((Catalog.find x kart) + cant) kart else Catalog.add x cant kart in
                if esta then carrito newkart (Catalog.add x (cantInMap - cant) cantidades) ep else carrito kart cantidades ep

  | `Pay ep ->  let ans = Ran.int 1 in
                let ep = Session.send ans ep in
                if ans <> 0 then Session.close ep else carrito kart cantidades ep

  
                
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let _ = Thread.create (carrito Catalog.empty cantidades) a in (* spawns service in its own thread *)
  op_client b "Remeras" 1 5
