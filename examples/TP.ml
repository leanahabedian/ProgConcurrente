
module Session = Session.Bare
open List
open Thread

let catalogo = [1]

let rec carrito ep kart =
    let req, ep = Session.receive ep in
        if (mem req catalogo) then
            carrito ep (req::kart)
        else
            carrito ep kart
            


let client ep =
    let ep = Session.send 1 ep in
    Session.close ep

let _ =
    let a, b = Session.create () in
    let _ = create (carrito a) [] in
    let _ = create client b in ()

(*
let _ =
    print_int (client (Service.request (Service.spawn carrito)))
*)
