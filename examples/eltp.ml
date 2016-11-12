(* This file is part of FuSe.                                           *)
(*                                                                      *)
(* FuSe is free software: you can redistribute it and/or modify         *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* FuSe is distributed in the hope that it will be useful,              *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with FuSe.  If not, see <http://www.gnu.org/licenses/>.        *)
(*                                                                      *)
(* Copyright 2015-2016 Luca Padovani                                    *)

module Session = Session.Bare
open List

let catalogo = [1]	     
let int_of_bool b = if b then 1 else 0


let op_client ep x =
  let ep = Session.send x ep in
  let result, ep = Session.receive ep in
  Session.close ep;
  result

let carrito kart ep =
  let x, ep = Session.receive ep in
  let esta = (mem x catalogo) in
  let ep = Session.send esta ep in
  print_int (int_of_bool esta);
  Session.close ep
  (*carrito ep (x::kart) else carrito ep kart*)

  
                
let _ =
  let a, b = Session.create () in   (* create session with endpoints a and b *)
  let _ = Thread.create (carrito a) [] in (* spawns service in its own thread *)
  print_int (int_of_bool (op_client b 1))

let _ =
  print_int (int_of_bool (op_client (Service.request (Service.spawn carrito [])) 1))
