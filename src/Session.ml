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
       
type _0
type (+'a, -'b) st = { name          : string;
		       channel       : unit Event.channel;
		       polarity      : int;
		       mutable valid : bool }
type et            = (_0, _0) st
type +'a it        = ('a, _0) st
type -'a ot        = (_0, 'a) st
type (+'a, +'b) seq
type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]
		    
module Bare = struct
  exception InvalidEndpoint
  exception UnusedValidEndpoint
	      
  let check_valid ep = if ep.valid then ep.valid <- false else raise InvalidEndpoint
  let check_same_endpoint ep1 ep2 =
    if ep1.channel != ep2.channel || ep1.polarity != ep2.polarity then raise InvalidEndpoint
  let check_invalid ep = if ep.valid then raise UnusedValidEndpoint
                   
  let fresh ep =
    let ep = { ep with valid = true } in
    Gc.finalise check_invalid ep;
    ep

  (**********************************)
  (*** INITIATION AND TERMINATION ***)
  (**********************************)

  let create ?(name = "channel") () =
    let ch = Event.new_channel () in
    let ep1 = { name = name ^ "⁺"; channel = ch; polarity = +1; valid = true } in
    let ep2 = { name = name ^ "⁻"; channel = ch; polarity = -1; valid = true } in
    (ep1, ep2)

  let close = check_valid

  (****************)
  (*** IDENTITY ***)
  (****************)

  let same_session ep ep' = ep.channel == ep'.channel

  let string_of_endpoint ep = ep.name
				
  (*****************)
  (*** LINEARITY ***)
  (*****************)
				
  let is_valid ep = ep.valid
                      
  let acquire ep =
    check_valid ep;
    fresh ep

  let try_acquire ep =
    if ep.valid then
      begin
	ep.valid <- false;
	Some (fresh ep)
      end
    else
      None

  (*********************************)
  (*** LOW-LEVEL MESSAGE PASSING ***)
  (*********************************)

  let select f ep =
    check_valid ep;
    Event.sync (Event.send ep.channel (Obj.magic f));
    fresh ep

  let accept ep =
    check_valid ep;
    Obj.magic (Event.sync (Event.receive ep.channel)) (fresh ep)
	      
  (***********************************)
  (*** MESSAGE PASSING AND CHOICES ***)
  (***********************************)
	      
  let send x = select (fun ep -> (x, ep))
  let receive = accept
  let select_true ep = select (fun x -> `True x) ep
  let select_false ep = select (fun x -> `False x) ep
  let branch = accept

  (******************************)      
  (*** SEQUENTIAL COMPOSITION ***)
  (******************************)      

  let (@=) scope ep =
    let result, ep' = scope (Obj.magic ep) in
    check_same_endpoint ep ep';
    (result, Obj.magic ep')

  let (@>) scope ep =
    snd ((fun ep -> ((), scope ep)) @= ep)
end
    
module Monadic = struct
  type ('t0, 't1, 'a) t = 't0 -> ('a * 't1)

  let (>>=) m f ep = let x, ep = m ep in f x ep
  let (>>>) m1 m2 = m1 >>= fun _ -> m2
  let return m ep = (m, ep)

  let rec fix f = f (fun ep -> fix f ep)

  let connect ms mc =
    let eps, epc = Bare.create () in
    let _ = Thread.create (fun ep -> let (), ep = ms ep in Bare.close ep) eps in
    let x, epc = mc epc in
    Bare.close epc;
    x
      
  let receive = Bare.receive
  let send x ep = let ep = Bare.send x ep in ((), ep)
  let select_true ep = let ep = Bare.select_true ep in ((), ep)
  let select_false ep = let ep = Bare.select_false ep in ((), ep)
  let branch m1 m2 ep =
    match Bare.branch ep with
    | `True ep -> m1 ep
    | `False ep -> m2 ep
end
