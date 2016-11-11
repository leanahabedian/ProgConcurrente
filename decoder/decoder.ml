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

let rec split_string s =
  try
    let i = String.index s '.' in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s i (String.length s - i) in
    s1 :: split_string s2
  with
    Not_found -> [s]
      
let _ =
  let options =
    [ "-margin", Arg.Int Format.set_margin, "sets the right margin (in characters)";
      "-show-end", Arg.Set Configuration.show_end, "show trailing end";
      "-sequence-polarity", Arg.Set Configuration.sequence_polarity, "show sequence polarity";
      "-prefix", Arg.String (fun s -> Configuration.set_prefix (split_string s)), "set module path (default is Session)";
      "-session-type", Arg.Set_string Configuration.session_type, "sets the session type (default is st)";
    ]
  in
  Arg.parse options (fun _ -> ()) "Usage: rosetta [OPTIONS]";
  try
    let lexbuf = Lexing.from_channel stdin in
    List.iter
      (function
       | Specification.Type t ->
	  Ast.pp (Ast.decode t);
	  print_newline ()
       | Specification.Val (x, t) ->
	  Format.open_box 0;
	  Format.print_string ("val " ^ x ^ " :");
	  Format.print_break 1 2;
	  Ast.pp (Ast.decode t);
	  Format.close_box ();
	  print_newline ()
      )
      (Parser.main Lexer.token lexbuf);
    flush stdout
  with Lexer.UnexpectedCharacter ch ->
    print_endline ("unexpected character " ^ String.make 1 ch);
    exit 0
