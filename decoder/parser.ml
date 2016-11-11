type token =
  | LID of (string)
  | CID of (string)
  | OID of (string)
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | TICK
  | UNDERSCORE
  | QMARK
  | COLON
  | ARROW
  | STAR
  | COMMA
  | AS
  | OF
  | AND
  | EXCEPTION
  | MODULE
  | TYPE
  | VAL
  | LT
  | GT
  | EQ
  | ELLIPSIS
  | SEMICOLON
  | SHARP
  | DOT
  | OR
  | BACKTICK
  | EOF
  | SIG
  | END

open Parsing;;
let _ = parse_error;;
# 19 "parser.mly"
				      let of_tuple_type =
					function
					  [t] -> t
					| ts -> Ast.Constructor (`Tuple, ts)
					
  let syntax_error msg pos =
    let msg' = "syntax error: " ^ msg in
      prerr_endline msg';
      assert false

  let expand t0 =
    let defs = Hashtbl.create 16 in
    let rec collect =
      function
      | Ast.Var _ as t -> t
      | Ast.Tagged (ctor, tags) ->
	 Ast.Tagged (ctor, List.map collect_tag tags)
      | Ast.Constructor (`As x, [t]) ->
	 Hashtbl.add defs x (collect t);
	 Ast.Var x
      | Ast.Constructor (`As _, _) -> assert false (* impossible *)
      | Ast.Constructor (ctor, ts) ->
	 Ast.Constructor (ctor, List.map collect ts)
      | _ -> assert false (* impossible *)
    and collect_tag (name, t) = name, collect t
    in
    let rec aux evars =
      function
      | Ast.Var x when List.mem x evars -> Ast.RecVar x
      | Ast.Var x when Hashtbl.mem defs x ->
	 Ast.Rec (x, aux (x :: evars) (Hashtbl.find defs x))
      | Ast.Var _ as t -> t
      | Ast.Tagged (ctor, tags) ->
	 Ast.Tagged (ctor, List.map (aux_tag evars) tags)
      | Ast.Constructor (ctor, ts) ->
	 Ast.Constructor (ctor, List.map (aux evars) ts)
      | _ -> assert false
    and aux_tag evars (name, t) = (name, aux evars t)
    in aux [] (collect t0)

  let cons_opt =
    function
    | None -> fun x -> x
    | Some x -> fun y -> x :: y
# 84 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* TICK *);
  265 (* UNDERSCORE *);
  266 (* QMARK *);
  267 (* COLON *);
  268 (* ARROW *);
  269 (* STAR *);
  270 (* COMMA *);
  271 (* AS *);
  272 (* OF *);
  273 (* AND *);
  274 (* EXCEPTION *);
  275 (* MODULE *);
  276 (* TYPE *);
  277 (* VAL *);
  278 (* LT *);
  279 (* GT *);
  280 (* EQ *);
  281 (* ELLIPSIS *);
  282 (* SEMICOLON *);
  283 (* SHARP *);
  284 (* DOT *);
  285 (* OR *);
  286 (* BACKTICK *);
    0 (* EOF *);
  287 (* SIG *);
  288 (* END *);
    0|]

let yytransl_block = [|
  257 (* LID *);
  258 (* CID *);
  259 (* OID *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\003\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\009\000\009\000\010\000\011\000\011\000\
\011\000\008\000\008\000\007\000\013\000\013\000\012\000\012\000\
\014\000\006\000\015\000\015\000\016\000\016\000\017\000\017\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\020\000\
\022\000\022\000\019\000\019\000\021\000\021\000\021\000\021\000\
\023\000\023\000\002\000\002\000\025\000\024\000\024\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\000\000\002\000\007\000\001\000\004\000\
\002\000\004\000\002\000\001\000\003\000\002\000\000\000\002\000\
\002\000\001\000\003\000\002\000\000\000\002\000\001\000\003\000\
\002\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\004\000\003\000\001\000\002\000\004\000\001\000\002\000\
\000\000\002\000\001\000\003\000\004\000\004\000\006\000\003\000\
\000\000\001\000\001\000\003\000\004\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\056\000\001\000\000\000\007\000\
\033\000\000\000\027\000\000\000\000\000\036\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\025\000\000\000\000\000\009\000\000\000\000\000\011\000\000\000\
\002\000\000\000\000\000\005\000\000\000\000\000\000\000\037\000\
\044\000\035\000\000\000\040\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\028\000\030\000\032\000\000\000\
\042\000\038\000\046\000\000\000\045\000\000\000\052\000\000\000\
\022\000\000\000\000\000\010\000\000\000\016\000\000\000\017\000\
\013\000\003\000\008\000\000\000\000\000\053\000\000\000\000\000\
\000\000\000\000\047\000\000\000\019\000\024\000\055\000\006\000"

let yydgoto = "\002\000\
\013\000\030\000\014\000\043\000\015\000\016\000\087\000\084\000\
\039\000\040\000\065\000\088\000\061\000\017\000\018\000\019\000\
\020\000\021\000\022\000\026\000\023\000\052\000\031\000\093\000\
\032\000"

let yysindex = "\012\000\
\008\255\000\000\000\000\243\254\035\255\048\255\024\255\031\255\
\036\255\038\255\035\255\072\255\000\000\000\000\008\255\000\000\
\000\000\027\255\000\000\032\255\021\255\000\000\000\000\078\255\
\055\255\047\255\020\255\020\255\052\255\000\000\049\255\029\255\
\000\000\053\255\050\255\000\000\000\255\044\255\000\000\057\255\
\000\000\069\255\066\255\000\000\075\255\035\255\035\255\000\000\
\000\000\000\000\035\255\000\000\078\255\253\254\077\255\070\255\
\000\000\020\255\035\255\035\255\000\000\056\255\087\255\045\255\
\000\000\035\255\085\255\035\255\000\000\000\000\000\000\079\255\
\000\000\000\000\000\000\061\255\000\000\035\255\000\000\078\255\
\000\000\008\255\064\255\000\000\003\255\000\000\065\255\000\000\
\000\000\000\000\000\000\093\255\089\255\000\000\067\255\087\255\
\036\255\061\255\000\000\008\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\097\000\000\000\000\000\000\000\000\000\091\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\059\000\000\000\001\000\088\000\000\000\000\000\000\000\
\000\000\000\000\254\254\091\255\000\000\000\000\000\000\010\255\
\000\000\000\000\117\000\000\000\000\000\146\000\000\000\188\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\095\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\000\000\071\255\209\000\000\000\117\000\000\000\167\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\094\255\000\000\007\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\044\000\241\255\000\000\000\000\253\255\095\000\009\000\
\040\000\000\000\000\000\010\000\000\000\063\000\000\000\064\000\
\062\000\052\000\238\255\061\000\000\000\000\000\054\000\015\000\
\000\000"

let yytablesize = 497
let yytable = "\044\000\
\029\000\025\000\048\000\075\000\049\000\049\000\004\000\038\000\
\003\000\004\000\062\000\005\000\001\000\006\000\024\000\007\000\
\051\000\008\000\060\000\076\000\049\000\003\000\004\000\063\000\
\033\000\009\000\010\000\011\000\012\000\034\000\024\000\034\000\
\051\000\047\000\074\000\003\000\004\000\035\000\005\000\037\000\
\006\000\045\000\007\000\046\000\008\000\003\000\085\000\072\000\
\005\000\029\000\006\000\053\000\007\000\056\000\008\000\057\000\
\081\000\058\000\026\000\050\000\086\000\048\000\038\000\059\000\
\091\000\060\000\095\000\064\000\051\000\027\000\028\000\067\000\
\041\000\066\000\094\000\042\000\068\000\029\000\003\000\004\000\
\054\000\055\000\007\000\077\000\104\000\078\000\082\000\031\000\
\083\000\090\000\092\000\096\000\051\000\097\000\098\000\099\000\
\004\000\049\000\100\000\041\000\054\000\079\000\004\000\036\000\
\101\000\089\000\102\000\069\000\071\000\070\000\080\000\073\000\
\103\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\000\000\029\000\029\000\029\000\029\000\
\029\000\000\000\029\000\000\000\000\000\000\000\029\000\029\000\
\000\000\029\000\029\000\029\000\029\000\029\000\000\000\029\000\
\029\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\029\000\034\000\034\000\034\000\034\000\034\000\004\000\034\000\
\000\000\034\000\034\000\034\000\034\000\000\000\034\000\034\000\
\034\000\034\000\034\000\000\000\034\000\034\000\000\000\000\000\
\000\000\000\000\034\000\026\000\026\000\034\000\026\000\026\000\
\026\000\026\000\026\000\000\000\026\000\000\000\000\000\000\000\
\026\000\000\000\000\000\026\000\026\000\026\000\026\000\026\000\
\000\000\026\000\026\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\026\000\031\000\031\000\031\000\031\000\031\000\
\000\000\031\000\000\000\031\000\000\000\031\000\031\000\000\000\
\031\000\031\000\031\000\031\000\031\000\000\000\031\000\031\000\
\000\000\000\000\000\000\000\000\031\000\021\000\021\000\031\000\
\021\000\000\000\021\000\000\000\021\000\000\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\021\000\021\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\015\000\015\000\021\000\015\000\000\000\015\000\
\000\000\015\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\023\000\
\023\000\000\000\023\000\000\000\023\000\000\000\023\000\000\000\
\023\000\015\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\012\000\012\000\000\000\012\000\
\000\000\012\000\000\000\012\000\000\000\012\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\012\000\012\000\
\012\000\018\000\018\000\000\000\018\000\000\000\018\000\000\000\
\018\000\000\000\018\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\018\000\018\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000"

let yycheck = "\015\000\
\000\000\005\000\021\000\007\001\007\001\024\000\000\000\011\000\
\001\001\002\001\011\001\004\001\001\000\006\001\028\001\008\001\
\007\001\010\001\016\001\023\001\023\001\001\001\002\001\024\001\
\001\001\018\001\019\001\020\001\021\001\000\000\028\001\001\001\
\023\001\013\001\053\000\001\001\002\001\002\001\004\001\002\001\
\006\001\015\001\008\001\012\001\010\001\001\001\002\001\051\000\
\004\001\030\001\006\001\005\001\008\001\002\001\010\001\007\001\
\060\000\029\001\000\000\005\001\064\000\080\000\066\000\011\001\
\068\000\016\001\082\000\024\001\014\001\022\001\023\001\003\001\
\001\001\017\001\078\000\004\001\011\001\030\001\001\001\002\001\
\027\000\028\000\008\001\007\001\100\000\016\001\031\001\000\000\
\002\001\005\001\030\001\028\001\014\001\029\001\002\001\007\001\
\000\000\007\001\032\001\005\001\007\001\058\000\032\001\009\000\
\096\000\066\000\097\000\045\000\047\000\046\000\059\000\051\000\
\098\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001\255\255\255\255\255\255\014\001\015\001\
\255\255\017\001\018\001\019\001\020\001\021\001\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\029\001\255\255\255\255\
\032\001\004\001\005\001\006\001\007\001\008\001\032\001\010\001\
\255\255\012\001\013\001\014\001\015\001\255\255\017\001\018\001\
\019\001\020\001\021\001\255\255\023\001\024\001\255\255\255\255\
\255\255\255\255\029\001\001\001\002\001\032\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\255\255\255\255\255\255\
\014\001\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\255\255\023\001\024\001\255\255\255\255\255\255\255\255\029\001\
\255\255\255\255\032\001\004\001\005\001\006\001\007\001\008\001\
\255\255\010\001\255\255\012\001\255\255\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\255\255\023\001\024\001\
\255\255\255\255\255\255\255\255\029\001\001\001\002\001\032\001\
\004\001\255\255\006\001\255\255\008\001\255\255\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\029\001\001\001\002\001\032\001\004\001\255\255\006\001\
\255\255\008\001\255\255\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\001\001\
\002\001\255\255\004\001\255\255\006\001\255\255\008\001\255\255\
\010\001\032\001\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\001\001\002\001\255\255\004\001\
\255\255\006\001\255\255\008\001\255\255\010\001\032\001\255\255\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\001\001\002\001\255\255\004\001\255\255\006\001\255\255\
\008\001\255\255\010\001\032\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  TICK\000\
  UNDERSCORE\000\
  QMARK\000\
  COLON\000\
  ARROW\000\
  STAR\000\
  COMMA\000\
  AS\000\
  OF\000\
  AND\000\
  EXCEPTION\000\
  MODULE\000\
  TYPE\000\
  VAL\000\
  LT\000\
  GT\000\
  EQ\000\
  ELLIPSIS\000\
  SEMICOLON\000\
  SHARP\000\
  DOT\000\
  OR\000\
  BACKTICK\000\
  EOF\000\
  SIG\000\
  END\000\
  "

let yynames_block = "\
  LID\000\
  CID\000\
  OID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'specification_list) in
    Obj.repr(
# 110 "parser.mly"
                     ( _1 )
# 376 "parser.ml"
               : Specification.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
      ( _1 )
# 383 "parser.ml"
               : 'value_name))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 115 "parser.mly"
                    ( _2 )
# 390 "parser.ml"
               : 'value_name))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
  ( [] )
# 396 "parser.ml"
               : 'specification_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'specification) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'specification_list) in
    Obj.repr(
# 120 "parser.mly"
                                   ( cons_opt _1 _2 )
# 404 "parser.ml"
               : 'specification_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'specification_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'specification_list) in
    Obj.repr(
# 121 "parser.mly"
                                                                 ( List.append _5 _7 )
# 413 "parser.ml"
               : 'specification_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 125 "parser.mly"
            ( Some (Specification.Type (expand _1)) )
# 420 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'value_name) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 126 "parser.mly"
                                 ( Some (Specification.Val (_2, expand _4)) )
# 428 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl) in
    Obj.repr(
# 127 "parser.mly"
                        ( None )
# 435 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'module_path) in
    Obj.repr(
# 128 "parser.mly"
                            ( None )
# 443 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition_ne_list) in
    Obj.repr(
# 129 "parser.mly"
                               ( None )
# 450 "parser.ml"
               : 'specification))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition) in
    Obj.repr(
# 133 "parser.mly"
                  ( )
# 457 "parser.ml"
               : 'type_definition_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_definition_ne_list) in
    Obj.repr(
# 134 "parser.mly"
                                              ( )
# 465 "parser.ml"
               : 'type_definition_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_information) in
    Obj.repr(
# 138 "parser.mly"
                             ( )
# 473 "parser.ml"
               : 'type_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
  ( )
# 479 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 143 "parser.mly"
               ( )
# 486 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl_ne_list) in
    Obj.repr(
# 144 "parser.mly"
                         ( )
# 493 "parser.ml"
               : 'type_information))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 148 "parser.mly"
      ( )
# 500 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'module_path) in
    Obj.repr(
# 149 "parser.mly"
                      ( )
# 508 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constr_args_opt) in
    Obj.repr(
# 153 "parser.mly"
                      ( )
# 516 "parser.ml"
               : 'constr_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
  ( )
# 522 "parser.ml"
               : 'constr_args_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 158 "parser.mly"
               ( )
# 529 "parser.ml"
               : 'constr_args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl) in
    Obj.repr(
# 162 "parser.mly"
              ( )
# 536 "parser.ml"
               : 'constr_decl_ne_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'constr_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'constr_decl_ne_list) in
    Obj.repr(
# 163 "parser.mly"
                                     ( )
# 544 "parser.ml"
               : 'constr_decl_ne_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 167 "parser.mly"
           ( _2 )
# 551 "parser.ml"
               : 'type_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'as_type_expr) in
    Obj.repr(
# 171 "parser.mly"
               ( _1 )
# 558 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_type_expr) in
    Obj.repr(
# 175 "parser.mly"
                  ( _1 )
# 565 "parser.ml"
               : 'as_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'as_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 176 "parser.mly"
                           ( Ast.Constructor (`As _3, [_1]) )
# 573 "parser.ml"
               : 'as_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_expr) in
    Obj.repr(
# 180 "parser.mly"
                  ( of_tuple_type _1 )
# 580 "parser.ml"
               : 'arrow_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_type_expr) in
    Obj.repr(
# 182 "parser.mly"
  ( Ast.Constructor (`Arrow, [of_tuple_type _1; _3]) )
# 588 "parser.ml"
               : 'arrow_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_type_expr) in
    Obj.repr(
# 186 "parser.mly"
                   ( [_1] )
# 595 "parser.ml"
               : 'tuple_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_expr) in
    Obj.repr(
# 187 "parser.mly"
                                        ( _1 :: _3 )
# 603 "parser.ml"
               : 'tuple_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_var) in
    Obj.repr(
# 191 "parser.mly"
           ( Ast.Var _1 )
# 610 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_type_expr) in
    Obj.repr(
# 192 "parser.mly"
                                   ( _4 )
# 618 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 193 "parser.mly"
                          ( _2 )
# 625 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 194 "parser.mly"
              ( Ast.Constructor (`Apply _1, []) )
# 632 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 195 "parser.mly"
                               ( Ast.Constructor (`Apply _2, [_1]) )
# 640 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr_comma_ne_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 196 "parser.mly"
                                                    ( Ast.Constructor (`Apply _4, _2) )
# 648 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'polymorphic_variant_type) in
    Obj.repr(
# 197 "parser.mly"
                           ( Ast.Tagged (`Variant, _1) )
# 655 "parser.ml"
               : 'atomic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comma_type_expr_list) in
    Obj.repr(
# 201 "parser.mly"
                                 ( _1 :: _2 )
# 663 "parser.ml"
               : 'type_expr_comma_ne_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "parser.mly"
  ( [] )
# 669 "parser.ml"
               : 'comma_type_expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr_comma_ne_list) in
    Obj.repr(
# 206 "parser.mly"
                                ( _2 )
# 676 "parser.ml"
               : 'comma_type_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 210 "parser.mly"
      ( [_1] )
# 683 "parser.ml"
               : 'type_constr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_constr) in
    Obj.repr(
# 211 "parser.mly"
                      ( _1 :: _3 )
# 691 "parser.ml"
               : 'type_constr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 215 "parser.mly"
                                 ( _3 )
# 698 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 216 "parser.mly"
                                 ( _3 )
# 705 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'tag_spec_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'tag_ne_list) in
    Obj.repr(
# 217 "parser.mly"
                                                ( _3 )
# 713 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tag_spec_list) in
    Obj.repr(
# 218 "parser.mly"
                              ( _2 )
# 720 "parser.ml"
               : 'polymorphic_variant_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 222 "parser.mly"
  ( [] )
# 726 "parser.ml"
               : 'tag_spec_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (string * Ast.t) list) in
    Obj.repr(
# 223 "parser.mly"
                   ( _1 )
# 733 "parser.ml"
               : 'tag_spec_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tag_spec) in
    Obj.repr(
# 227 "parser.mly"
           ( [_1] )
# 740 "parser.ml"
               : (string * Ast.t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tag_spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (string * Ast.t) list) in
    Obj.repr(
# 228 "parser.mly"
                               ( _1 :: _3 )
# 748 "parser.ml"
               : (string * Ast.t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'type_expr) in
    Obj.repr(
# 232 "parser.mly"
                            ( (_2, _4) )
# 756 "parser.ml"
               : 'tag_spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 236 "parser.mly"
               ( )
# 763 "parser.ml"
               : 'tag_ne_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tag_ne_list) in
    Obj.repr(
# 237 "parser.mly"
                           ( )
# 771 "parser.ml"
               : 'tag_ne_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Specification.t list)
;;
