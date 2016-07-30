type token =
  | IDE of (string)
  | NUM of (int)
  | REAL of (float)
  | TRUE
  | FALSE
  | PROGRAM
  | VAR
  | ARRAY
  | OF
  | INT
  | FLOAT
  | PROCEDURE
  | FUNCTION
  | BEGIN
  | END
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | REPEAT
  | UNTIL
  | FOR
  | TO
  | WRITE
  | CALL
  | PLUS
  | MINUS
  | TIMES
  | DIVISION
  | EQUAL
  | LESSEQUAL
  | LESS
  | AND
  | OR
  | NOT
  | ASSIGN
  | SEMICOLON
  | COLON
  | COMMA
  | LS
  | RS
  | LP
  | RP
  | GREATEREQUAL
  | GREATER
  | RECORD
  | DOT
  | EOF

open Parsing;;
# 1 "parser.mly"
 (* HEADER *)

open Syntaxtree;;

# 59 "parser.ml"
let yytransl_const = [|
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* PROGRAM *);
  263 (* VAR *);
  264 (* ARRAY *);
  265 (* OF *);
  266 (* INT *);
  267 (* FLOAT *);
  268 (* PROCEDURE *);
  269 (* FUNCTION *);
  270 (* BEGIN *);
  271 (* END *);
  272 (* IF *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* DO *);
  277 (* REPEAT *);
  278 (* UNTIL *);
  279 (* FOR *);
  280 (* TO *);
  281 (* WRITE *);
  282 (* CALL *);
  283 (* PLUS *);
  284 (* MINUS *);
  285 (* TIMES *);
  286 (* DIVISION *);
  287 (* EQUAL *);
  288 (* LESSEQUAL *);
  289 (* LESS *);
  290 (* AND *);
  291 (* OR *);
  292 (* NOT *);
  293 (* ASSIGN *);
  294 (* SEMICOLON *);
  295 (* COLON *);
  296 (* COMMA *);
  297 (* LS *);
  298 (* RS *);
  299 (* LP *);
  300 (* RP *);
  301 (* GREATEREQUAL *);
  302 (* GREATER *);
  303 (* RECORD *);
  304 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDE *);
  258 (* NUM *);
  259 (* REAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\006\000\006\000\003\000\003\000\008\000\
\008\000\007\000\009\000\011\000\011\000\011\000\011\000\012\000\
\012\000\004\000\004\000\013\000\013\000\014\000\014\000\015\000\
\015\000\017\000\017\000\018\000\016\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\021\000\021\000\024\000\
\024\000\019\000\019\000\019\000\019\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\026\000\026\000\
\022\000\022\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\028\000\028\000\028\000\020\000\
\020\000\020\000\023\000\023\000\029\000\029\000\010\000\000\000"

let yylen = "\002\000\
\006\000\000\000\001\000\001\000\002\000\000\000\001\000\001\000\
\002\000\006\000\005\000\001\000\001\000\006\000\008\000\001\000\
\001\000\000\000\001\000\001\000\002\000\007\000\009\000\000\000\
\001\000\001\000\003\000\003\000\001\000\004\000\004\000\006\000\
\004\000\005\000\008\000\003\000\006\000\000\000\001\000\001\000\
\002\000\001\000\004\000\006\000\003\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\004\000\006\000\003\000\
\006\000\008\000\005\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\000\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\080\000\000\000\000\000\003\000\000\000\
\079\000\000\000\000\000\000\000\007\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
\016\000\017\000\013\000\000\000\012\000\000\000\000\000\000\000\
\000\000\039\000\059\000\060\000\046\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\056\000\000\000\071\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\010\000\000\000\011\000\000\000\000\000\025\000\
\000\000\000\000\041\000\000\000\000\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\068\000\054\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\069\000\070\000\033\000\000\000\
\000\000\000\000\000\000\076\000\000\000\043\000\030\000\000\000\
\000\000\028\000\000\000\027\000\000\000\000\000\000\000\062\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\029\000\000\000\067\000\000\000\000\000\032\000\
\000\000\078\000\037\000\044\000\000\000\014\000\000\000\063\000\
\000\000\065\000\000\000\000\000\023\000\000\000\035\000\015\000\
\066\000"

let yydgoto = "\002\000\
\004\000\006\000\012\000\020\000\048\000\007\000\008\000\013\000\
\014\000\058\000\044\000\045\000\021\000\022\000\079\000\164\000\
\080\000\081\000\037\000\059\000\049\000\060\000\139\000\050\000\
\061\000\062\000\063\000\064\000\140\000"

let yysindex = "\004\000\
\031\255\000\000\225\254\000\000\064\255\063\255\000\000\225\254\
\000\000\062\255\064\255\160\255\000\000\063\255\000\000\063\255\
\051\255\064\255\064\255\136\000\000\000\160\255\000\000\089\255\
\042\000\074\255\086\255\136\000\030\255\030\255\136\000\064\255\
\046\255\064\255\146\000\232\254\121\255\000\000\098\255\120\255\
\000\000\000\000\000\000\126\255\000\000\064\255\064\255\136\000\
\151\255\000\000\000\000\000\000\000\000\000\000\064\255\030\255\
\030\255\010\255\186\000\249\254\000\000\135\255\000\000\150\255\
\018\255\154\255\144\255\046\255\059\255\141\255\000\000\046\255\
\064\255\046\255\000\000\195\255\000\000\162\255\155\255\000\000\
\171\255\169\255\000\000\176\255\186\255\000\000\179\000\048\255\
\046\255\064\255\046\255\046\255\046\255\046\255\046\255\046\255\
\046\255\136\000\030\255\030\255\046\255\046\255\136\000\030\255\
\046\255\237\254\000\000\046\255\027\255\000\000\127\255\022\255\
\184\255\063\255\064\255\192\255\000\000\046\255\000\000\000\000\
\054\255\196\255\150\255\150\255\179\255\179\255\179\255\179\255\
\179\255\209\255\135\255\000\000\000\000\000\000\000\000\083\255\
\124\255\085\255\197\255\000\000\046\255\000\000\000\000\234\255\
\230\255\000\000\136\000\000\000\184\255\199\255\046\255\000\000\
\046\255\136\000\000\000\046\255\046\255\206\255\250\254\198\255\
\184\255\000\000\000\000\063\255\000\000\017\255\080\255\000\000\
\247\254\000\000\000\000\000\000\238\255\000\000\136\000\000\000\
\046\255\000\000\136\000\184\255\000\000\033\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\073\000\000\000\000\000\122\000\000\000\090\000\
\000\000\000\000\000\000\150\000\000\000\106\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\164\000\000\000\000\000\
\000\000\000\000\000\000\233\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\217\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\211\255\211\255\242\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\158\255\000\000\000\000\000\000\034\000\000\000\099\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\215\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\222\255\000\000\000\000\000\000\000\000\
\000\000\178\000\000\000\000\000\000\000\222\255\000\000\000\000\
\000\000\188\255\218\255\248\255\071\255\133\255\011\000\022\000\
\024\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\223\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\178\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\149\255\000\000\237\255\253\000\000\000\124\000\
\000\000\251\255\000\000\147\255\247\000\000\000\223\000\000\000\
\156\000\000\000\200\000\006\000\000\000\229\255\159\000\226\000\
\202\255\190\000\108\000\133\000\121\000"

let yytablesize = 488
let yytable = "\010\000\
\035\000\086\000\065\000\146\000\001\000\017\000\147\000\091\000\
\092\000\098\000\179\000\066\000\026\000\027\000\036\000\005\000\
\072\000\091\000\092\000\043\000\091\000\092\000\036\000\073\000\
\119\000\036\000\067\000\099\000\070\000\088\000\009\000\051\000\
\052\000\053\000\054\000\172\000\003\000\103\000\069\000\163\000\
\078\000\078\000\036\000\091\000\092\000\132\000\009\000\051\000\
\052\000\085\000\089\000\174\000\099\000\091\000\092\000\055\000\
\175\000\090\000\176\000\091\000\092\000\144\000\087\000\145\000\
\009\000\056\000\141\000\036\000\142\000\011\000\184\000\055\000\
\057\000\106\000\185\000\016\000\136\000\109\000\130\000\111\000\
\091\000\092\000\099\000\135\000\122\000\091\000\092\000\048\000\
\068\000\025\000\048\000\120\000\036\000\151\000\121\000\152\000\
\107\000\036\000\125\000\126\000\127\000\128\000\129\000\039\000\
\048\000\048\000\091\000\092\000\048\000\078\000\137\000\091\000\
\092\000\138\000\048\000\074\000\046\000\099\000\074\000\177\000\
\155\000\178\000\074\000\138\000\157\000\074\000\074\000\162\000\
\047\000\074\000\074\000\074\000\074\000\074\000\168\000\075\000\
\074\000\023\000\074\000\024\000\074\000\036\000\074\000\074\000\
\074\000\071\000\159\000\156\000\036\000\049\000\091\000\092\000\
\049\000\091\000\092\000\181\000\166\000\074\000\167\000\183\000\
\076\000\169\000\138\000\077\000\143\000\084\000\049\000\049\000\
\100\000\036\000\049\000\018\000\019\000\036\000\061\000\104\000\
\049\000\061\000\101\000\102\000\105\000\061\000\182\000\108\000\
\061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
\061\000\041\000\042\000\061\000\112\000\061\000\114\000\061\000\
\113\000\061\000\061\000\061\000\064\000\091\000\092\000\064\000\
\133\000\134\000\115\000\064\000\116\000\117\000\064\000\064\000\
\064\000\064\000\064\000\064\000\064\000\064\000\064\000\123\000\
\124\000\064\000\154\000\064\000\118\000\064\000\149\000\064\000\
\064\000\064\000\072\000\160\000\153\000\072\000\161\000\173\000\
\158\000\072\000\165\000\171\000\072\000\072\000\180\000\038\000\
\072\000\072\000\072\000\072\000\072\000\042\000\024\000\072\000\
\040\000\072\000\026\000\072\000\015\000\072\000\072\000\072\000\
\073\000\075\000\077\000\073\000\038\000\082\000\148\000\073\000\
\110\000\083\000\073\000\073\000\150\000\170\000\073\000\073\000\
\073\000\073\000\073\000\050\000\000\000\073\000\050\000\073\000\
\131\000\073\000\000\000\073\000\073\000\073\000\051\000\000\000\
\052\000\051\000\009\000\052\000\050\000\050\000\000\000\000\000\
\050\000\040\000\058\000\041\000\042\000\058\000\050\000\051\000\
\051\000\052\000\052\000\051\000\057\000\052\000\000\000\057\000\
\000\000\051\000\000\000\052\000\058\000\000\000\000\000\058\000\
\000\000\002\000\000\000\000\000\000\000\058\000\057\000\002\000\
\000\000\057\000\000\000\000\000\002\000\002\000\002\000\057\000\
\002\000\000\000\004\000\002\000\000\000\002\000\000\000\002\000\
\004\000\002\000\002\000\000\000\000\000\004\000\004\000\004\000\
\000\000\004\000\008\000\000\000\004\000\000\000\004\000\000\000\
\004\000\000\000\004\000\004\000\000\000\008\000\008\000\008\000\
\008\000\008\000\006\000\000\000\008\000\000\000\008\000\000\000\
\008\000\000\000\008\000\008\000\000\000\006\000\006\000\006\000\
\009\000\006\000\000\000\000\000\006\000\000\000\006\000\000\000\
\006\000\000\000\006\000\006\000\000\000\028\000\018\000\029\000\
\000\000\000\000\030\000\000\000\031\000\000\000\032\000\000\000\
\033\000\034\000\000\000\018\000\020\000\018\000\000\000\000\000\
\018\000\000\000\018\000\000\000\018\000\000\000\018\000\018\000\
\000\000\020\000\006\000\020\000\000\000\000\000\020\000\000\000\
\020\000\000\000\020\000\000\000\020\000\020\000\000\000\006\000\
\000\000\006\000\000\000\000\000\006\000\000\000\006\000\000\000\
\006\000\000\000\006\000\006\000\000\000\091\000\092\000\000\000\
\000\000\093\000\094\000\095\000\091\000\092\000\000\000\000\000\
\093\000\094\000\095\000\000\000\000\000\000\000\119\000\096\000\
\097\000\000\000\000\000\000\000\000\000\000\000\096\000\097\000"

let yycheck = "\005\000\
\020\000\056\000\030\000\113\000\001\000\011\000\114\000\027\001\
\028\001\017\001\020\001\031\000\018\000\019\000\020\000\047\001\
\041\001\027\001\028\001\025\000\027\001\028\001\028\000\048\001\
\044\001\031\000\032\000\035\001\034\000\057\000\001\001\002\001\
\003\001\004\001\005\001\042\001\006\001\020\001\033\000\149\000\
\046\000\047\000\048\000\027\001\028\001\100\000\001\001\002\001\
\003\001\055\000\041\001\161\000\035\001\027\001\028\001\026\001\
\164\000\048\001\042\001\027\001\028\001\040\001\057\000\042\001\
\001\001\036\001\040\001\073\000\042\001\007\001\180\000\026\001\
\043\001\068\000\042\001\014\001\104\000\072\000\098\000\074\000\
\027\001\028\001\035\001\103\000\090\000\027\001\028\001\017\001\
\043\001\039\001\020\001\044\001\098\000\040\001\089\000\042\001\
\038\001\103\000\093\000\094\000\095\000\096\000\097\000\015\001\
\034\001\035\001\027\001\028\001\038\001\115\000\105\000\027\001\
\028\001\108\000\044\001\017\001\043\001\035\001\020\001\040\001\
\038\001\042\001\024\001\118\000\040\001\027\001\028\001\147\000\
\043\001\031\001\032\001\033\001\034\001\035\001\154\000\038\001\
\038\001\014\000\040\001\016\000\042\001\147\000\044\001\045\001\
\046\001\000\000\141\000\024\001\154\000\017\001\027\001\028\001\
\020\001\027\001\028\001\175\000\151\000\037\001\153\000\179\000\
\041\001\156\000\157\000\038\001\038\001\015\001\034\001\035\001\
\034\001\175\000\038\001\012\001\013\001\179\000\017\001\022\001\
\044\001\020\001\029\001\030\001\037\001\024\001\177\000\043\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\010\001\011\001\038\001\002\001\040\001\044\001\042\001\
\039\001\044\001\045\001\046\001\017\001\027\001\028\001\020\001\
\101\000\102\000\040\001\024\001\044\001\038\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\091\000\
\092\000\038\001\018\001\040\001\043\001\042\001\039\001\044\001\
\045\001\046\001\017\001\002\001\041\001\020\001\009\001\042\001\
\044\001\024\001\044\001\038\001\027\001\028\001\009\001\015\001\
\031\001\032\001\033\001\034\001\035\001\037\001\044\001\038\001\
\015\001\040\001\044\001\042\001\008\000\044\001\045\001\046\001\
\017\001\044\001\044\001\020\001\022\000\047\000\115\000\024\001\
\073\000\048\000\027\001\028\001\118\000\157\000\031\001\032\001\
\033\001\034\001\035\001\017\001\255\255\038\001\020\001\040\001\
\099\000\042\001\255\255\044\001\045\001\046\001\017\001\255\255\
\017\001\020\001\001\001\020\001\034\001\035\001\255\255\255\255\
\038\001\008\001\017\001\010\001\011\001\020\001\044\001\034\001\
\035\001\034\001\035\001\038\001\017\001\038\001\255\255\020\001\
\255\255\044\001\255\255\044\001\035\001\255\255\255\255\038\001\
\255\255\001\001\255\255\255\255\255\255\044\001\035\001\007\001\
\255\255\038\001\255\255\255\255\012\001\013\001\014\001\044\001\
\016\001\255\255\001\001\019\001\255\255\021\001\255\255\023\001\
\007\001\025\001\026\001\255\255\255\255\012\001\013\001\014\001\
\255\255\016\001\001\001\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\255\255\012\001\013\001\014\001\
\015\001\016\001\001\001\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\255\255\012\001\013\001\014\001\
\001\001\016\001\255\255\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\255\255\014\001\001\001\016\001\
\255\255\255\255\019\001\255\255\021\001\255\255\023\001\255\255\
\025\001\026\001\255\255\014\001\001\001\016\001\255\255\255\255\
\019\001\255\255\021\001\255\255\023\001\255\255\025\001\026\001\
\255\255\014\001\001\001\016\001\255\255\255\255\019\001\255\255\
\021\001\255\255\023\001\255\255\025\001\026\001\255\255\014\001\
\255\255\016\001\255\255\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\255\255\027\001\028\001\255\255\
\255\255\031\001\032\001\033\001\027\001\028\001\255\255\255\255\
\031\001\032\001\033\001\255\255\255\255\255\255\044\001\045\001\
\046\001\255\255\255\255\255\255\255\255\255\255\045\001\046\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  PROGRAM\000\
  VAR\000\
  ARRAY\000\
  OF\000\
  INT\000\
  FLOAT\000\
  PROCEDURE\000\
  FUNCTION\000\
  BEGIN\000\
  END\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  REPEAT\000\
  UNTIL\000\
  FOR\000\
  TO\000\
  WRITE\000\
  CALL\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVISION\000\
  EQUAL\000\
  LESSEQUAL\000\
  LESS\000\
  AND\000\
  OR\000\
  NOT\000\
  ASSIGN\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  GREATEREQUAL\000\
  GREATER\000\
  RECORD\000\
  DOT\000\
  EOF\000\
  "

let yynames_block = "\
  IDE\000\
  NUM\000\
  REAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'opt_rec_list) in
    let _3 = (peek_val parser_env 3 : 'opt_dec_list) in
    let _4 = (peek_val parser_env 2 : 'opt_proc_list) in
    let _5 = (peek_val parser_env 1 : 'cmd) in
    Obj.repr(
# 42 "parser.mly"
                                                  ( Program(_2,_3,_4,_5) )
# 421 "parser.ml"
               : Syntaxtree.program))
; (fun parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                                  ( [] )
# 427 "parser.ml"
               : 'opt_rec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rec_list) in
    Obj.repr(
# 47 "parser.mly"
                                                  ( _1 )
# 434 "parser.ml"
               : 'opt_rec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rec_) in
    Obj.repr(
# 51 "parser.mly"
                                                           ( [_1] )
# 441 "parser.ml"
               : 'rec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'rec_) in
    let _2 = (peek_val parser_env 0 : 'rec_list) in
    Obj.repr(
# 52 "parser.mly"
                                         ( _1::_2 )
# 449 "parser.ml"
               : 'rec_list))
; (fun parser_env ->
    Obj.repr(
# 56 "parser.mly"
                                                  ( [] )
# 455 "parser.ml"
               : 'opt_dec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'dec_list) in
    Obj.repr(
# 57 "parser.mly"
                                                  ( _1 )
# 462 "parser.ml"
               : 'opt_dec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'dec) in
    Obj.repr(
# 61 "parser.mly"
                                                           ( [_1] )
# 469 "parser.ml"
               : 'dec_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'dec) in
    let _2 = (peek_val parser_env 0 : 'dec_list) in
    Obj.repr(
# 62 "parser.mly"
                                         ( _1::_2 )
# 477 "parser.ml"
               : 'dec_list))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'ide) in
    let _4 = (peek_val parser_env 2 : 'dec_list) in
    Obj.repr(
# 66 "parser.mly"
                                                           ( Record(_2,_4) )
# 485 "parser.ml"
               : 'rec_))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'ide) in
    let _4 = (peek_val parser_env 1 : 'gType) in
    Obj.repr(
# 70 "parser.mly"
                                                           ( Dec(_2,_4) )
# 493 "parser.ml"
               : 'dec))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'bType) in
    Obj.repr(
# 75 "parser.mly"
                                                  ( Basic(_1)     )
# 500 "parser.ml"
               : 'gType))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'ide) in
    Obj.repr(
# 76 "parser.mly"
                                                  ( UserType(_1)   )
# 507 "parser.ml"
               : 'gType))
; (fun parser_env ->
    let _3 = (peek_val parser_env 3 : int) in
    let _6 = (peek_val parser_env 0 : 'bType) in
    Obj.repr(
# 77 "parser.mly"
                                                  ( Vector(_3,_6)   )
# 515 "parser.ml"
               : 'gType))
; (fun parser_env ->
    let _3 = (peek_val parser_env 5 : int) in
    let _5 = (peek_val parser_env 3 : int) in
    let _8 = (peek_val parser_env 0 : 'bType) in
    Obj.repr(
# 78 "parser.mly"
                                                  ( Matrix(_3,_5,_8) )
# 524 "parser.ml"
               : 'gType))
; (fun parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                                  ( Int )
# 530 "parser.ml"
               : 'bType))
; (fun parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                                  ( Float )
# 536 "parser.ml"
               : 'bType))
; (fun parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                                  ( [] )
# 542 "parser.ml"
               : 'opt_proc_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'proc_list) in
    Obj.repr(
# 88 "parser.mly"
                                                  ( _1 )
# 549 "parser.ml"
               : 'opt_proc_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'proc) in
    Obj.repr(
# 92 "parser.mly"
                                                           ( [_1] )
# 556 "parser.ml"
               : 'proc_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'proc) in
    let _2 = (peek_val parser_env 0 : 'proc_list) in
    Obj.repr(
# 93 "parser.mly"
                                         ( _1::_2 )
# 564 "parser.ml"
               : 'proc_list))
; (fun parser_env ->
    let _2 = (peek_val parser_env 5 : 'ide) in
    let _4 = (peek_val parser_env 3 : 'opt_param_list) in
    let _6 = (peek_val parser_env 1 : 'opt_dec_list) in
    let _7 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 98 "parser.mly"
                                           ( Proc(_2,_4,_6,_7) )
# 574 "parser.ml"
               : 'proc))
; (fun parser_env ->
    let _2 = (peek_val parser_env 7 : 'ide) in
    let _4 = (peek_val parser_env 5 : 'opt_param_list) in
    let _7 = (peek_val parser_env 2 : 'ret) in
    let _8 = (peek_val parser_env 1 : 'opt_dec_list) in
    let _9 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 100 "parser.mly"
                                           ( Func(_2,_4,_7,_8,_9) )
# 585 "parser.ml"
               : 'proc))
; (fun parser_env ->
    Obj.repr(
# 104 "parser.mly"
                                                  ( [] )
# 591 "parser.ml"
               : 'opt_param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'param_list) in
    Obj.repr(
# 105 "parser.mly"
                                                  ( _1 )
# 598 "parser.ml"
               : 'opt_param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'param) in
    Obj.repr(
# 109 "parser.mly"
                                                  ( [_1] )
# 605 "parser.ml"
               : 'param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'param) in
    let _3 = (peek_val parser_env 0 : 'param_list) in
    Obj.repr(
# 110 "parser.mly"
                                                  ( _1::_3 )
# 613 "parser.ml"
               : 'param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'ide) in
    let _3 = (peek_val parser_env 0 : 'bType) in
    Obj.repr(
# 114 "parser.mly"
                                                  ( Par(_1,_3) )
# 621 "parser.ml"
               : 'param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'bType) in
    Obj.repr(
# 118 "parser.mly"
                                                  ( Ret(_1) )
# 628 "parser.ml"
               : 'ret))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'lexp) in
    let _3 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 125 "parser.mly"
                                                           ( Ass(_1,_3) )
# 636 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'opt_cmd_list) in
    Obj.repr(
# 126 "parser.mly"
                                                           ( Blk(_2) )
# 643 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'bexp) in
    let _4 = (peek_val parser_env 2 : 'cmd) in
    let _6 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 127 "parser.mly"
                                                  ( Ite(_2,_4,_6) )
# 652 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'bexp) in
    let _4 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 128 "parser.mly"
                                                  ( While(_2,_4) )
# 660 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'cmd) in
    let _4 = (peek_val parser_env 1 : 'bexp) in
    Obj.repr(
# 129 "parser.mly"
                                                           ( Repeat(_2,_4) )
# 668 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 6 : 'ide) in
    let _4 = (peek_val parser_env 4 : 'aexp) in
    let _6 = (peek_val parser_env 2 : 'aexp) in
    let _8 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 130 "parser.mly"
                                                  ( For(_2,_4,_6,_8) )
# 678 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 131 "parser.mly"
                                                           ( Write(_2) )
# 685 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    let _2 = (peek_val parser_env 4 : 'ide) in
    let _4 = (peek_val parser_env 2 : 'opt_aexp_list) in
    Obj.repr(
# 132 "parser.mly"
                                                           ( PCall(_2,_4) )
# 693 "parser.ml"
               : 'cmd))
; (fun parser_env ->
    Obj.repr(
# 142 "parser.mly"
                                                  ( [] )
# 699 "parser.ml"
               : 'opt_cmd_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'cmd_list) in
    Obj.repr(
# 143 "parser.mly"
                                                  ( _1 )
# 706 "parser.ml"
               : 'opt_cmd_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'cmd) in
    Obj.repr(
# 147 "parser.mly"
                                                           ( [_1] )
# 713 "parser.ml"
               : 'cmd_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'cmd) in
    let _2 = (peek_val parser_env 0 : 'cmd_list) in
    Obj.repr(
# 150 "parser.mly"
                                         ( _1::_2 )
# 721 "parser.ml"
               : 'cmd_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'ide) in
    Obj.repr(
# 154 "parser.mly"
                                                  ( LVar(_1)   )
# 728 "parser.ml"
               : 'lexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'ide) in
    let _3 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 155 "parser.mly"
                                                  ( LVec(_1,_3)   )
# 736 "parser.ml"
               : 'lexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 5 : 'ide) in
    let _3 = (peek_val parser_env 3 : 'aexp) in
    let _5 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 156 "parser.mly"
                                                  ( LMat(_1,_3,_5) )
# 745 "parser.ml"
               : 'lexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'ide) in
    let _3 = (peek_val parser_env 0 : 'lexp) in
    Obj.repr(
# 158 "parser.mly"
                                                  ( LRec(_1,_3)  )
# 753 "parser.ml"
               : 'lexp))
; (fun parser_env ->
    Obj.repr(
# 163 "parser.mly"
                                                  ( B(true) )
# 759 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    Obj.repr(
# 164 "parser.mly"
                                                  ( B(false) )
# 765 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 165 "parser.mly"
                                                  ( Equ(_1,_3) )
# 773 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 166 "parser.mly"
                                                  ( LE(_1,_3)  )
# 781 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 167 "parser.mly"
                                                  ( LT(_1,_3)  )
# 789 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 169 "parser.mly"
                                                  ( GE(_1,_3)  )
# 797 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 170 "parser.mly"
                                                  ( GT(_1,_3)  )
# 805 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'bexp_factor) in
    Obj.repr(
# 172 "parser.mly"
                                                  ( Not(_2) )
# 812 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'bexp) in
    Obj.repr(
# 173 "parser.mly"
                                                  ( _2 )
# 819 "parser.ml"
               : 'bexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'bexp_term) in
    let _3 = (peek_val parser_env 0 : 'bexp_factor) in
    Obj.repr(
# 177 "parser.mly"
                                                  ( And(_1,_3) )
# 827 "parser.ml"
               : 'bexp_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'bexp_factor) in
    Obj.repr(
# 178 "parser.mly"
                                                  ( _1 )
# 834 "parser.ml"
               : 'bexp_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'bexp) in
    let _3 = (peek_val parser_env 0 : 'bexp_term) in
    Obj.repr(
# 182 "parser.mly"
                                                  ( Or(_1,_3) )
# 842 "parser.ml"
               : 'bexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'bexp_term) in
    Obj.repr(
# 183 "parser.mly"
                                                  ( _1 )
# 849 "parser.ml"
               : 'bexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr(
# 190 "parser.mly"
                                                  ( N(_1) )
# 856 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : float) in
    Obj.repr(
# 191 "parser.mly"
                                                  ( R(_1) )
# 863 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'ide) in
    Obj.repr(
# 193 "parser.mly"
                                                  ( Var(_1) )
# 870 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'ide) in
    let _3 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 194 "parser.mly"
                                                  ( Vec(_1,_3) )
# 878 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 5 : 'ide) in
    let _3 = (peek_val parser_env 3 : 'aexp) in
    let _5 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 195 "parser.mly"
                                                  ( Mat(_1,_3,_5) )
# 887 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'ide) in
    let _3 = (peek_val parser_env 0 : 'ide) in
    Obj.repr(
# 196 "parser.mly"
                                                  ( Rec(_1,_3) )
# 895 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 5 : 'ide) in
    let _3 = (peek_val parser_env 3 : 'ide) in
    let _5 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 197 "parser.mly"
                                                  ( RecV(_1,_3,_5) )
# 904 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 7 : 'ide) in
    let _3 = (peek_val parser_env 5 : 'ide) in
    let _5 = (peek_val parser_env 3 : 'aexp) in
    let _7 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 198 "parser.mly"
                                                  ( RecM(_1,_3,_5,_7) )
# 914 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'ide) in
    let _4 = (peek_val parser_env 1 : 'opt_aexp_list) in
    Obj.repr(
# 201 "parser.mly"
                                                  ( FCall(_2,_4) )
# 922 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'aexp) in
    Obj.repr(
# 203 "parser.mly"
                                                  ( _2 )
# 929 "parser.ml"
               : 'aexp_factor))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp_term) in
    let _3 = (peek_val parser_env 0 : 'aexp_factor) in
    Obj.repr(
# 207 "parser.mly"
                                                  ( Mul(_1,_3) )
# 937 "parser.ml"
               : 'aexp_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp_term) in
    let _3 = (peek_val parser_env 0 : 'aexp_factor) in
    Obj.repr(
# 208 "parser.mly"
                                                  ( Div(_1,_3) )
# 945 "parser.ml"
               : 'aexp_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'aexp_factor) in
    Obj.repr(
# 209 "parser.mly"
                                                  ( _1 )
# 952 "parser.ml"
               : 'aexp_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp_term) in
    Obj.repr(
# 213 "parser.mly"
                                                  ( Sum(_1,_3) )
# 960 "parser.ml"
               : 'aexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp_term) in
    Obj.repr(
# 214 "parser.mly"
                                                  ( Sub(_1,_3) )
# 968 "parser.ml"
               : 'aexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'aexp_term) in
    Obj.repr(
# 215 "parser.mly"
                                                  ( _1 )
# 975 "parser.ml"
               : 'aexp))
; (fun parser_env ->
    Obj.repr(
# 219 "parser.mly"
                                                  ( [] )
# 981 "parser.ml"
               : 'opt_aexp_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'aexp_list) in
    Obj.repr(
# 220 "parser.mly"
                                                  ( _1 )
# 988 "parser.ml"
               : 'opt_aexp_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'aexp) in
    Obj.repr(
# 224 "parser.mly"
                                                  ( [_1] )
# 995 "parser.ml"
               : 'aexp_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'aexp) in
    let _3 = (peek_val parser_env 0 : 'aexp_list) in
    Obj.repr(
# 225 "parser.mly"
                                                  ( _1::_3 )
# 1003 "parser.ml"
               : 'aexp_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 229 "parser.mly"
                                                  ( Ide(_1) )
# 1010 "parser.ml"
               : 'ide))
(* Entry program *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Syntaxtree.program)
;;
# 233 "parser.mly"

(* FOOTER *)

# 1039 "parser.ml"
