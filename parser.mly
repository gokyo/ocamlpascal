%{ (* HEADER *)

open Syntaxtree;;

%}

%token <string>   IDE
%token <int>      NUM
%token <float>    REAL

%token            TRUE FALSE

%token            PROGRAM VAR ARRAY OF INT FLOAT PROCEDURE FUNCTION BEGIN END
%token            IF THEN ELSE WHILE DO REPEAT UNTIL FOR TO WRITE CALL

%token            PLUS MINUS TIMES DIVISION EQUAL LESSEQUAL LESS AND OR NOT
%token            ASSIGN

%token            SEMICOLON COLON COMMA

%token            LS RS LP RP




%token            GREATEREQUAL GREATER

%token            RECORD DOT




%token            EOF

%start program
%type<Syntaxtree.program> program

%%

program
    : PROGRAM opt_rec_list opt_dec_list opt_proc_list cmd EOF
                                                  { Program($2,$3,$4,$5) }
    ;

opt_rec_list
    :                                             { [] }
    | rec_list                                    { $1 }
    ;

rec_list
    : rec_                                                 { [$1] }
    | rec_  rec_list                     { $1::$2 }
    ;

opt_dec_list
    :                                             { [] }
    | dec_list                                    { $1 }
    ;

dec_list
    : dec                                                  { [$1] }
    | dec  dec_list                      { $1::$2 }
    ;

rec_
    : RECORD ide BEGIN dec_list END SEMICOLON              { Record($2,$4) }
    ;

dec
    : VAR ide COLON gType SEMICOLON                        { Dec($2,$4) }
    ;


gType
    : bType                                       { Basic($1)     }
    | ide                                         { UserType($1)   }
    | ARRAY LS NUM RS OF bType                    { Vector($3,$6)   }
    | ARRAY LS NUM COMMA NUM RS OF bType          { Matrix($3,$5,$8) }
    ;

bType
    : INT                                         { Int }
    | FLOAT                                       { Float }
    ;

opt_proc_list
    :                                             { [] }
    | proc_list                                   { $1 }
    ;

proc_list
    : proc                                                 { [$1] }
    | proc  proc_list                    { $1::$2 }
    ;

proc
    : PROCEDURE ide LP opt_param_list RP opt_dec_list cmd
	                                          { Proc($2,$4,$6,$7) }
    | FUNCTION ide LP opt_param_list RP COLON ret opt_dec_list cmd 
	                                          { Func($2,$4,$7,$8,$9) }
    ;

opt_param_list
    :                                             { [] }
    | param_list                                  { $1 }
    ;

param_list
    : param                                       { [$1] }
    | param COMMA param_list                      { $1::$3 }
    ;

param
    : ide COLON bType                             { Par($1,$3) }
    ;

ret
    : bType                                       { Ret($1) }
    ;




cmd
    : lexp ASSIGN aexp SEMICOLON                           { Ass($1,$3) }
    | BEGIN opt_cmd_list END SEMICOLON                     { Blk($2) }
    | IF bexp THEN cmd ELSE cmd                   { Ite($2,$4,$6) }
    | WHILE bexp DO cmd                           { While($2,$4) }
    | REPEAT cmd UNTIL bexp SEMICOLON                      { Repeat($2,$4) }
    | FOR ide ASSIGN aexp TO aexp DO cmd          { For($2,$4,$6,$8) }
    | WRITE aexp SEMICOLON                                 { Write($2) }
    | CALL ide LP opt_aexp_list RP SEMICOLON               { PCall($2,$4) }
    ;







opt_cmd_list
    :                                             { [] }
    | cmd_list                                    { $1 }
    ;

cmd_list
    : cmd                                                  { [$1] }


    | cmd  cmd_list                      { $1::$2 }
    ;

lexp
    : ide                                         { LVar($1)   }
    | ide LS aexp RS                              { LVec($1,$3)   }
    | ide LS aexp COMMA aexp RS                   { LMat($1,$3,$5) }

    | ide DOT lexp                                { LRec($1,$3)  }
    ;


bexp_factor
    : TRUE                                        { B(true) }
    | FALSE                                       { B(false) }
    | aexp EQUAL aexp                             { Equ($1,$3) }
    | aexp LESSEQUAL aexp                         { LE($1,$3)  }
    | aexp LESS aexp                              { LT($1,$3)  }

    | aexp GREATEREQUAL aexp                      { GE($1,$3)  }
    | aexp GREATER aexp                           { GT($1,$3)  }

    | NOT bexp_factor                             { Not($2) }
    | LP bexp RP                                  { $2 }
    ;

bexp_term
    : bexp_term AND bexp_factor                   { And($1,$3) }
    | bexp_factor                                 { $1 }
    ;

bexp
    : bexp OR bexp_term                           { Or($1,$3) }
    | bexp_term                                   { $1 }
    ;




aexp_factor
    : NUM                                         { N($1) }
    | REAL                                        { R($1) }

    | ide                                         { Var($1) }
    | ide LS aexp RS                              { Vec($1,$3) }
    | ide LS aexp COMMA aexp RS                   { Mat($1,$3,$5) }
    | ide DOT ide                                 { Rec($1,$3) }
    | ide DOT ide LS aexp RS                      { RecV($1,$3,$5) }
    | ide DOT ide LS aexp COMMA aexp RS           { RecM($1,$3,$5,$7) }


    | CALL ide LP opt_aexp_list RP                { FCall($2,$4) }

    | LP aexp RP                                  { $2 }
    ;

aexp_term
    : aexp_term TIMES aexp_factor                 { Mul($1,$3) }
    | aexp_term DIVISION aexp_factor              { Div($1,$3) }
    | aexp_factor                                 { $1 }
    ;

aexp
    : aexp PLUS aexp_term                         { Sum($1,$3) }
    | aexp MINUS aexp_term                        { Sub($1,$3) }
    | aexp_term                                   { $1 }
    ;

opt_aexp_list
    :                                             { [] }
    | aexp_list                                   { $1 }
    ;

aexp_list
    : aexp                                        { [$1] }
    | aexp COMMA aexp_list                        { $1::$3 }
    ;

ide
    : IDE                                         { Ide($1) }
    ;

%%

(* FOOTER *)

