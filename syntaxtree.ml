
(* identifiers *)
type ide = Ide of string

(* arithmetical expressions *)
type aexp  =
	  N of int
	| R of float

	| Var of ide
        | Rec  of ide * ide
        | RecV of ide * ide * aexp
        | RecM of ide * ide * aexp * aexp
	| Vec of ide * aexp
	| Mat of ide * aexp * aexp

	| Sum of aexp * aexp
	| Sub of aexp * aexp
	| Mul of aexp * aexp
	| Div of aexp * aexp
	| FCall of ide * aexp list

(* boolean expressions *)
type bexp =
	  B of bool
	| Equ of aexp * aexp
	| LE of  aexp * aexp
	| LT of  aexp * aexp

	| GE of  aexp * aexp
	| GT of  aexp * aexp

	| Not of bexp
	| And of bexp * bexp
	| Or of bexp * bexp

(* left expressions *)
type lexp =
	  LVar of ide

        | LRec of ide * lexp

	| LVec of ide * aexp
	| LMat of ide * aexp * aexp


(* commands *)
type cmd =
	  Ass of lexp * aexp
	| Blk of cmd list
	| Ite of bexp * cmd * cmd

(*
NON ANCORA IMPLEMENTATO
        | It  of bexp * cmd   
*)


	| While of bexp * cmd
	| Repeat of cmd * bexp
	| For of ide * aexp * aexp * cmd
	| Write of aexp
	| PCall of ide * aexp list

(* declarations *)
type bType = Int
           | Float

type gType =
	  Basic of bType

        | UserType of ide              (* da matchare con rec_ ... *)

	| Vector of int * bType
	| Matrix of int * int * bType

type dec = Dec of ide * gType

(* record declarations *)

type rec_ = Record of ide * dec list

(* procedures / functions *)

type param = Par of ide * bType

type ret = Ret of bType

type proc =
	  Proc of ide * param list * dec list * cmd
	| Func of ide * param list * ret * dec list * cmd

(* programs *)

type program = Program of rec_ list * dec list * proc list * cmd
