(* *)

open Syntaxtree;;


(*  ****************************  *)

(* Ci sono le parti dell'interprete di Igor [versione 1.0 alfa] *)



(*** INTERPRETER DOMAINS ***)



(* memory *)
type loc = Loc of int
type value =
	  ValueInt of int
	| ValueFloat of float

type store = loc -> value

(* environment *)
type eval =

     Eval of loc
   | Descr_vect of loc * int * int
   | Descr_matr of loc * int * int * int * int

   | Descr_tpe_rec of (ide * gType * int) list
   | Descr_rec of loc * (ide * gType * int) list       (* start + lista(nome, tipo, offset) *)

   | Descr_proc of param list * dec list * cmd
   | Descr_func of loc * param list * dec list * ret * cmd



type envf = ide -> eval
type env = Env of envf * env | Null


(* exceptions *)

exception NO_MEM
exception NO_IDE
exception SYNTAX
exception INDEX_OUT_OF_BOUND
exception DIFFERENT_TYPE_OPERATION
exception PARAMETERS_DO_NOT_MATCH
exception DIFFERENT_TYPE_RETURNED
exception CALL_NOT_SUPPORTED

(******************************)

(* THE INTERPRETER *)

(* Type utility operations *)

let valueToInt (e:value) =
   match e with
        ValueInt(a) -> a
      | ValueFloat(a) -> truncate a

let valueToFloat (e:value) =
   match e with
     ValueInt(a) -> float a
   | ValueFloat(a) ->  a

let ideToString (e:ide) =
   match e with Ide(a) -> a

let locToInt (e:loc) =
   match e with Loc(a) -> a


(* utility functions *)

let initenv (x:ide): eval = raise NO_IDE

let updateenv ((e:envf), ide, (v:eval)): envf = fun
	y -> if (y = ide) then v else e(y)






(* from Igor *)


let rec buildFieldListDescr((fields: dec list),(descr: (ide * gType * int) list), (offset:int)) =
     match fields with
      [] -> descr
    | f :: lista -> (match f with
                        Dec(id, Basic(tpe)) -> ( buildFieldListDescr(lista,(id,Basic(tpe),offset)::descr, offset+1))
                      | Dec(id, Vector(dim, tpe)) -> buildFieldListDescr(lista,(id,Vector(dim, tpe),offset)::descr, offset+dim)
                      | Dec(id, Matrix(rows, cols, tpe)) -> buildFieldListDescr(lista,(id,Matrix(rows, cols, tpe),offset)::descr, offset+(rows*cols))
                      | _ -> raise SYNTAX
                    )
let rec search_offset field fl=
                                match fl with
                                 [] -> (raise SYNTAX)
                               | (nfield, tpe, offset)::resto -> if(field=nfield)
                                                                 then offset
                                                                 else search_offset field resto
let rec search_field_descr field fl =
                                 match fl with
                                  [] -> raise SYNTAX
                                | (nfield, tpe, offset)::resto -> if(field=nfield)
                                                                  then (nfield, tpe, offset)
                                                                  else search_field_descr field resto













let initmem (x:loc): value = raise NO_MEM

let newmem (s: store): loc =
	let rec aux n =
		try (s(Loc(n)); aux(n+1))
		with NO_MEM -> Loc(n)
	in aux 0

let updatemem ((s:store), addr, (v:value)): store = fun
	x -> if (x = addr) then v else s(x)

let rec updatemem_vector((s:store), addr, (dim:int), (v:value)): store =
   match dim with
        1 -> updatemem(s,addr,v)
      | z -> let news = updatemem(s,addr,v) in
             updatemem_vector(news,newmem news,dim-1,v)

let rec updatemem_matrix((s:store),addr,(nrows:int),(ncols:int),(v:value)):store =
   match nrows with
        1 -> updatemem_vector(s,addr,ncols,v)
      | x -> let news = updatemem_vector(s,addr,ncols,v) in
             updatemem_matrix(news,newmem news,nrows-1,ncols,v)













(* Igor utility functions *)

let rec updatemem_record((s:store), (addr: loc), (fields: (ide * gType * int) list) ):store =
       match fields with
       [] -> s
     | f::lista -> (match f with
                    (id, Basic(y), offset)->(match y with Int->(let news=updatemem(s, addr, ValueInt(0)) in
                                                            updatemem_record(news,(newmem news),lista)
                                                            )
                                         |Float->(let news=updatemem(s, addr, ValueFloat(0.0)) in
                                                  updatemem_record(news,newmem news,lista)
                                                 )
                                        )
                  | (id, Vector(dim, tpe), offset)->(match tpe with
                                                  Int -> (let news=updatemem_vector(s, addr, dim, ValueInt(0)) in
                                                          updatemem_record(news,newmem news,lista)
                                                         )
                                                  |Float -> (let news=updatemem_vector(s, addr, dim, ValueFloat(0.0)) in
                                                             updatemem_record(news,newmem news,lista)
                                                            )
                                                )
                  | (id, Matrix(rows, cols, tpe), offset)->(match tpe with
                                                         Int -> (let news=updatemem_matrix(s, addr, rows, cols, ValueInt(0)) in
                                                                 updatemem_record(news,newmem news,lista)
                                                                )
                                                        |Float -> (let news=updatemem_matrix(s, addr, rows, cols, ValueFloat(0.0)) in
                                                                   updatemem_record(news,newmem news,lista)
                                                                  )
                                                )
                  | _ -> raise SYNTAX
                    )





















(* evaluation of declarations *)

let rec dec_eval (d:dec list) ((e:envf), (s: store)) =
	match d with

	     [] -> (e,s)

  	     | Dec(x, Basic(y))::r ->
			let newaddr = newmem s in
				(match y with
					  Int -> dec_eval r
									(updateenv(e,x,Eval(newaddr)),
									updatemem(s,newaddr,ValueInt(0)))
             | Float -> dec_eval r (updateenv(e,x,Eval(newaddr)),
                                       updatemem(s, newaddr,ValueFloat(0.0))))

             | Dec(x, Vector(dim, tpe))::r ->
                   let newaddr = newmem s in
                        (match tpe with
                                Int -> dec_eval r(updateenv(e,x,Descr_vect(newaddr,dim,1)),
                                                  updatemem_vector(s,newaddr,dim,ValueInt(0)))
                                | Float -> dec_eval r(updateenv(e,x,Descr_vect(newaddr,dim,1)),
                                                  updatemem_vector(s,newaddr,dim,ValueFloat(0.0))))

             | Dec(x, Matrix(nrows,ncols,tpe))::r ->
                   let newaddr = newmem s in
                        (match tpe with
                                Int -> dec_eval r(updateenv(e,x,Descr_matr(newaddr,nrows,ncols,ncols,1)),
                                                  updatemem_matrix(s,newaddr,nrows,ncols,ValueInt(0)))
                                | Float -> dec_eval r(updateenv(e,x,Descr_matr(newaddr,nrows,ncols,ncols,1)),
                                                  updatemem_matrix(s,newaddr,nrows,ncols,ValueFloat(0.0))))



(* from Igor *)


          (*
              Questo costruttore definisce un nuovo tipo di dato di tipo record,
              la cui definizione viene mantenuta nell'enviroment
          *)

          (*
             SECONDO ME NON SERVE

             | Record(x, fields)::r ->
                       (dec_eval r (updateenv(e, x, Descr_tpe_rec(buildFieldListDescr(fields, [], 0))), s)
                       )
           *)


           (*
              Questo costruttore cerca la definizione del tipo definito dall'utente
              nell'enviroment e applica la struttura alla variabile dichiarata
           *)
             | Dec(x, UserType(tpe))::r -> let descr_record = e(tpe) in
                                             (match descr_record with

                                               (* fields è di tipo "(ide * gType * int) list" *)

                                               Descr_tpe_rec(fields) -> let newaddr = newmem s in
                                                                          (dec_eval r(updateenv(e, x,Descr_rec(newaddr, fields)),
                                                                                       updatemem_record(s, newaddr, fields)))
                                             | _ -> raise SYNTAX

             | _ -> raise SYNTAX
                                             )

















(* evaluation of arithmetical expressions *)


let rec eval_aexp (e:aexp) (r:envf) (s:store) : value =
	match e with
    	    N(n) -> ValueInt(n)
          | R(n) -> ValueFloat(n)
          | Var(i) -> (match r(i) with
		   	   Eval(l) -> s(l)
			 | _ -> raise SYNTAX)


(* from Igor *)
(*
   funziona solo con le variabili di tipo bType. Estendere alle gType
   richiede altri costruttori
   ... da finire ...
*)

          | Rec(id, field) -> (match r(id) with
                          Descr_rec(addr, f_list) ->(s(Loc(locToInt(addr)+ search_offset field f_list)))
                        | _ -> raise SYNTAX
                         )
          | RecV(id, id_vect, exp) ->(match r(id) with
                                   Descr_rec(addr, f_list) -> (let pos = valueToInt(eval_aexp exp r s)
                                                              and v_descr = (search_field_descr id_vect f_list) in
                                                                match v_descr with
                                                                  (name, Vector(dim, tpe), offset) -> if (pos>=0 && pos<dim)
                                                                                                then s(Loc(locToInt(addr)+(search_offset id_vect f_list)+pos))
                                                                                                else (raise INDEX_OUT_OF_BOUND
                                                                                                      )
                                                             )
                                  |_->(print_string "la variabile non è un record\n";
                                       raise SYNTAX
                                      )
                                  )
          | RecM(id, id_mat, row, col) ->(match r(id) with
                                      Descr_rec(addr, f_list) ->( let pos_row=valueToInt(eval_aexp row r s )  and
                                                                      pos_col=valueToInt(eval_aexp col r s ) and
                                                                      m_descr = (search_field_descr id_mat f_list) in
                                                                      match m_descr with
                                                                        (name, Matrix(row_dim, col_dim, tpe), offset) ->if ((pos_row >= 0) && (pos_row < row_dim)
                                                                                                                       && (pos_col >= 0) && (pos_col < col_dim)
                                                                                                                     )
                                                                                                                  then s(Loc(locToInt(addr)+ offset + (pos_row*col_dim)+ pos_col))
                                                                                                                  else ( raise INDEX_OUT_OF_BOUND )
                                                                )
                                     )





          | Vec(i,exp) ->
               let pos = valueToInt(eval_aexp exp r s) in
                  (match r(i) with
                     Descr_vect(vo,ub,mol) ->
                        if (pos>=0 && pos<ub) then
                          s(Loc(locToInt(vo)+pos*mol))
                        else
                          raise INDEX_OUT_OF_BOUND
                   | _ -> raise SYNTAX)

          | Mat(i,exp1,exp2) ->
              let pos1 = valueToInt(eval_aexp exp1 r s) and
                  pos2 = valueToInt(eval_aexp exp2 r s) in
                    (match r(i) with
                       Descr_matr(vo,ub1,ub2,mol1,mol2) ->
                          if (pos1>=0 && pos2>=0 && pos1<ub1 && pos2<ub2) then
                            s(Loc(locToInt(vo)+(pos1*mol1)+(pos2*mol2)))
                          else
                            raise INDEX_OUT_OF_BOUND
                     | _ -> raise SYNTAX)

	  | Sum (a,b) ->
		let aValue = (eval_aexp a r s) and
                    bValue = (eval_aexp b r s) in
				(match aValue with
					  ValueInt(op1) ->
					  	(match bValue with
							  ValueInt(op2) -> ValueInt(op1 + op2))
					| ValueFloat(op1) ->
						(match bValue with
							  ValueFloat(op2) ->
							  	ValueFloat(op1 +. op2)))

	  | Sub (a,b) ->
			let aValue = (eval_aexp a r s) and
			    bValue = (eval_aexp b r s) in
				(match aValue with
					  ValueInt(op1) ->
					  	(match bValue with
							  ValueInt(op2) -> ValueInt(op1 - op2))
					| ValueFloat(op1) ->
						(match bValue with
							  ValueFloat(op2) ->
							  	ValueFloat(op1 -. op2)))
	  | Mul (a,b)->
			let aValue = (eval_aexp a r s) and
			    bValue = (eval_aexp b r s) in
				(match aValue with
					  ValueInt(op1) ->
					  	(match bValue with
							  ValueInt(op2) -> ValueInt(op1 * op2))
					| ValueFloat(op1) ->
						(match bValue with
							  ValueFloat(op2) ->
							  	ValueFloat(op1 *. op2)))

	  | Div (a,b) ->
			let aValue = (eval_aexp a r s) and
			    bValue = (eval_aexp b r s) in
				(match aValue with
					  ValueInt(op1) ->
					  	(match bValue with
							  ValueInt(op2) -> ValueInt(op1 / op2))
					| ValueFloat(op1) ->
						(match bValue with
							  ValueFloat(op2) ->
							  	ValueFloat(op1 /. op2)))

          | FCall (i,inValue) -> raise CALL_NOT_SUPPORTED



(* evaluation of boolean expressions *)

let rec eval_bexp (e:bexp) (r:envf) (s:store) =
	match e with
		  B(b) ->	b
		| And (a,b) -> ((eval_bexp a r s ) && (eval_bexp b r s ))
		| Or  (a,b) -> ((eval_bexp a r s ) || (eval_bexp b r s ))
		| Equ (a,b) -> ((eval_aexp a r s )  = (eval_aexp b r s ))
		| LE  (a,b) -> ((eval_aexp a r s ) <= (eval_aexp b r s ))
		| LT  (a,b) -> ((eval_aexp a r s )  < (eval_aexp b r s ))
		| Not (a) -> (not(eval_bexp a r s ))


(* evaluation of procedures *)

let rec proc_eval (f:proc list) ((e:envf), (s: store)) =
   match f with
        [] -> (e,s)

      | Proc(x,inVars,locVars,cmds)::r ->
         proc_eval r
         (updateenv(e,x,Descr_proc(inVars,locVars,cmds)),s)

      | Func(x,inVars,outVar,locVars,cmds)::r ->
         let newaddr =
            newmem s
         in
            let linkRet outVar =
               (match outVar with
                    Ret(Int) ->
                     updatemem(s,newaddr,ValueInt(0))
                  | Ret(Float) ->
                     updatemem(s,newaddr,ValueFloat(0.0)))
            in
               proc_eval r
               (updateenv(e,x, Descr_func(newaddr,inVars,locVars,outVar,cmds)),
               (linkRet outVar))



(* evaluation of commands *)

let rec resolAexp (e:aexp list) (r:envf) (s:store) =
   match e with
        []  -> []
      | esp::vl ->
         [eval_aexp esp r s]@(resolAexp vl r s)


let typeChecking (inValue:value list) (inVars:param list) =
   let rec control (inValue:value list) (inVars:param list) :bool =
      match (inValue,inVars) with
           [],[] -> true
         | (id)::l,Par(i,t)::r ->
            (match id with
                 ValueInt(x) ->
                  if (t=Int) then
                     true&&(control l r)
                  else false
               | ValueFloat(x) ->
                  if (t=Float) then
                     true&&(control l r)
                  else false)
         | _ -> raise SYNTAX
   in
      if (List.length(inValue)!= List.length(inVars))
         then false
      else
         (control inValue inVars)

let rec updateVars (inVars:param list) (inValue:value list)
((r:envf), (s: store)) =
   match (inVars,inValue) with
        [],[] -> (r,s)
      | Par(idLoc,tpe)::vs,assVal::ve ->
         let (r,s) =
            updateVars vs ve (dec_eval [Dec(idLoc,Basic(tpe))] (r,s))
         in
            (match r(idLoc) with
                 Eval(l) -> (r,updatemem(s,l,assVal))
               | _ -> raise SYNTAX)
      | _ -> raise SYNTAX


let rec exec (c: cmd) (Env(r,r_list), (s: store)) =
   match c with
        Ass(i,e) ->
         let control e =
            (match e with
                 FCall (i,inValue) ->
                  let inValue =
                     (resolAexp inValue r s)
                  in
                     (match (r(i)) with
                          Descr_func(l,inVars,_,_,_) ->
                           if ((typeChecking inValue inVars)) then
                              (let s =
                                 (exec_proc (r(i)) inValue
                                 (Env(r,r_list),s))
                              in
                                 (s,s(l)))
                           else
                              raise PARAMETERS_DO_NOT_MATCH
                        | _ -> raise SYNTAX)
               | _ ->
                  (s,(eval_aexp e r s)))
            in
               let (s,retval) =
                  control e
               in
                  (match i with
                       LVar(id) ->
                        (match r(id) with
                             Eval(l) ->
                              updatemem(s,l,retval)
                           | Descr_func(l,_,_,_,_) ->
                              updatemem(s,l,retval)
                           | _ ->
                              raise SYNTAX)
                     | LVec(id,esp) ->
                        let pos =
                           valueToInt((eval_aexp esp r s))
                        in
                           (match r(id) with
                                Descr_vect(id,dim,mol) ->
                                 if (pos>=dim) then
                                    raise INDEX_OUT_OF_BOUND
                                 else
                                    updatemem(s,Loc(locToInt(id)+pos),retval)
                              | _ ->
                                 raise SYNTAX)
                     | LMat(id,esp1,esp2) ->
                        let rig =
                           valueToInt((eval_aexp esp1 r s))
                        and col =
                           valueToInt((eval_aexp esp2 r s))
                        in
                           (match r(id) with
                                Descr_matr(id,dimRig,dimCol,mol1,mol2) ->
                                 if (rig>=dimRig || col>=dimCol) then
                                    raise INDEX_OUT_OF_BOUND
                                 else
                                  updatemem(s,Loc(locToInt(id)+
                                  (rig*mol1)+(col*mol2)),retval)
                     | _   ->
                        raise SYNTAX))
      | Blk([]) ->
         s
      | Blk(x::y) ->
         (exec (Blk(y)) (Env(r,r_list),(exec x (Env(r,r_list),s))))
      | Ite(b,c1,c2) ->
         if (eval_bexp b r s) then
            (exec c1 (Env(r,r_list),s))
         else
            (exec c2 (Env(r,r_list),s))
      | While(b,c) ->
         if (not(eval_bexp b r s)) then
            s
         else
            (exec (While(b,c)) (Env(r,r_list),(exec c (Env(r,r_list),s))))
      | Repeat(c,b) ->
         let news =
            exec c (Env(r,r_list),s)
         in
            if (eval_bexp b r news) then
               news
            else
               (exec (Repeat(c,b)) (Env(r,r_list),news))
      | For(i,valmin_exp,valmax_exp,c) ->
		   let valmin = eval_aexp valmin_exp r s
		   and valmax = eval_aexp valmax_exp r s in
			   (match (r(i)) with
                 Eval(l) ->
                   let s1 = updatemem(s,l,valmin) in
						   let rec f(valv,s) =
							   match (valv,valmax) with
								   (ValueInt(v),ValueInt(max)) ->
									   if (v > max) then
										   s
									   else
										   f(ValueInt(v+1),
										   (updatemem(exec c (Env(r,r_list),s),l,
                                 ValueInt(v+1))))
						   in
							   f(valmin, s1))

      | Write(e) ->
         let aValue =
            (eval_aexp e r s) in
               (match aValue with
                    ValueInt(op1) ->
                      print_int(op1); print_string "\n";s
                  | ValueFloat(op1) ->
                      print_float(op1); print_string "\n";s)
      | PCall(i,inValue) ->
         let inValue =
            (resolAexp inValue r s)
         in
            (match (r(i)) with
                 Descr_proc(inVars,locVars,cmds) ->
                  if ((typeChecking inValue inVars)) then
                     (let s =
                        (exec_proc (r(i)) inValue (Env(r,r_list),s))
                     in s)
                  else
                     raise PARAMETERS_DO_NOT_MATCH
               | _ -> raise SYNTAX)

and exec_proc (ev:eval) (li:value list) (Env(r,r_list), (s: store)) :store =
   let sequence inVars locVars cmds (li:value list) (Env(r,r_list),
   (s: store)): store =
      let (rext,s) =
         updateVars inVars li (r,s)
      in
         let (rext,s) =
            dec_eval locVars (rext,s)
         in
            exec cmds (Env(rext,Env(r,r_list)),s)
   in
      match ev with
           Descr_proc(inVars,locVars,cmds) ->
            sequence inVars locVars cmds li (Env(r,r_list),s)
         | Descr_func(l,inVars,locVars,outVar,cmds) ->
            sequence inVars locVars cmds li (Env(r,r_list),s)
         | _ ->
            raise SYNTAX



(* evaluation of programs *)

(* DA SISTEMARE I RECORDS ............ *)






let run (Program(recs, var, proc, com)) =
   let (r1,s1) =
      proc_eval proc (dec_eval var (initenv,initmem))
   in
      (exec com (Env(r1,Null),s1))