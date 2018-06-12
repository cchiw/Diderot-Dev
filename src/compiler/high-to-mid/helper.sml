(* expand-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* handle for evaluating fem
* and checking inside a fem field
*)

structure Helper : sig

    val argsToString : MidIR.var list -> string
    val paramToString : int * Ein.param_kind -> string    
    val multipleRhsToString: string * (MidIR.var * MidIR.rhs) list -> string   
    val bodyToString: string * Ein.ein_exp * MidIR.var list  -> string
    val einToString : string * MidIR.var * Ein.ein * MidIR.var list -> string    
    val rhsToString : MidIR.var * MidIR.rhs -> string 
    val getRHSEIN : MidIR.var -> Ein.ein * MidIR.var list
    val getRHSMesh : MidIR.var -> MidOps.mesh
    val getRHSElement : MidIR.var ->  MidOps.element
    val getRHSDegree : MidIR.var ->  string
    val getRHSLiteral : MidIR.var ->  string

    val iterP: Ein.ein_exp list -> Ein.ein_exp
    val iterA: Ein.ein_exp list  -> Ein.ein_exp
    
  end = struct

    structure SrcIR = HighIR
    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure GVar = IR.GlobalVar
    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR

    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s

    (* ------------------------------------ cvt to strings  --------------------------------------------  *)    
    (* parameters *)   	
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
  	  | paramToString (i, E.FLD (d, shp)) = concat["F", i2s i, "(", i2s d, ")[", shp2s shp , "]"]
      | paramToString (i, E.KRN) = "H" ^ i2s i
      | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]
      | paramToString (i, E.SEQ (n, shp)) =    concat["Sequence[", i2s n, "]{", shp2s shp , "]"]
      
    (* RHS *)
    fun rhsToString (x, rhs) = concat [IR.Var.toString x ,"=", IR.RHS.toString rhs]
    fun multipleRhsToString(name, rhss) = concat [name, String.concatWith "\n\t" (List.map rhsToString rhss)]
    fun einToString(name, y, ein, args) = concat["\n\n", name, rhsToString (y, IR.EINAPP(ein, args))]
    fun ll ([], cnt) = ""
      | ll (a1::args, cnt) = 
      	String.concat["\n\t", Int.toString(cnt), "_", MidTypes.toString(DstIR.Var.ty a1), " ",rhsToString (a1, IR.Var.getDef a1), ", ", ll(args, cnt+1)]
    fun argsToString es = ll(es,0)
    fun bodyToString(name, body, args) = concat["\n\n", name, "::", EinPP.expToString(body), "-",  argsToString args]
    
	 (* ------------------------------------ get RHS --------------------------------------------  *)
	 (* expects RHS to be an EIN APP *)
	fun getRHSEIN x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => (ein, args)
        | rhs => raise Fail(concat["\n\nexpected EIN operator but found ", rhsToString (x, rhs)])
        (* end case *))
    (*expects a mesh*)
    fun getRHSMesh x = (case IR.Var.getDef x
	 	of IR.OP(Op.BuildSpace, [m, e, d, _])  => (getRHSMesh m)
		| IR.OP(Op.BuildSpace, [m, e, d])  => (getRHSMesh m) 
        | IR.OP(Op.BuildMesh m, _)          =>  m
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x, 
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    (* expectes an element *)
    fun getRHSElement x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d, _]) => getRHSElement e
		| IR.OP(Op.BuildSpace, [m, e, d]) => getRHSElement e 
        | IR.OP(Op.BuildElement e, _)      => e
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x, 
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    (* expectes a degree *)
    fun getRHSDegree x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d, _]) => getRHSDegree d
	    | IR.OP(Op.BuildSpace, [m, e, d]) => getRHSDegree d
        | d => (IR.RHS.toString d)
		(* end case *))
	(* expects a literal *)
    fun getRHSLiteral x = (case IR.Var.getDef x
        of  IR.LIT(Literal.String s) => s
        | rhs => raise Fail(concat[
       	 "expected rhs operator for ", IR.Var.toString x, 
        	" but found ", IR.RHS.toString rhs
        ])
        (* end case *))      
        
     (* ------------------------------------ cvt to strings  --------------------------------------------  *)    
       
    (* variables *)
    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt

    (* ------------------------------------ rewriting --------------------------------------------  *)
    fun iterP es =  let 
		fun iterPP([], [r]) = r
		| iterPP ([], rest) = E.Opn(E.Prod, rest)
		| iterPP (E.Const 0::es, rest) = E.Const(0)
		| iterPP (E.Const 1::es, rest) = iterPP(es, rest)
		| iterPP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) = 
			(* variable can't be 0 and 1 '*)
			if(c1 = c2 orelse (not (v1 = v2)))
			then iterPP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
			else  E.Const(0)
		| iterPP(E.Opn(E.Prod, ys)::es, rest) = iterPP(ys@es, rest)
		| iterPP (e1::es, rest)   = iterPP(es, e1::rest)
	in iterPP(es, []) end
	fun iterA es =  let
		fun iterAA([], []) = E.Const 0
		| iterAA([], [r]) = r
		| iterAA ([], rest) = E.Opn(E.Add, rest)
		| iterAA (E.Const 0::es, rest) = iterAA(es, rest)
		| iterAA (E.Opn(E.Add, ys)::es, rest) = iterAA(ys@es, rest)
		| iterAA (e1::es, rest)   = iterAA(es, e1::rest)
	in iterAA(es, []) end	
	        
end
