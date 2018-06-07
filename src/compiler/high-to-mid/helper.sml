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

    val getRHSEINSrc: HighIR.var -> string
    val getRHSEIN :  MidIR.var -> Ein.ein * MidIR.var list
    val getRHSArgs : MidIR.var -> MidIR.var list
    val getRHSMesh :  MidIR.var -> MidOps.mesh
    val getRHSElement :  MidIR.var ->  MidOps.element
    val getRHSDegree :  MidIR.var ->  string
    val getRHSS :  MidIR.var ->  string
    val ll : MidIR.var list * int -> string
    val paramToString : int * Ein.param_kind -> string
    val prntNewbies: (MidIR.var * MidIR.rhs) list * string -> 'a list
    val toStringBA: string * Ein.ein_exp * MidIR.var list  -> string
    val line : string * MidIR.var * Ein.ein * MidIR.var list -> string
    val iterP: Ein.ein_exp list -> Ein.ein_exp
    val iterA: Ein.ein_exp list  -> Ein.ein_exp
    val pullPosition: Ein.ein * MidIR.var list ->(MidIR.var * MidIR.rhs) list  * Ein.ein * MidIR.var list
    
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

	 (* ------------------------------------ cvt to strings  --------------------------------------------  *)
    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt
    fun ll ([], cnt) = ""
      | ll (a1::args, cnt) = 
      	String.concat["\n\t", Int.toString(cnt), "_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1), ", ", ll(args, cnt+1)]
    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
  	  | paramToString (i, E.FLD (d, shp)) = concat["F", i2s i, "(", i2s d, ")[", shp2s shp , "]"]
      | paramToString (i, E.KRN) = "H" ^ i2s i
      | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]
      | paramToString (i, E.SEQ (n, shp)) =    concat["Sequence[", i2s n, "]{", shp2s shp , "]"]
     fun prntNewbies(newbies, id) = let 
        val _ = (id)
        val _ =   List.map (fn (lhs, DstIR.EINAPP(e, a)) => (concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs), " ", DstIR.Var.name(lhs), " = ", EinPP.toString(e) , "-", ll(a, 0), "---->"])
		     | (lhs, rhs) => (concat["\n\n -->:", MidTypes.toString(DstIR.Var.ty lhs), DstIR.Var.name(lhs), " = ", DstIR.RHS.toString rhs])
            ) newbies
        in [] end
     fun line(name, y, ein, args) = String.concat[name, ":", MidIR.Var.name(y), " = ", EinPP.toString(ein), "-", ll(args, 0)]
     fun toStringBA(name, e, args) = (String.concat["\n\n", name, ": body:", EinPP.expToString(e), " args#:", Int.toString(length(args)), "\n\n"])
	 (* ------------------------------------ get RHS --------------------------------------------  *)
    fun getRHSEINSrc x = (case SrcIR.Var.getDef x
        of rhs => (concat["\n\n lhs ", SrcIR.Var.toString x, " rhs ", SrcIR.RHS.toString rhs];"")
        (* end case *))
    fun getRHSEIN x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => (ein, args)
        | IR.LIT l => raise Fail(concat["\n\nLIT expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.LIT l)])
        | IR.OP l => raise Fail(concat["\n\nOP expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.OP l)])
        | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.CONS l)])
        | IR.GLOBAL l => raise Fail(concat["\n\nglobal expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.GLOBAL l)])
        | rhs => raise Fail(concat["\n\nexpected rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString rhs])
        (* end case *))
    fun getRHSArgs x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => args
        | IR.LIT l => raise Fail(concat["\n\nLIT expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.LIT l)])
        | IR.OP l => raise Fail(concat["\n\nOP expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.OP l)])
        | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.CONS l)])
        | IR.GLOBAL l => raise Fail(concat["\n\nglobal expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.GLOBAL l)])
        | rhs => raise Fail(concat["\n\nexpected rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString rhs])
        (* end case *))
    fun getRHSMesh x = (case IR.Var.getDef x
	 	of IR.OP(Op.BuildSpace, [m, e, d, _])  => (getRHSMesh m)
		| IR.OP(Op.BuildSpace, [m, e, d])  => (getRHSMesh m) 
        | IR.OP(Op.BuildMesh m, _)          =>  m
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x, 
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    fun getRHSElement x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d, _]) => getRHSElement e
		| IR.OP(Op.BuildSpace, [m, e, d]) => getRHSElement e 
        | IR.OP(Op.BuildElement e, _)      => e
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x, 
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    fun getRHSDegree x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d, _]) => getRHSDegree d
	    | IR.OP(Op.BuildSpace, [m, e, d]) => getRHSDegree d
        | d => (IR.RHS.toString d)
			 (* end case *))
    fun getRHSS x = (case IR.Var.getDef x
        of  IR.LIT(Literal.String s) => s
        | rhs => raise Fail(concat[
       	 "expected rhs operator for ", IR.Var.toString x, 
        	" but found ", IR.RHS.toString rhs
        ])
        (* end case *))        
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
	
	
	fun pullPosition(E.EIN{body as E.Probe(f,[pos],ty), params, index}, args) =
        let

            val shape = []
            val vP = V.new ("posoutside", Ty.tensorTy shape)
            val ein = Ein.EIN{body=pos, params=params, index= shape}
            val stmt = FloatEin.transform(vP, ein, args)
            val _ = prntNewbies(stmt, "new stmt")
            val body' = E.Probe(f, [E.Tensor(List.length(args),[])], ty)
            val params' = params@[E.TEN(true, shape)]
            val args' =  args@[vP]
            val ein = Ein.EIN{body=body', params=params', index=index}
            in 
                (stmt, ein, args')   (*@PolyEin.transform(y, ein, args@[vP]) *)
            end 
                
end
