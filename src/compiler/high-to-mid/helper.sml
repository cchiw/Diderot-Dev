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

    val getRHSEINSrc: HighIR.var -> unit
    val getRHSEIN :  MidIR.var -> Ein.ein * MidIR.var list
    val getRHSMesh :  MidIR.var -> MidOps.mesh
    val getRHSElement :  MidIR.var ->  MidOps.element
    val getRHSDegree :  MidIR.var ->  string
    val getRHSS :  MidIR.var ->  string
    val ll : MidIR.var list * int -> string
    val paramToString : int * Ein.param_kind -> string
    val prntNewbies: (MidIR.var * MidIR.rhs) list * 'a -> unit list
    val line : string * MidIR.var * Ein.ein * MidIR.var list -> string
    val iterP: Ein.ein_exp list * Ein.ein_exp list -> Ein.ein_exp
    val iterA: Ein.ein_exp list * Ein.ein_exp list -> Ein.ein_exp
    
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
    fun ll ([],cnt) = ""
      | ll (a1::args,cnt) = 
      	String.concat["\n\t", Int.toString(cnt),"_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1),",", ll(args,cnt+1)]
    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
  	  | paramToString (i, E.FLD (d, shp)) = concat["F", i2s i, "(", i2s d, ")[",shp2s shp , "]"]
      | paramToString (i, E.KRN) = "H" ^ i2s i
      | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]
      | paramToString (i, E.SEQ shp) =    concat["Sequence[",shp2s shp , "]"]
     fun prntNewbies(newbies, id) = let 
        val _ = (id)
        in  List.map (fn (lhs,DstIR.EINAPP(e,a))=>print (concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs)," ",DstIR.Var.name(lhs),"=",EinPP.toString(e) ,"-",ll(a,0),"---->"])
		            | (lhs,rhs) => print(concat["\n\n -->:",DstIR.Var.name(lhs),"=",DstIR.RHS.toString rhs])
            ) newbies
        end
     fun line(name, y, ein, args) = String.concat[name, ":",MidIR.Var.name(y),"=", EinPP.toString(ein),"-",ll(args,0)]
          
	 (* ------------------------------------ get RHS --------------------------------------------  *)
    fun getRHSEINSrc x = (case SrcIR.Var.getDef x
        of  SrcIR.EINAPP (ein, args) => (print"ein-app")
        | SrcIR.LIT l => print(concat["\n\nSrcLIT expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.LIT l)])
        | SrcIR.OP l => print(concat["\n\nsrcOP expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.OP l)])
        | SrcIR.CONS l => print(concat["\n\nsrccons expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.CONS l)])
        | SrcIR.GLOBAL l => print(concat["\n\nsrcglobal expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.GLOBAL l)])
        | rhs => print(concat["\n\nsrcexpected rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString rhs])
        (* end case *))
    fun getRHSEIN x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => (ein, args)
        | IR.LIT l => raise Fail(concat["\n\nLIT expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.LIT l)])
        | IR.OP l => raise Fail(concat["\n\nOP expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.OP l)])
        | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.CONS l)])
        | IR.GLOBAL l => raise Fail(concat["\n\nglobal expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.GLOBAL l)])
        | rhs => raise Fail(concat["\n\nexpected rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString rhs])
        (* end case *))
    fun getRHSMesh x = (case IR.Var.getDef x
	 	of IR.OP(Op.BuildSpace, [m, e, d,_])  =>  (getRHSMesh m)
		| IR.OP(Op.BuildSpace, [m, e, d])  =>  (getRHSMesh m) 
        | IR.OP(Op.BuildMesh m, _)          =>  m
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x,
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    fun getRHSElement x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d,_])  =>   getRHSElement e
		| IR.OP(Op.BuildSpace, [m, e, d])  =>   getRHSElement e 
        | IR.OP(Op.BuildElement e, _)       =>   e
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x,
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))
    fun getRHSDegree x = (case IR.Var.getDef x
		of IR.OP(Op.BuildSpace, [m, e, d,_])  =>   getRHSDegree d
	    | IR.OP(Op.BuildSpace, [m, e, d])  =>   getRHSDegree d
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
    fun iterP([], [r]) = r
    | iterP ([], rest) = E.Opn(E.Prod, rest)
    | iterP (E.Const 0::es, rest) = E.Const(0)
    | iterP (E.Const 1::es, rest) = iterP(es, rest)
    | iterP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) =
        (* variable can't be 0 and 1 '*)
        if(c1=c2 orelse (not (v1=v2)))
        then iterP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
        else  E.Const(0)
    | iterP(E.Opn(E.Prod, ys)::es, rest) = iterP(ys@es, rest)
    | iterP (e1::es, rest)   = iterP(es, e1::rest)


    fun iterA([], []) = E.Const 0
    | iterA([], [r]) = r
    | iterA ([], rest) = E.Opn(E.Add, rest)
    | iterA (E.Const 0::es, rest) = iterA(es, rest)
    | iterA (E.Opn(E.Add, ys)::es, rest) = iterA(ys@es, rest)
    | iterA (e1::es, rest)   = iterA(es, e1::rest)
        
end
