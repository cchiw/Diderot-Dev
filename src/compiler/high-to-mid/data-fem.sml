(* expand-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* get name of file from program name described *)
structure DataFem : sig

    val defineField: Ein.ofield *  MidIR.var list  ->  MidIR.var list * meshElem.mesh * meshElem.element* string

  end = struct

    structure IR = MidIR
    structure Op = MidOps
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure DE = DerivativeEin
    structure GVar = IR.GlobalVar
    structure EF = ExpandFem
     structure ME = meshElem


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
    (* fun getRHSShape x = (case IR.Var.getDef x *)
    (*     of IR.OP(Op.BuildSpace, [m, e, d,_])  =>   getRHSElement e *)
    (*     | IR.OP(Op.BuildElement e, _)       =>   e *)
    (*     | rhs => raise Fail(concat[ *)
    (*         "expected rhs operator for ", IR.Var.toString x, *)
    (*         " but found ", IR.RHS.toString rhs *)
    (*         ]) *)
    (*     (* end case *)) *)

    fun getRHSS x = (case IR.Var.getDef x
        of  IR.LIT(Literal.String s) => s
        | rhs => raise Fail(concat[
        "expected rhs operator for ", IR.Var.toString x,
        " but found ", IR.RHS.toString rhs
        ])
        (* end case *))



    (* get name of data file, and define compoments of the fem field (mesh, element)*)
    fun defineField (ofield, args) =
        (case ofield
            of E.DataFem dataid => let
                (*built-in. need to redo*)
               (*val (mesh, element)  = (ME.CubeMesh, ME.P) (*fixed for now *)*)
               (* val (mesh, element)  = (ME.UnitSquareMesh,ME.P)*)
                 val (mesh, element)  = (ME.NoneMesh, ME.NoneE)
               val datafile = getRHSS (List.nth(args, dataid))
               in
                    ([], mesh, element, datafile)
               end
            | E.BuildFem (fnspaceid, path) => let
                (*fn space argument*)
                val vfnspace = List.nth(args, fnspaceid)
                val (mesh, element, d ) =  (getRHSMesh vfnspace ,  getRHSElement vfnspace, getRHSDegree vfnspace)
                val dd : int = case (Int.fromString(d)) of
                                   SOME(i) => i
                                 | _ => raise Fail "Expected integer for dimension, but got something else"
                (* translate to fnspace to string*)
                val space  = String.concat[ME.toStringMesh mesh, "_",  ME.toStringElement element, "_",Int.toString(dd)]
                val pathtospace = getRHSS (List.nth(args,path))
                val datafile = String.concat[pathtospace,space,".json"]
                in
                    ([], mesh, element, datafile)
                end
            | E.ManyPointerBuildFem(mid, sid, fnspaceid, path) => let
                (*fn space argument*)
                val vfnspace = List.nth(args, fnspaceid)

                val (mesh, element,d)  = (getRHSMesh vfnspace ,  getRHSElement vfnspace,getRHSDegree vfnspace)
                (* translate to fnspace to string*)
                val space  = String.concat[ME.toStringMesh mesh, "_",  ME.toStringElement element, "_",d]

                val pathtospace = getRHSS (List.nth(args,path))
                val datafile = String.concat[pathtospace,space,".json"]
                val vSpace = List.nth(args, sid)
                val vMesh = List.nth(args, mid)
                in
                    ([vMesh, vSpace], mesh, element, datafile)
                end
            (* end case*))

end
