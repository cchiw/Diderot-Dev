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
    structure GVar = IR.GlobalVar
    structure ME = meshElem
    structure H = Helper

    (* get name of data file, and define compoments of the fem field (mesh, element)*)
    fun defineField (ofield, args) =
    	(case ofield
            of E.DataFem dataid => let
				val (mesh, element)  = (ME.NoneMesh, ME.NoneE)
				val datafile = H.getRHSS (List.nth(args, dataid))
				in
					([], mesh, element, datafile)
				end
			| E.BuildFem (fnspaceid, path) => let
				(*fn space argument*)
				val vfnspace = List.nth(args, fnspaceid)
				val (mesh, element, d ) =  (H.getRHSMesh vfnspace ,  H.getRHSElement vfnspace, H.getRHSDegree vfnspace)
				val dd : int = case (Int.fromString(d))
					of SOME(i) => i
					| _ => raise Fail "Expected integer for dimension, but got something else"
				(* translate to fnspace to string*)
				val space  = String.concat[ME.toStringMesh mesh, "_",  ME.toStringElement element, "_",Int.toString(dd)]
				val pathtospace = H.getRHSS (List.nth(args,path))
				val datafile = String.concat[pathtospace,space,".json"]
				val _ = print(datafile)
				in
					([], mesh, element, datafile)
				end
    	(* end case*))

end
