(* expand-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure translateField : sig

    val transform : MidIR.assign -> MidIR.assign list

  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet
    structure OF = OFieldToFem


    fun transform (y, IR.EINAPP(ein as E.EIN{body,...}, args)) = (case (body)
        of E.Probe(E.OField(ofld, _,_),_,_) => 
         	(case ofld
            	of E.CFExp _      => PolyEin.transform(y, ein, args)
                | E.DataFem _     => OF.scan_evaluate(y, IR.EINAPP(ein, args))
                | E.BuildFem _    => OF.scan_evaluate(y, IR.EINAPP(ein, args))
            (* end case*))
        |  E.Sum(_, E.Probe(E.OField(ofld, _,_),_,_)) => 
        	(case ofld
                of E.CFExp _      => PolyEin.transform(y, ein, args)
                | E.DataFem _     => OF.sum_evaluate (y, IR.EINAPP(ein, args))
                | E.BuildFem _    => OF.sum_evaluate (y, IR.EINAPP(ein, args))
            (* end case*))
        | _ =>   [(y, IR.EINAPP(ein, args))]
         (* end case*))
    | transform (y, e) =  [(y, e)]


end
