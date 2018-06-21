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
    structure OF = EvalFem

           
    fun OfieldTransform (y, IR.EINAPP(ein, args), f, isSum) = 
    	(case f
    	    of E.OField(E.CFExp _ , _,_) =>  PolyEin.transform(y, ein, args) 
            | E.OField (E.DataFem _ , _ , _) => if(isSum) 
                    then OF.sum_evaluate (y, IR.EINAPP(ein, args))
                     else  OF.scan_evaluate(y, IR.EINAPP(ein, args))
                     (*
            | E.OField (E.DataFem(_, E.Seq) =>   teo's function(y, IR.EINAPP(ein, args))
            *)
            | _ => [(y, IR.EINAPP(ein, args))]
        (*end case*))


   fun transform (y, rhs as IR.EINAPP(ein as E.EIN{body, params, index}, args)) = 
        (case body 
            of E.Probe(f, [E.Tensor _], _) =>   OfieldTransform (y, rhs, f, false)
            | E.Probe(f, [pos], _) =>
                let
                val  (stmt, ein, args) = FloatEin.liftPos(ein, args)
                val rhs = IR.EINAPP(ein, args)
                in stmt@OfieldTransform (y, rhs, f, false) end 
            | E.Probe(f, _,_) =>   OfieldTransform (y, rhs, f, false)
            | E.Sum(_, E.Probe(f, [E.Tensor _],ty)) =>  OfieldTransform (y, rhs, f, true)
            | E.Sum(_, E.Probe(f, [pos], ty)) =>
                let
                val  (stmt, ein, args)  = FloatEin.liftPos(ein, args)
                val rhs = IR.EINAPP(ein, args)
                in stmt@OfieldTransform (y, rhs, f, true) end 
            | E.Sum(_, E.Probe(f, _,_)) =>   OfieldTransform (y, rhs, f, true)
            | _ =>  [(y, rhs)]
        (* end case*))
     | transform (y, rhs) =  [(y, rhs)]
end
