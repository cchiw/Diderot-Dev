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
    structure BF = BuildFem


    fun transform (y, IR.EINAPP(ein as E.EIN{body,...}, args)) = (case (body)
            of E.Probe(E.OField(ofld, _,_),_) => let
(*

            val _ = (String.concat ["\n\n\n Transform oField:\n\t", EinPP.toString(ein)])
            val _ = ("\n\n with args:"^String.concatWith "," (List.map IR.Var.name  args))
*)
            in (case ofld
            of E.CFExp _   => PolyEin.transform(y, ein, args)
                | E.DataFem _     => BF.scan_evaluate(y, IR.EINAPP(ein, args))
                | E.BuildFem _    => BF.scan_evaluate(y, IR.EINAPP(ein, args))
                | E.ManyPointerBuildFem _    => BF.scan_evaluate(y, IR.EINAPP(ein, args))
            (* end case*))
            end
        |  E.Sum(_, E.Probe(E.OField(ofld, _,_),_)) => let
(*
            val _ = (String.concat ["\n\n\n Transform oField:\n\t", EinPP.toString(ein)])
            val _ = ("\n\n with args:"^String.concatWith "," (List.map IR.Var.name  args))
*)
            in (case ofld
                of E.CFExp _   => PolyEin.transform(y, ein, args)
                | E.DataFem _     => BF.sum_evaluate (y, IR.EINAPP(ein, args))
                | E.BuildFem _    => BF.sum_evaluate (y, IR.EINAPP(ein, args))
                | E.ManyPointerBuildFem _    => BF.sum_evaluate(y, IR.EINAPP(ein, args))
                (* end case*))
            end
        | _ =>   [(y, IR.EINAPP(ein, args))]
         (* end case*))
    | transform (y, e) =  [(y, e)]


end
