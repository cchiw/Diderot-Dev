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

structure checkFieldFunc : sig

    val checkParams :  (int * 'a) list * Ein.ein_exp * MidIR.var list -> unit 
    
  end = struct


    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure E = Ein
    structure H = Helper
    structure ISet = IntRedBlackSet

	        
    (* Check all substitution has been made *)	
    fun checkParams(cfexp_ids, exp_fn, args_orig) = let
          (*create set for replaced variables *)
        val replaced_vars = List.map (fn (id,_) => List.nth(args_orig, id)) cfexp_ids
        (* find parameters that were used in the function expression exp_fn *)
        val used_set = CleanParams.getFreeParams exp_fn
        fun err msg = 
            raise Fail(concat["\n\n Error in the definition of the field function: ", msg,"\n\n"])
        fun errVar last =  err (concat["Can not use the input field variable inside ", last, "\n" ])
        fun ckVars x  = (List.find (fn r => IR.Var.same(r, x)) replaced_vars)
        (* need to check that replacement variable isn't embedded inside another operator *)
        fun deep_layer (x, last)  = 
            let
                (* need to check that replacement variable isn't embedded inside another operator*)
                val _  = (case ckVars(x)
                        of NONE => ()
                        | SOME _ => errVar (last) 
                    (* end case*))           
                val rhs = IR.Var.getDef(x)
                val name = concat["(",H.rhsToString (x, rhs),")"]
             in    
                (case rhs
                     of IR.LIT(Literal.InVar _) => errVar (last) 
                     | IR.LIT _ => ()
                     | IR.CONS (args, _) => List.app (fn a => deep_layer (a, name))  args
                     | IR.VAR y => deep_layer (y, name)  
                     | IR.OP(op1, args) => List.app (fn a => deep_layer (a, concat[Op.toString op1, name]))  args
                     | IR.GLOBAL _ => () 
                     | _=> err(concat["\n\n Can not use ( ", name, ")" ])
                 (* end case *))
            end 
        (* top layer can have variable *)
        fun isFieldFuncOk x  = (case IR.Var.getDef(x)
                of IR.LIT _ => ()
                | IR.VAR y => (case ckVars(y)
                        of SOME _ => ()
                        | NONE => deep_layer(y, concat["(",H.rhsToString (x, IR.Var.getDef(x)),")"]))
               | rhs =>  deep_layer(x, concat["(",H.rhsToString (x, rhs),")"])
            (* end case *))
        fun iter(id, []) = ()
           | iter (id, a1::args) = 
            if(ISet.member(used_set, id))
             (* Are all the arguments field function okay? *)
                then (isFieldFuncOk(a1);iter (id+1, args) )
                else iter (id+1, args)             
        val _ = iter(0, args_orig)
        in () end 
end
