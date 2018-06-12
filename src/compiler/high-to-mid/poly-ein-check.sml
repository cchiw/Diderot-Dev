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

    val isFieldFuncOk : MidIR.var list * MidIR.var ->  unit
    val checkParams :  (int * 'a) list * Ein.ein_exp * MidIR.var list -> unit 
  end = struct


    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure E = Ein
    structure H = Helper
    structure ISet = IntRedBlackSet

	        
     (*Is this argument okay for a field function*)
    fun isFieldFuncOk (replaced_vars, x) = let
        fun err msg = 
            raise Fail(concat["\n\n Error in the definition of the field function: ", msg,"\n\n"])
        fun ckVars x = List.find (fn r => IR.Var.same(r, x)) replaced_vars   
                          val _ = print(concat[ "\nc starting :", H.rhsToString (x, IR.Var.getDef x)])  
        fun deep_layer (x, last)  = 
            let
                fun errVar () =  err (concat["Can not use the input field variable inside ", last, "\n" ])
                val _ = print(concat[ "\ncurrent :", H.rhsToString (x, IR.Var.getDef x)])
                (* need to check that replacement variable isn't embedded inside another operator*)
                val _  = (case ckVars x
                        of NONE => ()
                        | SOME _ => errVar () 
                    (* end case*))           
                val rhs = IR.Var.getDef(x)
                val name = concat["(",H.rhsToString (x, rhs),")"]
             in    
                (case rhs
                     of IR.LIT(Literal.InVar _) => errVar () 
                     | IR.LIT _ => ()
                     | IR.CONS (args, _) => List.app (fn a => deep_layer (a, name))  args
                     | IR.VAR y => deep_layer (y, name)  
                     | IR.OP(op1, args) => List.app (fn a => deep_layer (a, concat[Op.toString op1, name]))  args
                     | IR.GLOBAL _ => () 
                     | _=> err(concat["\n\n Can not use ( ", name, ")" ])
                 (* end case *))
            end 
        (*top lhs is okay*)
        fun top_layer x  = let
            val rhs = IR.Var.getDef(x)
            val name = concat["(",H.rhsToString (x, rhs),")"]
            in (case rhs
                of IR.LIT _ => ()
                | IR.VAR y => (case ckVars(y)
                        of SOME _ => ()
                        | NONE => deep_layer(y, name))

               | _ =>  deep_layer(x, name)
            (* end case *))
            end 
        in 
           top_layer x
        end 
        
        	(* Check all substitution has been made *)	
    fun checkParams(cfexp_ids, exp_fn, args_orig) = let
        (*create set for replaced variables *)
        val replaced_set = List.map (fn (id,_) => List.nth(args_orig, id)) cfexp_ids

         val _ = List.app (fn x => print(concat[ "\n args original:", H.rhsToString (x, IR.Var.getDef x)])) args_orig  


        (*find parameters that were used in the function expression exp_fn*)
        val used_set = CleanParams.getFreeParams exp_fn
        fun iter(id, []) = ()
           | iter (id, a1::args) = 
            if(ISet.member(used_set, id))
             (* Are all the arguments field function okay? *)
                then (isFieldFuncOk(replaced_set, a1);iter (id+1, args) )
                else iter (id+1, args)             
        val _ = iter(0, args_orig)
        in () end 
end
