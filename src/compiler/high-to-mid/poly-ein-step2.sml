(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin2 : sig

    val polyArgs :  Ein.param_kind list * Ein.ein_exp * MidIR.var list * Ein.mu option
          * (int * Ein.inputTy) list * int list
          -> MidIR.var list * Ein.param_kind list * Ein.ein_exp * (MidIR.var * MidIR.rhs) list

  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet
    structure SrcIR = HighIR
    structure DstIR = MidIR
    structure ScanE = ScanEin
    structure H = Helper
	val ll = H.ll
	val paramToString = H.paramToString
	val iterP = H.iterP
	val iterA = H.iterA
	

    (* The terms with a param_id in the mapp are replaced
    * body - ein expression
    * args - variable arguments
    * dim - dimension 
    * SeqId_current - current sequential option
    * mapp - map for replacements 
    *)
    fun replace (body, dim, SeqId_current, mapp) = let
    	(*rewriteTensor
    	* This a is a tensor that is treated like a field
    	* Replace tensor term with a new term (new id and space components)
    	* V => [V_0 , V_1, V_2]
    	* and with deltas to turn each component on and off
    	* V => V_0*Delta_0i + V_1*Delta_1i V_2+Delta_2i
    	*)
    	fun rewriteTensor (E.Tensor(tid, alpha)) = 
    		let 
    			fun mkBase (alpha') = (case SeqId_current
    					of NONE =>  E.Tensor(tid, alpha')
      			  		| SOME (sid) =>  E.Seq(tid, sid, alpha')
      			  		(*end case*))
        		fun mkPoly (alpha') = E.Poly(mkBase alpha', 1, [])
    		in 	
				(case alpha
					of [] => mkPoly []
					| [vx] =>
						let
							fun mkComponent cx = E.Opn(E.Prod, [mkPoly [E.C cx], E.Delta(E.C cx,  vx)]) 
							val polyTerms =  List.tabulate(dim, (fn n => mkComponent n))
						in  E.Opn(E.Add, polyTerms) end
					| _ => raise Fail "unhandled size"
				(*end case*))
    		end
  		(*search body for tensor terms that are meant to be replaced*)
        fun rewrite body = 
        	(case body
                of E.Tensor (id, _)		 => if (ISet.member(mapp, id)) then rewriteTensor body else body
                | E.Lift(e1)            => E.Lift(rewrite e1)
                | E.Comp(e1, es)        => E.Comp(rewrite e1, es)
                | E.Sum(op1, e1)        => E.Sum(op1, rewrite e1)
                | E.Op1(E.PowInt n, e1) => 
                	let
                    	val tmp = rewrite e1
                    	in iterP (List.tabulate(n, fn _ => tmp))
                    end
                | E.Op1(op1, e1)                       => E.Op1(op1, rewrite e1)
                | E.Op2(op2, e1, e2)                   => E.Op2(op2, rewrite e1, rewrite e2) 
                | E.Opn(E.Prod, E.Opn(E.Add, ps)::es)  =>
                    let
                    	val ps = List.map (fn  e1 =>   iterP(e1::es)) ps
                    	val body = E.Opn(E.Add, ps)
                    in  rewrite body end
                | E.Opn(E.Prod, ps)             => iterP(List.map rewrite  ps)
                | E.Opn(E.Add , ps)             => iterA(List.map rewrite  ps)
                | E.Opn(E.Swap id , es)         =>  if (ISet.member(mapp, id)) then raise Fail "revisit here" else body  
                | _    => body
            (* end case*))
        in rewrite body  end

        
    (* Replace the arguments identified in cfexp-ids with the arguments in probe-ids
    * params - EIN params
    * e - EIN body  
    * args - vars
    * SeqId - optional sequence index variable
    * cfexp_ids - closed-form expression has ids
    * probe_ids - field is probed at position with ids
    *  PROBE(CFEXP (cfexp_ids), probe_ids)
    *)
    fun polyArgs(params, e, args,  SeqId, cfexp_ids, probe_ids) = 
        let
			(*does rewritement of a single variable 
			* rewritement instances of arg at pid position with arg at idx position 
			*)
			fun single_TF (pid, args, params, idx, e)  = 
				let
					(*check if the current parameter is a sequence and get dimension*)
					val (dim, SeqId_current) = (case List.nth(params, idx)
							of E.SEQ(_, [dim])   => (dim, SeqId)
							| E.TEN (_, []) => (1, NONE)
							| E.TEN (_, [i]) => (i, NONE)
							| p => raise Fail("unsupported argument type:"^H.paramToString(idx, p))
						(* end case *))						
					(*variable arg, and param*)
					val arg_new = List.nth(args, idx)
					val param_new = List.nth(params, idx)
					val arg_rewrited = List.nth(args, pid)
					val _ = (String.concat["\n\nrewrite :", IR.Var.name  arg_rewrited, " with new :", IR.Var.name  arg_new])
					(*id keeps track of placement and puts it in mapp*)
					fun findArg(_, es, newargs, [], newparams, mapp) = ((List.rev newargs)@es, List.rev newparams, mapp)
					| findArg(id, e1::es, newargs, p1::ps, newparams, mapp) = 
						if(IR.Var.same(e1, arg_rewrited))
						then findArg(id+1, es, arg_new::newargs, ps, param_new::newparams, ISet.add(mapp, id))
						else findArg(id+1, es, e1::newargs, ps , p1::newparams, mapp)
					val (args, params, mapp) = findArg(0, args, [], params, [], ISet.empty)
					(* get dimension of vector that is being broken into components*)
					val param_pos = List.nth(params, pid)
				
					val _ =( H.toStringBA("mark1", e, args))
									
					val e = replace (e, dim, SeqId_current, mapp)
					val _ = H.toStringBA("mark2", e, args)
				in (args, params, e) end								
			(*iterate over all the input tensor variable expressions *)
			fun iter([], args, params, _, e, rtn) = (args, params, e, rtn)
			  | iter((pid, E.T)::es, args, params, idx::idxs, e, rtn) = let
			  	(*variable is treated as a tensor so a simple variable swap is sufficient *)
				val args = List.take(args, pid)@[List.nth(args, idx)]@List.drop(args, pid+1) 
				in iter(es, args, params, idxs, e,rtn) end
			  | iter((pid, E.F)::es, args, params, idx::idxs, e, rtn) = 
			  	(*variable is treated as a field so it needs to be expanded into its components*)
				let(*
				    val _ =  (String.concat["\nfield pid:", Int.toString(pid), "idx:", Int.toString(idx)])
				    val _ = List.map (fn v => ("\ninside:"^IR.Var.name(v))) args
					val arg_y = List.nth(args, pid)
					val arg_x = List.nth(args, idx)
					val rtn1 = (arg_y, IR.VAR (arg_x))
					*)
					(*FIXME need to iterate upward*)
					val (args, params, e) = single_TF (pid, args, params, idx, e)
				in
					iter(es, args, params, idxs, e, rtn)
				end
			(*probe_id: start of position variables for probe operation *)
			val (args, params, e, rtn) = iter(cfexp_ids, args, params, probe_ids, e, [])
		in (args, params, e, rtn) end
		

  end
