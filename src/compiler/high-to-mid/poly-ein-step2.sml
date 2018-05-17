(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin2 : sig

    val polyArgs :  MidIR.var list * Ein.param_kind list * (int * Ein.inputTy) list * 
          int list * Ein.ein_exp * Ein.mu option
          -> MidIR.var list * Ein.param_kind list * Ein.ein_exp

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
	

    (********************************** helper functions  *******************************)
    fun mkBase (tid, alpha, NONE) =  E.Tensor(tid, alpha)
      | mkBase (tid, alpha, SOME (sid)) =  E.Seq(tid, sid, alpha)
    fun mkPoly (tid, alpha, SeqId_current) = E.Poly(mkBase (tid, alpha, SeqId_current), 1, [])
    fun mkposMultiple(id, vx, dim, SeqId_current) =  let
		fun p1(id, cx, vx, SeqId_current)    = let
			val alpha =  [E.C cx]		
			val exp_poly =  mkPoly (id, alpha, SeqId_current)
			val exp_del = E.Delta(E.C cx, E.V vx)
			in  E.Opn(E.Prod, [exp_poly, exp_del])  end
		val polyTerms =  List.tabulate(dim, (fn n => p1(id, n, vx, SeqId_current)))
		in  E.Opn(E.Add, polyTerms) end
    fun mkposScalar(id, SeqId_current) =    mkPoly (id,[], SeqId_current)


    (********************************** Step 2 *******************************)
    (* replace tensor with delta expression and flatten products *)
    fun replaceD (body, dim, mapp, args,SeqId_current) = let
        fun replace (body,args) =
        	let
            	fun sort([], fs, args)  = (List.rev fs,args)
                | sort (e1::es, fs, args) = let
                    val (body, args) =replace(e1, args)
                    in sort(es, body::fs,args) end
               (* fun wrap(id,alpha E.None)    => e
                  | wrap(e, E.Some id) => E.*)
            in (case body
                of E.Const _            => (body, args)
                | E.ConstR _            => (body, args)
                | E.Zero _              => (body, args)
                | E.Delta _             => (body, args)
                | E.Epsilon _           => (body, args)
                | E.Eps2 _              => (body, args)
                | E.Field _             => (body, args)
                | E.Tensor(id, [])       => 
                	if (ISet.member(mapp, id))
                	then (mkposScalar(id,SeqId_current),args) 
                	else (body, args)
                | E.Tensor(id, [E.V vx]) =>
                    if (ISet.member(mapp, id))
                    then (mkposMultiple(id,vx, dim,SeqId_current), args)
                    else (body, args)
                | E.Tensor _            => (body, args)
                | E.Poly _              => (body, args)
                | E.Lift(e1)            =>
                    let
                   		val (body, args) = replace(e1,args)
                    in (E.Lift(body), args) end
                | E.Comp(e1, es)        =>
                    let
                    	val (body, args) = replace(e1,args)
                    in (E.Comp(body,es), args) end
                | E.Sum(op1, e1)        =>
                    let
                    	val (body, args) = replace(e1,args)
                    in (E.Sum(op1,body), args) end
                | E.Op1(E.PowInt n, e1)   => let
                    	val (body, args) = replace(e1,args)
                    	val body = iterP (List.tabulate(n, fn _ => body),[])
                    in
                        (body, args)
                    end
                | E.Op1(op1, e1)                       => let
                    	val (body, args) = replace(e1,args)
                    in (E.Op1(op1, body), args) end
                | E.Op2(op2, e1, e2)                   =>
                    let
                    	val (body1, args) = replace(e1,args)
                    	val (body2, args) = replace(e2,args)
                    in (E.Op2(op2, body1, body2), args) end
                | E.Opn(E.Prod, E.Opn(E.Prod, ps)::es) => replace(iterP(ps@es,[]),args)
                | E.Opn(E.Prod, E.Opn(E.Add, ps)::es)  =>
                    let
                    	val ps = List.map (fn  e1 =>   iterP(e1::es,[])) ps
                    	val body = E.Opn(E.Add, ps)
                    in  replace (body, args) end

                | E.Opn(E.Prod, [_])            => raise Fail(EinPP.expToString(body))
                | E.Opn(E.Prod, ps)             => let
                    	val (fs, args) = sort(ps,[], args)
                   	 val body = iterP(fs, [])
                    in (body, args) end
                | E.Opn(E.Add , ps)             =>
                    let
                    	val (fs, args) = sort(ps,[], args)
                    	val body = iterA(fs, [])
                    in (body, args) end
                | E.Opn(E.Swap id , ps)         =>
                    let
                    	val body = E.Tensor(id,[])::ps
                    	val (fs, args) = sort(body,[], args)
                    	val body = iterA(fs, [])
                    in (body, args) end
                | _                             => raise Fail(EinPP.expToString(body))
                (* end case*))
            end
        in replace (body,args)
        end
    (*do the variable replacement for all the poly input variables *)
    fun polyArgs(args, params, pargs, probe_ids, e, SeqId) =
        let
			(*replacement instances of arg at idy position with arg at idx position *)
			fun replaceArg(args, params,  idx, idy) =
				let
					(*variable arg, and param*)
					val arg_new = List.nth(args, idx)
					val param_new = List.nth(params, idx)
					val arg_replaced = List.nth(args, idy)
					val _ = (String.concat["\n\nreplace :", IR.Var.name  arg_replaced," with new :", IR.Var.name  arg_new])
					(*id keeps track of placement and puts it in mapp*)
					fun findArg(_, es, newargs, [], newparams, mapp) = ((List.rev newargs)@es, List.rev newparams, mapp)
					| findArg(id, e1::es, newargs, p1::ps, newparams, mapp) =
						if(IR.Var.same(e1,arg_replaced))
						then findArg(id+1,es, arg_new::newargs, ps, param_new::newparams, ISet.add(mapp, id))
						else findArg(id+1,es, e1::newargs, ps ,p1::newparams, mapp)
					val (args, params, mapp) = findArg(0, args, [], params, [], ISet.empty)
					val _ = ("\n\nparams:"^String.concatWith "," (List.mapi paramToString params))
					val _ = ("\nargs:"^String.concatWith "," (List.map IR.Var.name  args))

				in (args, params, mapp) end
			(*field variables *)
			fun single_TF (pid, args, params, idx,e) =
				let
					val _ = (String.concat["\n iter TF replacing param id:",Int.toString(pid),"with idx:",Int.toString(idx)])
					val _ = (String.concat["\n\nMark -1: body:",EinPP.expToString(e)," args#:",Int.toString(length(args)),"\n\n"])
					val SeqId_current = (case List.nth(params,idx)
							of E.SEQ(_)   => SeqId
							| param_probe => NONE
						(* end case *))
					val (args, params, mapp) = replaceArg(args, params, idx, pid)
					(* get dimension of vector that is being broken into components*)
					val param_pos = List.nth(params, pid)
					val dim = 
						(case param_pos
							of E.TEN (_,[]) => 1
							| E.TEN (_,[i]) => i
							| E.SEQ([])		=> 1
							| E.SEQ([i])    => i
							| _ => raise Fail"unsupported replacement argument type"
						(* end case*))
					(* replace position tensor with deltas in body*)
					val _ = (String.concat["\n\nMark 0: body:",EinPP.expToString(e)," args#:",Int.toString(length(args)),"\n\n"])
					val (e,args) = replaceD (e, dim, mapp, args,SeqId_current)
					val _ = (String.concat["\n\nMark 1: body:",EinPP.expToString(e)," args#:",Int.toString(length(args)),"\n\n"])
					val (e,args) = replaceD (e, dim, mapp, args,SeqId_current)
					val _ = (String.concat["\n\nMark 2: body:",EinPP.expToString(e)," args#:",Int.toString(length(args)),"\n\n"])
				in (args,params, e) end				
			(*iterate over all the input tensor variable expressions *)
			fun iter([], args, params, _, e) = (args, params,e)
			  | iter((pid,E.T)::es, args, params, idx::idxs,e) = let
				val args = List.take(args,pid)@[List.nth(args, idx)]@List.drop(args,pid+1) (*tensor variables*)
				in iter(es, args, params, idxs, e) end
			  | iter((pid,E.F)::es, args, params, idx::idxs,e) =
				let
					val (args,params, e) = single_TF (pid, args, params, idx,e)
				in
					iter(es, args,params, idxs, e)
				end
			(*probe_id: start of position variables for probe operation *)
			fun iTos(name,es) =
				 String.concat[name ,String.concatWith","(List.map (fn e1=> String.concat[Int.toString(e1)]) es)]
			val _ = (String.concat["\n\n",EinPP.expToString(e),
						"\n\n",iTos("probe_idxs:",probe_ids),
						"\n\n","Argsl:", Int.toString(length(args)),"Paramsl:", Int.toString(length(params))])
			val (args, params, e) = iter(pargs, args, params, probe_ids, e)
			val _ = (String.concat["\n\n post replacing TT:",EinPP.expToString(e),"-",ll(args,0)])
		in (args, params, e) end
		

  end
