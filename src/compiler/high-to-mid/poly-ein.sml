(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin : sig

    val transform : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assign list

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
			in  E.Opn(E.Prod, [exp_poly,exp_del])  end
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
		
		
    (********************************** Step 3 *******************************)
    (* collect the poly expressions*)
    fun mergeD (body) = let
    		fun merge body = (case body
        		of E.Const _            => body
				| E.ConstR _            => body
				| E.Zero _              => body
				| E.Delta _             => body
				| E.Epsilon _           => body
				| E.Eps2 _              => body
				| E.Field _             => body
				| E.Tensor _            => body
				| E.Poly _              => body
				| E.Lift(e1)            => E.Lift(merge e1)
				| E.Comp(e1, es)        => E.Comp(merge e1, es)
				| E.Sum(op1, e1)        => E.Sum(op1, merge e1)
				| E.Op1(op1, e1)        => E.Op1(op1, merge e1)
				| E.Op2(op2, e1, e2)    => E.Op2(op2, merge e1, merge e2)
				| E.Opn(E.Prod, E.Opn(E.Prod, ps)::es) =>  merge(iterP(ps@es,[]))
				| E.Opn(E.Prod, E.Opn(E.Add, ps)::es)
					=>  merge (E.Opn(E.Add, List.map (fn  e1 => merge(iterP(e1::es,[]))) ps))
				| E.Opn(E.Prod, ps)        =>
					let
						val _ = (String.concat ["\n\n merged in product:", EinPP.expToString(body)])
						fun  iter([], rest, [], _) = iterP(rest,[])
							| iter([], rest, ids, mapp) = let
								fun mkPolyAll([], p) =  iterP(p@rest,[])
						 		 | mkPolyAll(id::es, p) = let
									fun mkPoly(c, 0) = []
							 		 | mkPoly(5, n) = [E.Poly(E.Tensor(id, []), n, [])]
							  		 | mkPoly(c, n) = [E.Poly(E.Tensor(id, [E.C c]), n, [])]
									val (n0, n1, n2, n3) = (case IMap.find (mapp, id)
										of SOME e => e
										| NONE => (0,0,0,0)
										(* end case*))
									val _ = (String.concat ["\n\nlookup : ", Int.toString(id)," ->>",  Int.toString(n0),",", Int.toString(n1),",", Int.toString(n2),",", Int.toString(n3)])
									val ps = mkPoly(5, n0)@mkPoly(0, n1)@ mkPoly(1, n2)@ mkPoly(2, n3)
									in mkPolyAll(es, p@ps) end
								in mkPolyAll(ids, []) end
							| iter(e1::es, rest, ids,  mapp) =(case e1
								of E.Opn(E.Prod, ys) => iter(ys@es, rest, ids, mapp)
								| E.Poly(E.Tensor(id, ix), n, _) =>
									let
										val ((n0, n1, n2, n3), ids) = (case IMap.find (mapp, id)
											of SOME e => (e, ids)
											| NONE => ((0,0,0,0), id::ids)
											(* end case*))
										fun next e =  let
											val (n0,n1,n2,n3) =e
											val _ = (String.concat ["\n\ninsert: ", Int.toString(id)," ->>",  Int.toString(n0),",", Int.toString(n1),",", Int.toString(n2),",", Int.toString(n3)])
											in iter(es, rest, ids, IMap.insert (mapp, id, e)) end
									in
										if(ix=[]) then  next (n0+n, n1, n2, n3)
										else if(ix=[E.C 0]) then next (n0, n1+n, n2, n3)
										else if(ix=[E.C 1]) then next (n0, n1, n2+n, n3)
										else if(ix=[E.C 2]) then next (n0, n1, n2, n3+n)
										else  raise Fail(EinPP.expToString(body))
									end
							| _ => iter(es, e1::rest, ids, mapp)
							(* end case *))
						val body'= iter( List.map merge ps , [], [], IMap.empty)
						val _ = (String.concat ["\n\n ->>", EinPP.expToString(body')])
					in body' end
				| E.Opn(E.Add , ps) =>  iterA(List.map merge ps, [])
				| E.Opn(E.Swap id , ps) =>  iterA(List.map merge ps, [])
				| _                     => raise Fail(EinPP.expToString(body))
				(* end case*))
				val body' = merge body
				(*val _ = (String.concat ["\n\n mergenew :", EinPP.expToString(body')])*)
				in body' end

    (********************************** Step 4 *******************************)
    fun cleanup body = 
    	(case body
			of E.Const _            => body
			| E.ConstR _            => body
			| E.Zero _              => body
			| E.Delta _             => body
			| E.Epsilon _           => body
			| E.Eps2 _              => body
			| E.Field _             => body
			| E.Tensor _            => body
			| E.Poly (E.Tensor(id,c), 0, dx)  => E.Const 1
			| E.Poly (_, n, dx)  => body
			| E.Lift(e1)            => E.Lift(cleanup(e1))
			| E.Sum(op1, e1)        => let
				val e2 = cleanup e1
				in (case e2
					of E.Opn(E.Add, ps) => E.Opn(E.Add, List.map (fn e1=>E.Sum(op1, cleanup(e1))) ps )
					| _                 => E.Sum(op1, cleanup(e2))
				(*end case*))
				end
			| E.Op1(op1, e1)        => E.Op1(op1, cleanup( e1))
			| E.Op2(op2, e1, e2)    => E.Op2(op2, cleanup( e1), cleanup( e2))
			| E.Opn(E.Prod, es)        =>  let
				val xx = List.map (fn e1=>cleanup ( e1)) es
				in iterP(xx, []) end
			| E.Opn(E.Add, es)        =>  let
				val xx = List.map (fn e1=> cleanup ( e1)) es
				in iterA(xx, []) end
			| _    => raise Fail(EinPP.expToString(body))
        (* end case*))
    (*apply differentiation and clean up*)
    fun normalize (e', dx) = 
    	let
			fun rewrite(body) = (("\nn rewite:"^EinPP.expToString(body));case body
				of E.Apply(E.Partial dx, e)     =>  rewrite(DerivativeEin.differentiate (dx, e))     (* differentiate *)
				| E.Op1(op1, e1)                =>   E.Op1(op1, rewrite e1)
				| E.Op2(op2, e1,e2)             =>   E.Op2(op2, rewrite e1, rewrite e2)
				| E.Opn(opn, es)                =>   E.Opn(opn, List.map rewrite es)
				| _                             => body
				(* end case*))
        	val e' = (case dx
				of []       => cleanup(e')
				| [di]      => rewrite(E.Apply(E.Partial [di], e'))
				| [di, dj]  => rewrite(E.Apply(E.Partial [dj], rewrite(E.Apply(E.Partial [di], e'))))
				| [di, dj, dk]  =>
					rewrite(E.Apply(E.Partial [dk], rewrite(E.Apply(E.Partial [dj], rewrite(E.Apply(E.Partial [di], e'))))))
				| _    => raise Fail("unsupported level of differentiation: not yet")
				(*end case*))
        (*val _ = (String.concat ["\n\n differentiate :", EinPP.expToString(e')])*)
        in e' end


    (********************************** main *******************************)
    (* transform ein operator *)
    fun transform_Core (y, sx, ein as Ein.EIN{body,index, params}, args, SeqId) =
        let
        	val _ = print("\n\n*******************\n") 
            val _ = print(H.line("core:",y, ein,args))
            val E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty) = body

            (********************************** Step 2 *******************************)
            (* replace polywrap args/params with probed position(s) args/params *)
            val start_idxs =  List.map (fn E.Tensor(tid,_) => tid) expProbe
            val n_pargs = length(pargs)
            val n_probe = length(start_idxs)
            val _ = if(not(n_pargs =n_probe))
                    then raise  Fail(concat[" n_pargs:", Int.toString( n_pargs),"n_probe:",Int.toString(n_probe)])
                    else 1
            val (args, params, e) = polyArgs(args, params, pargs, start_idxs, e, SeqId)
            val ein = Ein.EIN{body=e, index=index, params=params}
            val _ = print(H.line("\n\n  Step 2 post polyArgs",y, ein,args))
            (********************************** Step 3 *******************************)
            (* need to flatten before merging polynomials in product *)
            val e = mergeD(e)
            val _ = print(H.line("\n\n  Step3  mergeD",y, ein,args))
           (********************************** Step 4 *******************************)
           (* normalize ein by cleaning it up and differntiating*)
            val e = normalize(e, dx)
            val _ = print(H.line("\n\n   Step4  normalize",y, ein,args))
         in (args, params, e) end 
            
            
    (********************************** main *******************************)
    (* transform ein operator *)
    fun transform_Wrapper (y, ein, args) =
        let
            val _ = print(H.line("\n\n   Step 0 Wrapper",y, ein,args))
            val Ein.EIN{body,index, params} = ein
            (********************************** Step 0 *******************************)
            val (sx, body) =  (case body
            		of E.Sum(sx,body) => (sx, body)
            		| _ => ([], body)
            	(* end case *))
        	val E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty) = body
        	
        	(* FIX ME variable is used in summation for sequences *)
        	val freshid = 101
        	val sx_seq = [(freshid, 0, 3)] 
            val SeqId = (case pty 
            		of SOME _ => SOME (E.V freshid )
            		| NONE =>  NONE 
            	(* end case *))
            (********************************** Step 2-4 *******************************)
            val (args, params, e)  = transform_Core (y, sx, ein, args, SeqId) 
            
        	(********************************** Step 5 *******************************)
            (* Add summation wrapper to ein expression *)
            val e = (case sx
            		of [] => e
                	|  _  => E.Sum(sx, e)
            	(* end case *))
            (* Add sequence wrapper to ein expression *)           
            val e = (case pty
            		of SOME opn => E.OpR(opn, sx_seq, e) (*E.Sum(sx_seq,e)*)
            		| NONE =>  e 
            	(* end case *))
            val _ = print(H.line("\n\n   Step 5 ",y, ein,args))
            val _ = (String.concat["\n\n*******************\n"])
            (********************************** Step 5  *******************************)
            val newbie = (y, IR.EINAPP(Ein.EIN{body= e, index=index, params=params}, args))
            val stg_poly = Controls.get Ctl.stgPoly
            val _ =  if(stg_poly) then ScanE.readIR_Single(newbie,"tmp-poly") else 1
        in
               [newbie]
        end



       fun transform e =  transform_Wrapper e
  

	
	    (********************************** unused  *******************************)
(*
    fun replaceCons(body, args) = let
        val E.Tensor(id, [E.V vx]) = body
        val idshift = length(args)
        val x  = List.nth(args,id)
        in (case IR.Var.getDef(x)
            of IR.CONS ([a1,a2], Ty.TensorTy[2]) => ( case (IR.Var.getDef(a1),IR.Var.getDef(a2))
                of (IR.EINAPP(ein1, args1),IR.EINAPP(ein2, args2)) =>
(*replace with ein apply*)
                | _ =>  (MkOperators.concatTensorBody ([], nflds, idshift), args@args_cons)
            | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for "(*, IR.Var.toString x*), " but found ", IR.RHS.toString (IR.CONS l)])
            | _ => (body, args)
            (*end case*))
        end

*)


  end
