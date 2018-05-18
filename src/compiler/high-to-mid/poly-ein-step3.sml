(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin3 : sig

    val rewriteMerge : Ein.ein_exp -> Ein.ein_exp
    val rewriteDifferentiate :  Ein.ein_exp  -> Ein.ein_exp

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
	
	(*flatten product and merge poly terms into a single poly term
	*  x *x  => X^2
	*)
	fun rewriteMergeProd es  = 
		let			
			fun iter([], rest, ids, mapp) = (rest, ids, mapp)
				| iter(e1::es, rest, ids,  mapp) = (case e1
					of E.Poly(E.Tensor(id, ix), n, _) =>
						let
							val ((n0, n1, n2, n3), ids) = (case IMap.find (mapp, id)
								of SOME e => (e, ids)
								| NONE => ((0,0,0,0), id::ids)
								(* end case*))
							fun SetMap ns = IMap.insert (mapp, id, ns)							
							val mapp = 
								if(ix=[]) then SetMap (n0+n, n1, n2, n3)
								else if(ix=[E.C 0]) then SetMap (n0, n1+n, n2, n3)
								else if(ix=[E.C 1]) then SetMap (n0, n1, n2+n, n3)
								else if(ix=[E.C 2]) then SetMap (n0, n1, n2, n3+n)
								else  raise Fail("unhandled")
						in 
							iter(es, rest, ids, mapp)
						end
					| _ => iter(es, e1::rest, ids, mapp)
					(* end case *))			
			val (rest, ids, mapp) = iter(es , [], [], IMap.empty)
			(*make poly term for each id given component and power*)
			fun mkPolyAll([], ps) =  ps
			  | mkPolyAll(id::es, ps) =  
				(case IMap.find (mapp, id)
					of NONE => mkPolyAll(es, ps)
					| SOME (n0, n1, n2, n3) =>
						let (*each term is T(id)_c^n*)
							fun mkPoly([]) = []
							   | mkPoly((c, 0)::es) = mkPoly es
							   | mkPoly((5, n)::es) = E.Poly(E.Tensor(id, []), n, []) :: (mkPoly es) (*Note:5 is used for scalars*)
							   | mkPoly((c, n)::es) = E.Poly(E.Tensor(id, [E.C c]), n, []):: (mkPoly es)
							val pnew =  mkPoly([(5, n0), (0, n1), (1, n2),(2, n3)])
						in mkPolyAll(es, pnew@ps) end	
				(* end case*))					
			val ps =  mkPolyAll(ids, []) 
			(*flatten *)
			val e = iterP(ps@rest)	
		in
			e
		end
	
    (* collect the poly expressions*)
    fun rewriteMerge body =  
    	(case body
			of E.Lift(e1)           => E.Lift(rewriteMerge e1)
			| E.Comp(e1, es)        => E.Comp(rewriteMerge e1, es)
			| E.Sum(op1, e1)        => E.Sum(op1, rewriteMerge e1)
			| E.Op1(op1, e1)        => E.Op1(op1, rewriteMerge e1)
			| E.Op2(op2, e1, e2)    => E.Op2(op2, rewriteMerge e1, rewriteMerge e2)
			| E.Opn(E.Prod, E.Opn(E.Prod, ps)::es) =>  rewriteMerge(iterP(ps@es))
			| E.Opn(E.Prod, E.Opn(E.Add, ps)::es)
				=>  rewriteMerge (E.Opn(E.Add, List.map (fn  e1 => rewriteMerge(iterP(e1::es))) ps))
			| E.Opn(E.Add , ps) 		=>  iterA(List.map rewriteMerge ps)
			| E.Opn(E.Swap id , ps) 	=>  iterA(List.map rewriteMerge ps)
			| E.Opn(E.Prod, ps)         =>  rewriteMergeProd (List.map rewriteMerge ps)
		    | _            			    => body
		(* end case*))
		
		
	(*apply differentiation*)
    fun rewriteDifferentiate body = (case body
		of E.Apply (E.Partial [], e)       => e  
		| E.Apply(E.Partial (d1::dx), e)   =>
			let 
				(* differentiate *)
				val e = DerivativeEin.differentiate ([d1], e)
			in rewriteDifferentiate (E.Apply(E.Partial dx, e)) end 	    
		| E.Op1(op1, e1)                => E.Op1(op1, rewriteDifferentiate e1)
		| E.Op2(op2, e1,e2)             => E.Op2(op2, rewriteDifferentiate e1, rewriteDifferentiate e2)
		| E.Opn(opn, es)                => E.Opn(opn, List.map rewriteDifferentiate es)
		| _                             => body
		(* end case*))
  end
