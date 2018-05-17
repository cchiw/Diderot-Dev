(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin3 : sig

    val mergeD :Ein.ein_exp -> Ein.ein_exp

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

  end
