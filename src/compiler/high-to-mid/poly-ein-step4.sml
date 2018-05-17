(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin4 : sig

    val normalize :  Ein.ein_exp * Ein.mu list -> Ein.ein_exp

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


  end
