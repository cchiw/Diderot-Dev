(* ein-to-scalar.sml
 *
 * Generate LowIR scalar computations that implement Ein expressions.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure EinToScalar : sig

  (* expand a scalar-valued Ein operator application to LowIR code; return the LowIR
   * vaiable that holds the result of the application (the assignments will be added
   * to avail).
   *)
    val expand : {
            avail : AvailRHS.t,                 (* the generated LowIR assignments *)
            mapp : int IntRedBlackMap.map,      (* mapping from deBruijn indices to argument IDs *)
            body : Ein.ein_exp,                 (* the EIN operator body *)
            lowArgs : LowIR.var list            (* corresponding LowIR arguments *)
          } -> LowIR.var

  end = struct

    structure IR = LowIR
    structure Ty = LowTypes
    structure Op = LowOps
    structure Var = LowIR.Var
    structure E = Ein
    structure Mk = MkLowIR
    structure IMap = IntRedBlackMap

    fun mapIndex (mapp, id) = (case IMap.find(mapp, id)
           of SOME x => x
            | NONE => raise Fail(concat["mapIndex(_, V ", Int.toString id, "): out of bounds"])
          (* end case *))
	fun mkProd es = E.Opn(E.Prod, es)
    fun unwrap_poly (mapp, E.Poly(t, n, dx)) = 
    	let
    		val zero = E.Const 0
			val one = E.Const 1
			val ec = E.Const n
			val ecc = E.Const (n-1)
			val eccc = E.Const (n-2)
			val ndel = length(dx)
			(*Take a certain number of derivatives*)
			fun getDel () = 
				if(n<ndel) then zero
				else (case (ndel,n)
					of (0, _) => mkProd (List.tabulate (n, fn _ => t))
					| (1, 1) => one 
					| (1, _) => mkProd (ec::(List.tabulate (n-1, fn _ => t)))	
					| (2, 2) => ec
					| (2, _) => mkProd (ec::ecc::(List.tabulate (n-2, fn _ => t)))
					| (3, 3) => mkProd ([ec,ecc]) 
					| (3, _) => mkProd ([ec,ecc,eccc]@(List.tabulate (n-3, fn _ => t)))
					| _ =>  raise Fail"add more cases for derivative"
					(*end case*))
			val shape = (case t
				of E.Tensor(_,alpha) => alpha
				| E.Seq (_,_,alpha) => alpha
				(*end case*))
		in 
		 (case (shape, dx)
			of ([],_) =>  getDel () 
			 | ([E.C c], _) =>
				let
					fun iter [] = getDel () 
					| iter (vx::es) = if(Mk.lookupMu (mapp, vx)=c) then iter(es) else zero
				in 
					iter (dx)
				end
			(*end case*))
        end          
                        
    fun expand {avail, mapp, body, lowArgs} = let
            (*val _ = (String.concat["\nin expand:", EinPP.expToString(body)])*)
(*
            val i = Mk.lookupMu (mapp, E.V 0)
val _ = (String.concat["\nin direction i:",Int.toString(i)])*)
(*
              val j = Mk.lookupMu (mapp, E.V 1)
val _ = (String.concat["\nin direction i:",Int.toString(i),"-",Int.toString(j)])
*)
          fun gen (mapp, body) = let
              (*********sumexpression ********)
                fun tb n =  List.tabulate (n, fn e => e)
                fun sumCheck (mapp, (v, lb, ub) :: sumx, e) = let
                      fun sumloop mapp = gen (mapp, e)
                      fun sumI1 (left, (v, [i], lb1), [], rest) = let
                            val mapp = IMap.insert (left, v, lb1+i)
                            val vD = gen (mapp, e)
                            in
                              rest@[vD]
                            end
                        | sumI1 (left, (v, i::es, lb1), [], rest) = let
                            val mapp = IMap.insert (left, v, i+lb1)
                            val vD = gen (mapp, e)
                            in
                              sumI1 (mapp, (v, es, lb1), [], rest@[vD])
                            end
                        | sumI1 (left, (v, [i], lb1), (a, lb2, ub2) ::sx, rest) =
                            sumI1 (IMap.insert (left, v, lb1+i), (a, tb (ub2-lb2+1), lb2), sx, rest)
                        | sumI1 (left, (v, s::es, lb1), (a, lb2, ub2) ::sx, rest) = let
                            val mapp = IMap.insert (left, v, s+lb1)
                            val xx = tb (ub2-lb2+1)
                            val rest' = sumI1 (mapp, (a, xx, lb2), sx, rest)
                            in
                              sumI1 (mapp, (v, es, lb1), (a, lb2, ub2) ::sx, rest')
                            end
                        | sumI1 _ = raise Fail "None Variable-index in summation"
                      in
                        sumI1 (mapp, (v, tb (ub-lb+1), lb), sumx, [])
                      end
                in
                  case body
                   of E.Value v => Mk.intToRealLit (avail, mapIndex (mapp, v))
                    | E.Const c => Mk.intToRealLit (avail, c)
                    | E.Delta(i, j) => Mk.delta (avail, mapp, i, j)
                    | E.Epsilon(i, j, k) => Mk.epsilon3 (avail, mapp, i, j, k)
                    | E.Eps2(i, j) => Mk.epsilon2 (avail, mapp, i, j)
                    | E.Tensor(id, ix) => Mk.tensorIndex (avail, mapp, List.nth(lowArgs, id), ix)
                    | E.Seq(id, mu, alpha) =>  (*ID{mu}[alpha]*)
                    	Mk.seqTensorIndex (avail, mapp, List.nth(lowArgs, id), mu, alpha)
                    | E.Zero _ =>  Mk.intToRealLit (avail, 0)
                    | E.Op1(op1, e1) => let
                        val arg = gen (mapp, e1)
                        in
                          case op1
                           of E.Neg => Mk.realNeg (avail, arg)
                            | E.Sqrt => Mk.realSqrt (avail, arg)
                            | E.Cosine => Mk.realCos (avail, arg)
                            | E.ArcCosine => Mk.realArcCos (avail, arg)
                            | E.Sine => Mk.realSin (avail, arg)
                            | E.ArcSine => Mk.realArcSin (avail, arg)
                            | E.Tangent => Mk.realTan (avail, arg)
                            | E.ArcTangent => Mk.realArcTan (avail, arg)
                            | E.Exp => Mk.realExp (avail, arg)
                            | E.PowInt 0 => Mk.intToRealLit (avail, 1)
                            | E.PowInt n => Mk.intPow (avail, arg, n)
                            | E.Abs => Mk.realAbs(avail, arg)
                            | E.Sgn => Mk.realSgn(avail, arg)
                          (* end case *)
                        end

                    | E.Op2(E.Sub, e1, e2) => Mk.realSub (avail, gen (mapp, e1), gen (mapp, e2))
                    | E.Op2(E.Max, e1, e2) => Mk.realMax (avail, gen (mapp, e1), gen (mapp, e2))
                    | E.Op2(E.Min, e1, e2) => Mk.realMin (avail, gen (mapp, e1), gen (mapp, e2))
                    | E.Op2(E.Div, e1 as E.Tensor (_, [_]), e2 as E.Tensor (_, [])) =>
                        gen (mapp, E.Opn(E.Prod, [E.Op2 (E.Div, E.Const 1, e2), e1]))
                    | E.Op2(E.Div, e1, e2) => Mk.realDiv (avail, gen (mapp, e1), gen (mapp, e2))
                    | E.Op3(E.Clamp, e1, e2, e3) =>
                        Mk.realClamp(avail, gen(mapp, e1), gen(mapp, e2), gen(mapp, e3))
                    | E.Opn(E.Add, es) =>
                        Mk.reduce (avail, Mk.realAdd, List.map (fn e => gen(mapp, e)) es)
                    | E.OpR(opn, sumx, e) => 
                    	let 
                    	val op2 = (case opn
								of E.Add => Mk.realAdd
								| E.Prod => Mk.realMul
								| E.MaxN => Mk.realMax
								| E.MinN => Mk.realMin
                    		(* end case *))
                        in Mk.reduce (avail, op2, sumCheck (mapp, sumx, e)) end
                    | E.Opn(E.Prod, es) =>
                        Mk.reduce (avail, Mk.realMul, List.map (fn e => gen(mapp, e)) es)
                    | E.Opn(E.Swap id, args) =>  let
                        val id' = List.nth(lowArgs, id)
                        val args' = List.map (fn e=> gen (mapp, e)) args
                        in (case args'
                            of [a,b] =>  Mk.swap2(avail,id',a,b)
                            | [a,b,c] =>  Mk.swap3(avail,id',a,b,c)
                            | [a,b,c,d] =>  Mk.swap4(avail,id',a,b,c,d)
                            | [a,b,c,d,e] =>  Mk.swap5(avail,id',a,b,c,d,e)
                            | [a,b,c,d,e,f] =>  Mk.swap6(avail,id',a,b,c,d,e,f)
                            (*end case*))
                        end
                    | E.Sum(sx, E.Opn(E.Prod, (img as E.Img _) :: (kargs as (E.Krn _ :: _)))) =>
                        FieldToLow.expand {
                            avail = avail, mapp = mapp,
                            sx = sx, img = img, krnargs = kargs,
                            args = lowArgs
                          }
                    | E.Sum(sumx, e) =>
                        Mk.reduce (avail, Mk.realAdd, sumCheck (mapp, sumx, e))
                    | E.Probe(E.Epsilon e1, e2,_) => gen(mapp,E.Epsilon e1)
                    | E.Probe(E.Eps2 e1, e2,_) => gen(mapp,E.Eps2 e1)
                    | E.Probe(E.Const e1, e2,_) => gen(mapp, E.Const e1)
                    | E.Probe(E.Delta e1, e2,_) => gen(mapp, E.Delta e1)
                    | E.Probe e => raise Fail("probe ein-exp: " ^ EinPP.expToString body)
                    | E.Field _ => raise Fail("field should have been replaced: " ^ EinPP.expToString body)                        
                    | E.Poly _ =>   gen(mapp, unwrap_poly(mapp, body))
                    | E.EvalFem (fnspace, [g, E.Tensor(idt, _)]) => (case g
                        of E.Inverse(_,E.BigF(_, idc, dx))
                            =>  Mk.ProbeInvF(avail, mapp, fnspace, dx, List.nth(lowArgs, idc), List.nth(lowArgs, idt))
                         | E.BigF(_, idc, dx)
                            =>  Mk.ProbeF(avail, mapp, fnspace,  dx, List.nth(lowArgs, idc), List.nth(lowArgs, idt))
                         | _ => raise Fail("unsupported ein-exp: " ^ EinPP.expToString body)
                        (* end case *))
                    | E.EvalFem(fnspace, [phi, g, pos])  => (case phi
                        of E.Basis(_, idb, dx)     =>
                            let
                                val vG = gen(mapp, E.EvalFem(fnspace, [g, pos]))
                                in  Mk.ProbePhi(avail, mapp, fnspace, dx, List.nth(lowArgs, idb), vG)
                            end
                         | _ => raise Fail("unsupported ein-exp: " ^ EinPP.expToString body)
                        (* end case*))
                    | E.If(E.Compare(op1, e1, e2), e3, e4) =>
                        let
                            val vA = gen (mapp, e1)
                            val vB = gen (mapp, e2)
                        	val vC = (case op1
                        		of E.GT => Mk.boolGT(avail, vA, vB)
                        		 | E.LT => Mk.boolLT(avail, vA, vB)
                        	(* end case*))
                        in
                            Mk.realIf(avail, vC, gen (mapp, e3), gen (mapp, e4))
                        end
                    | E.If(E.Var id, e3, e4) 
                        => Mk.realIf(avail, List.nth(lowArgs, id), gen (mapp, e3), gen (mapp, e4))
                  | _ => raise Fail("unsupported ein-exp: " ^ EinPP.expToString body)
                  (*end case*)
                end
          val rtn = gen (mapp, body)
          in rtn 
          end

    end
