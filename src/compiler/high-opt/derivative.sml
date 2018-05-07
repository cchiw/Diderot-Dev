(* derivative.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * FIXME: this file needs documentation
 *)

structure Derivative : sig

    val mkApply : Ein.ein_exp * Ein.ein_exp * Ein.index_bind list *Ein.param_kind list * Ein.index_id -> Ein.ein_exp option

  end  = struct

    structure E = Ein

    fun err str=raise Fail (String.concat["Ill-formed EIN Operator: ", str])

    fun mkAdd exps = E.Opn(E.Add, exps)
    fun mkSub (e1, e2) = E.Op2(E.Sub, e1, e2)
    fun mkProd exps = E.Opn(E.Prod, exps)
    fun mkDiv (e1, e2) = E.Op2(E.Div, e1, e2)
    fun mkNeg e = E.Op1(E.Neg, e)
    fun mkAbs e = E.Op1(E.Abs, e)

    fun filterProd args = (case EinFilter.mkProd args
           of SOME e => e
            | NONE => mkProd args
          (* end case *))

    fun rewriteProd [a] = a
      | rewriteProd exps = E.Opn(E.Prod, exps)

  (* chain rule *)
    fun prodAppPartial ([], _) = err "Empty App Partial"
      | prodAppPartial ([e1], p0) = E.Apply(p0, e1)
      | prodAppPartial (e::es, p0) = let
          val l = prodAppPartial (es, p0)
          val e2' = filterProd [e, l]
          val e1' = filterProd (es @ [E.Apply(p0, e)])
          in
            mkAdd[e1', e2']
          end

    fun applyOp1 (op1, e1, dx) = let
          val d0::dn = dx
          val px = E.Partial dx
          val inner = E.Apply(E.Partial[d0], e1)
          val square = mkProd [e1, e1]
          val one = E.Const 1
          val e2 = mkDiv(one, E.Op1(E.Sqrt, mkSub(one, square)))
          fun iterDn e2 = if null dn then e2 else E.Apply(E.Partial dn, e2)
          in 
            case op1
             of E.Neg => mkNeg(E.Apply(px, e1))
              | E.Exp => iterDn (mkProd [inner, E.Op1(E.Exp, e1)])
              | E.Sqrt => let
                  val half = mkDiv (E.Const 1, E.Const 2)
                  val e3 = mkDiv (inner, E.Op1(op1, e1))
                  in
                    case dn
                     of [] => mkProd [half, e3]
                      | _ => mkProd [half, E.Apply(E.Partial dn, e3)]
                    (* end case *)
                  end
              | E.Cosine => iterDn (mkProd [mkNeg (E.Op1(E.Sine, e1)), inner])
              | E.ArcCosine => iterDn (mkProd [mkNeg e2, inner])
              | E.Sine => iterDn (mkProd [E.Op1(E.Cosine, e1), inner])
              | E.ArcSine => iterDn (mkProd [e2, inner])
              | E.Tangent =>
                  iterDn (mkProd [mkDiv(one, mkProd[E.Op1(E.Cosine, e1), E.Op1(E.Cosine, e1)]), inner])
              | E.ArcTangent =>
                  iterDn (mkProd [mkDiv(one, mkAdd[one, square]), inner])
              | E.PowInt n => iterDn (mkProd [E.Const n, E.Op1(E.PowInt(n-1), e1), inner])
              | E.Abs => iterDn  (mkProd [inner, E.Op2(E.Div, e1, E.Op1(E.Abs, e1))])
          (* end case *)
        end

    fun applyOp2 (op2, e1, e2, dx) = let
          val (d0::dn) = dx
          val p0 = E.Partial [d0]
          val inner1 = E.Apply(E.Partial[d0], e1)
          val inner2 = E.Apply(E.Partial[d0], e2)
          val zero = E.Const 0
          fun iterDn e2 = if null dn then e2 else E.Apply(E.Partial dn, e2)
          in
            case op2
             of E.Sub => mkSub (inner1, inner2)
             | E.Max =>  let
                val comp = E.GT(e1, e2)
                val exp  = E.If(comp, inner1, inner2)
                in iterDn exp end
             | E.Min =>  let
                val comp = E.LT(e1, e2)
                val exp  = E.If(comp, inner1, inner2)
                in iterDn exp end
              | E.Div => (case (e1, e2)
                   of (_, E.Const e2) => mkDiv (inner1, E.Const e2)
                    | (E.Const 1, _) => (case EinFilter.partitionField [e2]
                         of (_, []) => zero
                          | (pre, h) => let (* Quotient Rule *)
                              val h' = E.Apply(p0, rewriteProd h)
                              val num = mkProd [E.Const ~1, h']
                              in
                                iterDn (mkDiv (num, mkProd (pre @ h @ h)))
                              end
                        (* end case *))
                    | (E.Const c, _) => (case EinFilter.partitionField [e2]
                         of (_, []) => zero
                          | (pre, h) => let (* Quotient Rule *)
                              val h' = E.Apply(p0, rewriteProd h)
                              val num = mkNeg (mkProd [E.Const c, h'])
                              in
                                iterDn (mkDiv (num, mkProd (pre@h@h)))
                              end
                        (* end case *))
                    | _ => (case EinFilter.partitionField [e2]
                         of (_, []) => mkDiv (inner1, e2) (* Division by a real *)
                          | (pre, h) => let (* Quotient Rule *)
                              val g' = inner1
                              val h' = E.Apply(p0, rewriteProd h)
                              val num = mkSub (mkProd (g' :: h), mkProd[e1, h'])
                              in
                                iterDn (mkDiv (num, mkProd (pre@h@h)))
                              end
                        (* end case *))
                  (* end case *))
            (* end case *)
          end

    fun applyOpn (opn, es, dx) = let
          val (d0::dn) = dx
          val p0 = E.Partial [d0]
          fun iterDn e2 = if null dn then e2 else E.Apply(E.Partial dn, e2)
          in
            case opn
             of E.Add => mkAdd (List.map (fn a => E.Apply(E.Partial dx, a)) es)
              | E.Prod => let
                  val (pre, post) = EinFilter.partitionField es
                  in
                    case post
                     of [] => E.Const 0 (* no fields in expression *)
                      | _ => iterDn (filterProd (pre @ [prodAppPartial (post, p0)]))
                    (* end case *)
                  end
            |  E.Swap id => E.Opn(E.Swap id,  (List.map (fn a => E.Apply(E.Partial dx, a)) es))
            (* end case *)
          end

    (* rewrite free index (i) in  expression e to vk *)
    fun rewriteIx(vk, e) = let
        (* the variable index inside a composition will start at 0*)
        fun change(E.V 0) = E.V vk
          | change ix     = ix
        in (case e
            of E.Const _        => e
            | E.Tensor(id2, alpha)
                                => E.Tensor(id2, List.map (fn e=> change(e)) alpha)
            | E.Zero (alpha)    => E.Zero(List.map (fn e => change(e)) alpha)
            | E.Delta (i,j)     => E.Delta(change i, change j)
            | E.Epsilon (i,j,k) => E.Epsilon(change i, change j, change k)
            | E.Eps2 (i,j)      => E.Eps2(change i, change j)
            | E.Field(id2, alpha)
                                => E.Field(id2, List.map (fn e => change(e)) alpha)
            | E.Lift e1         => E.Lift(rewriteIx(vk, e1))
            | E.Conv(id2, alpha, h2, dx2)
                                => E.Conv(id2, List.map (fn e => change(e)) alpha, h2, dx2)
            | E.Partial(alpha)
                                => E.Partial (List.map (fn e => change(e)) alpha)
            | E.Apply(e1, e2)   => E.Apply(e1, rewriteIx(vk, e2))
            | E.Probe(e1, e2)   => E.Probe(rewriteIx(vk, e1), e2)
            | E.Comp(e1, es) => E.Comp(rewriteIx(vk, e1), es)
            | E.Sum(sx, e1)
                                => E.Sum(sx, rewriteIx(vk, e1))
            | E.Op1(op1, e1)
                                => E.Op1(op1, rewriteIx(vk, e1))
            | E.Op2(op2, e1, e2)
                                => E.Op2(op2, rewriteIx(vk, e1), rewriteIx(vk, e2))
            | E.Opn(opn, es)
                                => E.Opn(opn, List.map (fn e => rewriteIx(vk, e)) es)
            | E.OField(E.BuildFem (a,b), e1, dx) => E.OField(E.BuildFem (a,b), rewriteIx(vk, e1), dx)

            |  _ => e
            (*end case*))
        end

    fun findDim(e, params) = (case e
            of E.Const _            => NONE
            | E.Tensor _            => NONE
            | E.Zero _              => NONE
            | E.Delta _             => NONE
            | E.Epsilon _           => NONE
            | E.Eps2 _              => NONE
            | E.Lift _              => NONE
            | E.Field(id, _)   => let
                val E.IMG(d,_) = List.nth(params, id)
                in SOME(d) end
            | E.Conv(id, _, _, _)   => let
                val E.IMG(d,_) = List.nth(params, id)
                in SOME(d) end

            | E.Partial _           => NONE
            | E.Apply(e1, e2)       => findDim(e2, params)
            | E.Probe(e1, e2)       => findDim(e1, params)
            | E.Comp(e1, _)     => findDim(e1, params)
            | E.Sum(_, e1)          => findDim(e1, params)
            | E.Op1(_, e1)          => findDim(e1, params)
            | E.Op2(_, e1, e2)      =>
                (case findDim(e1, params)
                    of NONE => findDim(e2, params)
                    |  e  => e
                (*end case *))
            | E.Opn(_, es)          => let
                fun iter([]) = NONE
                  | iter(e1::es) =
                    (case findDim(e1, params)
                        of NONE => iter(es)
                        |  e  => e
                        (*end case *))
                in iter(es) end
            | E.OField(E.BuildFem _, E.Tensor(id,_), _) =>
                    let
                    val E.FLD(d,_) = List.nth(params, id)
                    in SOME(d) end
            |  _ => raise Fail(String.concat["find Dim does not handle: ", EinPP.expToString(e)])
        (* end case*))
  (* rewrite Apply nodes *)
    fun mkApply (px as E.Partial dx, e, index, params, sumX) = let
          val (d0::dn) = dx
          val p0 = E.Partial[d0]
          fun iterDn e2 = if null dn then e2 else E.Apply(E.Partial dn, e2)
          val zero = E.Const 0
          in
            case e
             of E.Const _ => SOME zero
              | E.ConstR _ => SOME zero
              | E.Tensor _ => err "Tensor without Lift"
              | E.Zero _ => err "Zero without Lift"
              | E.Delta _ => SOME zero
              | E.Epsilon _ => SOME zero
              | E.Eps2 _ => SOME zero
              | E.Field _ => NONE
              | E.Lift _ => SOME zero
              | E.Conv(v, alpha, h, d2) => SOME(E.Conv(v, alpha, h, d2@dx))
              | E.Partial _ => err("Apply of Partial")
              | E.Apply(E.Partial d2, e2) => SOME(E.Apply(E.Partial(dx@d2), e2))
              | E.Apply _ => err "Apply of non-Partial expression"
              | E.Probe _ => err "Apply of Probe"
              | E.Comp(e1, [(e2, n)]) =>
                let
                val E.V ii = d0
                val vk =  100+sumX (* need to fix*)
                val px' = E.Partial[E.V vk]
                val e3 = E.Comp(E.Apply(px', e1), [(e2, n)])
                val e5 = rewriteIx(vk, e2)
                val e4 = E.Apply(p0, e5) (* should p0 here be rewritten? or shifted base don the length of n  *)
                val SOME(d)  = findDim(e1, params)
                val en = E.Sum([(vk, 0, d-1)], E.Opn(E.Prod, [e3, e4]))
                (*val _ = (String.concat["\napply derivative:\n\t","think new dx is ", Int.toString(vk),"-",EinPP.expToString(E.Apply(px,e )), "\n\t===>\n,",EinPP.expToString(en)])*)
                in SOME (iterDn en) end
              | E.Comp _ => err "unsupported differentiation of comp"
              | E.Value _ => err "Value used before expand"
              | E.Img _ => err "Probe used before expand"
              | E.Krn _ => err "Krn used before expand"
              | E.Sum(sx, e1) => SOME(E.Sum(sx, E.Apply(px, e1)))
              | E.Op1(op1, e1) => SOME(applyOp1(op1, e1, dx))
              | E.Op2(op2, e1, e2) => SOME(applyOp2(op2, e1, e2, dx))
              | E.Op3(op3, e1, e2, e3) => SOME zero (*assume clamp is not lifted*)
              | E.Opn(opn, es) => SOME(applyOpn(opn, es, dx))
              | E.OField(ofld, e2,  E.Partial alpha) => SOME(E.OField(ofld, e2,  E.Partial (alpha@dx)))
              | E.If(comp, e3, e4) => SOME(E.If(comp, E.Apply(px, e3), E.Apply(px, e4)))
            (* end case *)
          end

  end
