(* float-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure FloatEin : sig

    val transform : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assign list
    val liftPos : Ein.ein * MidIR.var list ->(MidIR.var * MidIR.rhs) list  * Ein.ein * MidIR.var list
    
  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein

    fun cut (name, origProbe, params, index, sx, argsOrig, avail, newvx) = let
        (* clean and rewrite current body *)

          val (tshape1, sizes1, body1) = CleanIndex.clean (origProbe, index, sx)
          val id = length params
          val Rparams = params@[E.TEN(true, sizes1)]
          val y = V.new (concat[name, "_l_", Int.toString id], Ty.tensorTy sizes1)
          val IR.EINAPP(ein, args) = CleanParams.clean (body1, Rparams, sizes1, argsOrig@[y])
        (* shift indices in probe body from constant to variable *)
          val Ein.EIN{
                    body = E.Probe(E.Conv(V, [c1], h, dx), pos, ty), 
                    index = index0, 
                    params = params0
                } = ein
        (* only called with vector fields*)
          val E.IMG(dim, [i]) = List.nth(params0, V)
          val index1 = index0@[i]
          val unshiftedBody = E.Probe(E.Conv(V, [E.V newvx], h, dx), pos, ty)
        (* clean to get body indices in order *)
          val (_, sizes2, body2) = CleanIndex.clean (unshiftedBody, index1, [])
          val ein2 = E.EIN{params = params0, index =  sizes2, body = body2}

          val lhs = AvailRHS.addAssign (avail, "L", Ty.tensorTy sizes2, IR.EINAPP(ein2, args))
          val Rargs = argsOrig @ [lhs]
        (*Probe that tensor at a constant position  c1*)
          val nx = List.mapi (fn (i, _) => E.V i) dx
          val Re = E.Tensor(id, c1 :: tshape1)
          val Rparams = params @ [E.TEN(true, sizes2)]
          in
            (Re, Rparams, Rargs)
          end

    fun cut_ofield1 (name, origProbe, params, index, sx, argsOrig, avail, c1, shiftcx) = let
            (* clean and rewrite current body *)
            val (tshape1, sizes1, body1) = CleanIndex.clean (origProbe, index, sx)
            val id = length params
            val Rparams = params@[E.TEN(true, sizes1)]
            val y = V.new (concat[name, "_l_", Int.toString id], Ty.tensorTy sizes1)
            val IR.EINAPP(ein, args) = CleanParams.clean (body1, Rparams, sizes1, argsOrig@[y])
            (* shift indices in probe body from constant to variable *)
            val Ein.EIN{
                body = E.Probe(E.OField(ofield, E.Tensor(V, alpha), E.Partial dx), pos, ty), 
                index = index0, 
                params = params0
                } = ein
            (* only called with vector fields*)
            val E.FLD(dim, i::_) = List.nth(params0, V)
            val index1 = List.take(index0, shiftcx)@(i::List.drop(index0, shiftcx))


            val alphan = length(alpha)
            val alpha = List.tabulate(alphan, fn i=> E.V i )

            val dxn = length(dx)
            val dx = List.tabulate(dxn, fn i=> E.V (i+alphan))
            val unshiftedBody = E.Probe(E.OField(ofield, E.Tensor(V, alpha), E.Partial  dx), pos, ty)

            val _ = (String.concat["\n\n   unshiftedBody:", EinPP.expToString( unshiftedBody)])
            (* clean to get body indices in order *)
            val (_, sizes2, body2) = CleanIndex.clean (unshiftedBody, index1, [])
            val ein2 = E.EIN{params = params0, index =  sizes2, body = body2}

            val _ = (String.concat["\n\n  ein2:", EinPP.toString(ein2)])
            val lhs = AvailRHS.addAssign (avail, "L", Ty.tensorTy sizes2, IR.EINAPP(ein2, args))
            val Rargs = argsOrig @ [lhs]

            (*Probe that tensor at a constant position  c1*)
            (*add constant to placement shiftcx*)
            val tshape = List.take(tshape1, shiftcx)@(c1::List.drop(tshape1, shiftcx))
            val Re = E.Tensor(id, tshape)
            val Rparams = params @ [E.TEN(true, sizes2)]
        in
            (Re, Rparams, Rargs)
        end

   (* lift:ein_app*params*index*sum_id*args-> (ein_exp* params*args*code)
    * lifts expression and returns replacement tensor
    * cleans the index and params of subexpression
    * creates new param and replacement tensor for the original ein_exp
    *)
    fun lift (name, e, params, index, sx, args, avail) = let

          val (tshape, sizes, body) = CleanIndex.clean(e, index, sx)
          val id = length params
          val Re = E.Tensor(id, tshape)
          val einapp = CleanParams.clean (body, params, sizes, args)
          val lhs = AvailRHS.addAssign (
                avail, 
                concat[name, "_l_", Int.toString id], Ty.tensorTy sizes, 
                CleanParams.clean (body, params, sizes, args))
          in
            (Re, params @ [E.TEN(true, sizes)], args @ [lhs])
          end
    fun err str = raise Fail(String.concat["Ill-formed EIN Operator", str])
    fun mkProbe (exp as E.Probe(fld, x, ty)) =
       (case fld
            of E.Tensor _        => fld (*in case cfexp of tensor*)
            | E.Lift e           => e
            | E.Zero _           => fld
            | E.Partial _        => err "Probe Partial"
            | E.Probe _          => err "Probe of a Probe"
            | E.Value _          => err "Value used before expand"
            | E.Img _            => err "Probe used before expand"
            | E.Krn _            => err "Krn used before expand"
            | E.Comp _           => exp (* expanded in the next stage*)
            | E.Epsilon _        => fld
            | E.Eps2 _           => fld
            | E.Const _          => fld
            | E.Delta _          => fld
            | E.Sum(sx1, e)      => (E.Sum(sx1, mkProbe(E.Probe(e, x, ty))))
            | E.Op1(op1, e)      => (E.Op1(op1, mkProbe (E.Probe(e, x, ty))))
            | E.Op2(op2, e1, e2) => (E.Op2(op2, mkProbe(E.Probe(e1, x, ty)), mkProbe(E.Probe(e2, x, ty))))
            | E.Opn(opn, [])     => err "Probe of empty operator"
            | E.Opn(opn, es)     => (E.Opn(opn, List.map (fn e => mkProbe (E.Probe(e, x, ty))) es))
            | _                  => exp 
        (* end case*))
       |  mkProbe e = e

    (*
    * sx- outside summation, sx2-wrapped around probe
    *)
    fun compSingle (name, params, sx, sx2, args, avail, innerVar, A, index, index2, fld) = 
        let
            (* outer term in composition: newP, innerVar*)
            val id2 = length params
            val args2 = args@[innerVar]
            val newP2 = params@[E.TEN(true, index2)]
            val exp_pos = E.Tensor(id2, [])
            val exp_fld =(case fld
                of E.OField(E.CFExp tterm, _, dx) => E.OField(E.CFExp tterm, A, dx)
                | _  => A
                (*end case*))
            val exp_probe = mkProbe(E.Probe(exp_fld, [exp_pos], NONE))
            val outerExp2 = (case sx2
                of [] => exp_probe
                | _   => E.Sum(sx2, exp_probe)
                (* end case*))
            val (tshape2, sizes2, body2) = CleanIndex.clean(outerExp2, index, sx)
            val einapp2 = CleanParams.clean (body2, newP2, sizes2, args2)
            val IR.EINAPP(outerExp, _) = einapp2
            val _ = (String.concat["\n\n  outer:", EinPP.toString(outerExp)])
            val lhs = AvailRHS.addAssign (avail, concat[name, "l"], Ty.tensorTy sizes2, einapp2)

            (*replacement*)
            val Re = E.Tensor(id2, tshape2)
            in
              (Re, sizes2, lhs)
            end


    fun compn (name, exp, params, indexD, sx, sx2, args, avail) = 
        let
            val E.Probe(fld, pos, ty) = exp
            val (D, es) = (case fld
                of E.Comp e => e
                |  E.OField(E.CFExp tterm, E.Comp e, dx) => e
                (*end case*))
            fun iter(sizes, lhs, [], avail) = compSingle(name, params, sx, sx2, args, avail, lhs, D, indexD, sizes, fld)
              | iter(sizes, lhs, (B, indexB)::ns, avail) = let
                val (_, sizes2, lhs2) = compSingle(name, params, [], [], args, avail, lhs, B, indexB, sizes, fld)
                in
                  iter(sizes2, lhs2, ns, avail)
                end
            val es' = List.rev(es)
            (* inner term in composition *)
            val (A, indexA) = List.hd(es')
            val wrapA = (case fld
                of E.OField(E.CFExp tterm, _, dx) => E.OField(E.CFExp tterm, A, dx)
                | _  => A
                (*end case*))
            val innerExp = E.EIN{params = params, index =  indexA, body = E.Probe(wrapA, pos, ty)}
            val innerVar = AvailRHS.addAssign (avail, "Inner", Ty.tensorTy indexA, IR.EINAPP(innerExp, args))
            val _ = (String.concat["\n\n", V.name(innerVar), " = ", EinPP.toString(innerExp)])
            (*other  composition *)
            val (Re, sizes, lhs) = iter(indexA, innerVar, List.tl(es'), avail)
            val _ = (String.concat["\n\n", V.name(lhs), " = ", EinPP.expToString(Re)])
            in
              (Re, params @ [E.TEN(true, sizes)], args @ [lhs])
            end


    fun isOp e = (case e
          of E.Op1 _    => true
           | E.Op2 _    => true
           | E.Op3 _    => true
           | E.Opn _    => true
           | E.Sum _    => true
           | E.Probe _  => true
           | _          => false
         (* end case *))

    fun transform (y, ein as Ein.EIN{body=E.Probe (E.Conv _, _, ty), ...}, args) =
          [(y, IR.EINAPP(ein, args))]
      | transform (y, ein as Ein.EIN{body=E.Sum(_, E.Probe (E.Conv _, _, ty)), ...}, args) =
          [(y, IR.EINAPP(ein, args))]
      | transform (y, ein as Ein.EIN{body=E.Tensor _, ...}, args) =
            [(y, IR.EINAPP(ein, args))]
      | transform (y, Ein.EIN{params, index, body}, args) = let
          val avail = AvailRHS.new()


          fun filterOps (es, params, args, index, sx) = let
                fun filter ([], es', params, args) = (rev es', params, args)
                  | filter (e::es, es', params, args) = if isOp e
                      then let
                        val (e', params', args') = lift("op1_e3", e, params, index, sx, args, avail)
                        in
                          filter (es, e'::es', params', args')
                        end
                      else filter (es, e::es', params, args)
                in
                  filter (es, [], params, args)
                end
        fun rewrite (sx, exp, params, args) = (case exp
            of E.Probe(fld, pos, ty) => (case fld
               of E.Conv(_, [E.C _], _, [])         => cut ("cut", exp, params, index, sx, args, avail, 0)
                | E.Conv(_, [E.C _ ], _, [E.V 0])   => cut ("cut", exp, params, index, sx, args, avail, 1)
                | E.Conv(_, [E.C _ ], _, [E.V 0, E.V 1])
                                                    => cut ("cut", exp, params, index, sx, args, avail, 2)
                | E.Conv(_, [E.C _ ], _, [E.V 0, E.V 1, E.V 2])
                                                    => cut ("cut", exp, params, index, sx, args, avail, 3)
                | E.OField(_, E.Tensor(_, alpha), dx) => lift ("probe", exp, params, index, sx, args, avail)
                | E.Comp(_, []) => raise Fail "nonsupported nest"
                | E.Comp(_, es)
                    => compn("composition", exp, params, index, sx, [], args, avail)
                | E.OField(E.CFExp tterm, E.Comp _, dx)
                     => raise Fail "cfexp of a composition not allowed"
                    (* compn("poly-wrap-composition", exp, params, index, sx, [], args, avail)*)
                |  _ => (case (mkProbe exp)
                    of E.Probe _ => lift ("probe", exp, params, index, sx, args, avail)
                    | exp       => rewrite(sx, exp, params, args))
                    (* end case*))
			| E.Sum(sx2, e) => (("\n Mark A Sum exp:"^EinPP.expToString(exp));case e
                of E.Probe(E.Comp(_, []), _, ty) =>  raise Fail ("nonsupported nest")
                 | E.Probe(E.Comp(_, es), _, ty) =>  compn("composition", e, params, index, sx, sx2, args, avail)
            	 | E.Probe ( _, _, ty) => ("\nMark C";lift ("sumprobe", exp, params, index, sx, args, avail))
                 | _ => let
                    val _ = "\nMark D"
                    val (e', params', args') = rewrite (sx2@sx, e, params, args)
                    in
                        (E.Sum(sx2, e'), params', args')
                    end
                (*end case*))
            | E.Op1(op1, e1) => let
                  val (e1', params', args') = rewrite (sx, e1, params, args)
                  val ([e1], params', args') = filterOps ([e1'], params', args', index, sx)
                  in
                    (E.Op1(op1, e1), params', args')
                  end
            | E.Op2(E.Div, E.Const 1, e2) => let
                  val e1 = E.Const 1
                  val (e1', params', args') = rewrite (sx, e1, params, args)
                  val (e2', params', args') = rewrite (sx, e2, params', args')
                  val ([e1', e2'], params', args') =
                    filterOps ([e1', e2'], params', args', index, sx)
                  in
                    (E.Op2(E.Div, e1', e2'), params', args')
                  end

            | E.Op2(op2, e1, e2) => let
                  val (e1', params', args') = rewrite (sx, e1, params, args)
                  val (e2', params', args') = rewrite (sx, e2, params', args')
                  val ([e1', e2'], params', args') =
                        filterOps ([e1', e2'], params', args', index, sx)
                  in
                    (E.Op2(op2, e1', e2'), params', args')
                  end
            | E.Op3(op3, e1, e2, e3) => let
                  val (e1', params', args') = rewrite (sx, e1, params, args)
                  val (e2', params', args') = rewrite (sx, e2, params', args')
                  val (e3', params', args') = rewrite (sx, e3, params', args')
                  val ([e1', e2', e3'], params', args') =
                    filterOps ([e1', e2', e3'], params', args', index, sx)
                  in
                    (E.Op3(op3, e1', e2', e3'), params', args')
                  end
            | E.Opn(opn, es) => let
                  fun iter ([], es, params, args) = (List.rev es, params, args)
                    | iter (e::es, es', params, args) = let
                        val (e', params', args') = rewrite (sx, e, params, args)
                        in
                          iter (es, e'::es', params', args')
                        end
                  val (es, params, args) = iter (es, [], params, args)
                  val (es, params, args) = filterOps (es, params, args, index, sx)
                  in
                    (E.Opn(opn, es), params, args)
                  end
            | E.If(E.Var id, e3, e4) => let
                val (e3', params', args') = rewrite (sx, e3, params, args)
                val (e4', params', args') = rewrite (sx, e4, params', args')
                val ([e3', e4'], params', args') =
                        filterOps ([e3', e4'], params', args', index, sx)
                val comp' = E.Var id
 
                in
                    (E.If(comp', e3', e4'), params', args')
                end
            | E.If(E.Compare(op1, e1, e2), e3, e4) => let
                val (e1', params', args') = rewrite (sx, e1, params, args)
                val (e2', params', args') = rewrite (sx, e2, params', args')
                val (e3', params', args') = rewrite (sx, e3, params', args')
                val (e4', params', args') = rewrite (sx, e4, params', args')
                val ([e1', e2', e3', e4'], params', args') =
                filterOps ([e1', e2', e3', e4'], params', args', index, sx)
                in
                    (E.If(E.Compare(op1, e1', e2'), e3', e4'), params', args')
                end
            | _ => (exp, params, args)
                (* end case *))
          val (body', params', args') = rewrite ([], mkProbe (body), params, args)
          val einapp = CleanParams.clean (body', params', index, args')
          in
            List.rev ((y, einapp) :: AvailRHS.getAssignments avail)
          end

	fun liftPos(E.EIN{body as E.Probe(f,[pos],ty), params, index}, args) =
        let
            val _ = (String.concat["\n pull position:",EinPP.expToString(body)])
            val shape = []
            val vP = V.new ("posoutside", Ty.tensorTy shape)
            val ein = Ein.EIN{body=pos, params=params, index= shape}
            val stmt = transform(vP, ein, args)
            val body' = E.Probe(f, [E.Tensor(List.length(args),[])], ty)
            val params' = params@[E.TEN(true, shape)]
            val args' =  args@[vP]
            val ein = Ein.EIN{body=body', params=params', index=index}
            in 
                (stmt, ein, args')   (*@PolyEin.transform(y, ein, args@[vP]) *)
            end 
            
  end
