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

    fun p1(id, cx, vx) = E.Opn(E.Prod, [E.Poly(id, [E.C cx], 1, []), E.Delta(E.C cx, E.V vx)])
    fun mkpos(id, vx,dim) = E.Opn(E.Add, List.tabulate(dim, (fn n => p1(id, n, vx))))
    fun mkposScalar(id) =  E.Poly(id, [], 1, [])

    fun iterP([], [r]) = r
    | iterP ([], rest) = E.Opn(E.Prod, rest)
    | iterP (E.Const 0::es, rest) = E.Const(0)
    | iterP (E.Const 1::es, rest) = iterP(es, rest)
    | iterP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) =
        (* variable can't be 0 and 1 '*)
        if(c1=c2 orelse (not (v1=v2)))
        then iterP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
        else  E.Const(0)
    | iterP(E.Opn(E.Prod, ys)::es, rest) = iterP(ys@es, rest)
    | iterP (e1::es, rest)   = iterP(es, e1::rest)


    fun iterA([], []) = E.Const 0
    | iterA([], [r]) = r
    | iterA ([], rest) = E.Opn(E.Add, rest)
    | iterA (E.Const 0::es, rest) = iterA(es, rest)
    | iterA (E.Opn(E.Add, ys)::es, rest) = iterA(ys@es, rest)
    | iterA (e1::es, rest)   = iterA(es, e1::rest)


    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
    | paramToString (i, E.FLD (d, shp)) = concat["F", i2s i, "(", i2s d, ")[",shp2s shp , "]"]
    | paramToString (i, E.KRN) = "H" ^ i2s i
    | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]

    (********************************** Step 1 *******************************)
    (* replace tensor with delta expression and flatten products *)
    fun replaceD (body, dim, mapp) = let
        fun replace body = (case body
            of E.Const _            => body
            | E.ConstR _            => body
            | E.Zero _              => body
            | E.Delta _             => body
            | E.Epsilon _           => body
            | E.Eps2 _              => body
            | E.Field _             => body
            | E.Tensor(id, [])       => if (ISet.member(mapp, id)) then mkposScalar(id) else  body
            | E.Tensor(id, [E.V vx]) => if (ISet.member(mapp, id)) then mkpos(id,vx, dim) else body
            | E.Tensor _            => body
            | E.Poly _              => body
            | E.Lift(e1)            => E.Lift(replace e1)
            | E.Comp(e1, es)        => E.Comp(replace e1, es)
            | E.Sum(op1, e1)        => E.Sum(op1, replace e1)
            | E.Op1(E.PowInt n, e1)   => let
                val e = replace e1
                in
                    iterP (List.tabulate(n, fn _ => e),[])
                end
            | E.Op1(op1, e1)        => E.Op1(op1, replace e1)
            | E.Op2(op2, e1, e2)    => E.Op2(op2, replace e1, replace e2)
            | E.Opn(E.Prod, E.Opn(E.Prod, ps)::es) =>  replace(iterP(ps@es,[]))
            | E.Opn(E.Prod, E.Opn(E.Add, ps)::es)
                =>  replace (E.Opn(E.Add, List.map (fn  e1 => replace(iterP(e1::es,[]))) ps))
            | E.Opn(E.Prod, [_])            => raise Fail(EinPP.expToString(body))
            | E.Opn(E.Prod, ps)             => iterP(List.map replace ps , [])
            | E.Opn(E.Add , ps)             => iterA(List.map replace ps, [])
            | E.Opn(E.Swap id , ps)         => iterA(List.map replace (E.Tensor(id,[])::ps), [])
            | _                             => raise Fail(EinPP.expToString(body))
            (* end case*))
        val body' = replace body
        (*val _ = (String.concat ["\n\nreplaced :", EinPP.expToString(body), "\n\t->:", EinPP.expToString(body')])*)
        in body' end

    (*do the variable replacement for all the poly input variables *)
    fun polyArgs(args, params, pargs, start_idxs, e) =
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
                fun findArg(_, [], newargs, _, newparams, mapp) = (List.rev newargs, List.rev newparams, mapp)
                | findArg(id, e1::es, newargs, p1::ps, newparams, mapp) =
                    if(IR.Var.same(e1,arg_replaced))
                    then findArg(id+1,es, arg_new::newargs, ps, param_new::newparams, ISet.add(mapp, id))
                    else findArg(id+1,es, e1::newargs, ps ,p1::newparams, mapp)

                val (args, params, mapp) = findArg(0, args, [], params, [], ISet.empty)
                (*
                val _ = ("\n\nparams:"^String.concatWith "," (List.mapi paramToString params))
                val _ = ("\nargs:"^String.concatWith "," (List.map IR.Var.name  args))
                *)
            in (args, params, mapp) end

        (*iterate over all the poly variable expressions *)
        fun iter(_, args, params, [], e) = (args, params,e)
            | iter (pid::es, args, params, idx::idxs,e) =
            let
                val _ = (String.concat["\n replacing param id:",Int.toString(pid),"idx:",Int.toString(idx)])
                val (args, params, mapp) = replaceArg(args, params, idx, pid)
                (* get dimension of vector that is being broken into components*)
                val param_pos = List.nth(params, pid)
                val dim = (case param_pos
                    of E.TEN (_,[]) => 1
                    | E.TEN (_,[i]) => i
                (* end case*))
                (* replace position tensor with deltas in body*)
                val e = replaceD (e, dim, mapp)
                val e = replaceD (e, dim, mapp)
            in iter(es, args,params, idxs, e) end

        (*start_id: start of position variables for probe operation *)
        val (args, params, e) = iter(pargs, args, params, start_idxs, e)
        in (args, params, e) end
    (********************************** Step 2 *******************************)
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
        | E.Opn(E.Prod, ps)        => let
             val _ = (String.concat ["\n\n merged in product:", EinPP.expToString(body)])
            fun  iter([], rest, [], _) = iterP(rest,[])
            | iter([], rest, ids, mapp) = let
                fun mkPolyAll([], p) =  iterP(p@rest,[])
                  | mkPolyAll(id::es, p) = let
                    fun mkPoly(c, 0) = []
                      | mkPoly(5, n) = [E.Poly(id, [], n, [])]
                      | mkPoly(c, n) = [E.Poly(id, [E.C c], n, [])]
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
                | E.Poly(id, ix, n, _) =>
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

    (********************************** Step 3 *******************************)
    fun cleanup(body) = (case body
        of E.Const _            => body
        | E.ConstR _            => body
        | E.Zero _              => body
        | E.Delta _             => body
        | E.Epsilon _           => body
        | E.Eps2 _              => body
        | E.Field _             => body
        | E.Tensor _            => body
        | E.Poly (id,c, 0, dx)  => E.Const 1
        | E.Poly (id,c, n, dx)  => body
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
    fun normalize (e', dx) = let
        fun rewrite(body) = (case body
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

        fun useCount (SrcIR.V{useCnt, ...}) = !useCnt
        fun ll ([],cnt) = ""
        | ll (a1::args,cnt) = String.concat["\n\t", Int.toString(cnt),"_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1),",", ll(args,cnt+1)]


    (********************************** main *******************************)
    (* transform ein operator *)
    fun transform (y, ein as Ein.EIN{body,index, params}, args) =
        let
            val E.Probe(E.OField(E.PolyWrap pargs, e, dx), expProbe) = body
            val _ = print(String.concat["\n\n*******************\n  starting polyn:",MidIR.Var.name(y),"=", EinPP.toString(ein),"-",ll(args,0)])
            (********************************** Step 1 *******************************)
            (* replace polywrap args/params with probed position(s) args/params *)
            val start_idxs = (case (expProbe)
                of E.Tensor(tid,_) => [tid]
                |   E.Opn(E.Add,ps) => List.map (fn E.Tensor(tid,_) => tid) ps
                (*end case*))

            val (args, params, e) = polyArgs(args, params, pargs, start_idxs, e)
            val ein = Ein.EIN{body=e, index=index, params=params}
            val _ = print(String.concat["\n\n   polyArgs\n:",MidIR.Var.name(y),"=", EinPP.toString(ein),"-",ll(args,0)])

            (********************************** Step 2 *******************************)
            (* need to flatten before merging polynomials in product *)
            val e = mergeD(e)
            val _ = (String.concat["\n\n   mergeD->", EinPP.expToString(e)])
 
           (********************************** Step 3 *******************************)
            (* normalize ein by cleaning it up and differntiating*)
            val e = normalize(e, dx)
            val _ = (String.concat["\n\n   normalize->", EinPP.expToString(e),"********"])
            val _ = (String.concat["\n\n*******************\n"])
            val newbie = (y, IR.EINAPP(Ein.EIN{body=e, index=index, params=params}, args))

            val stg_poly = Controls.get Ctl.stgPoly
            val _ =  if(stg_poly) then ScanE.readIR_Single(newbie,"tmp-poly") else 1
      
        in
               [newbie]
        end



  end
