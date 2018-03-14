(* apply.sml
 *
 * Apply EIN operator arguments to EIN operator.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Apply : sig

    val apply : Ein.ein * int * Ein.ein -> Ein.ein option

  end = struct

    structure E = Ein

    structure IMap = IntRedBlackMap

    fun mapId (i, dict, shift) = (case IMap.find(dict, i)
          of NONE => i + shift
           | SOME j => j
          (* end case *))

    fun mapIndex (ix, dict, shift) = (case IMap.find(dict, ix)
            of NONE =>  (String.concat["\nnone map:",Int.toString(ix),"-->",Int.toString(ix+shift),"\n"];E.V(ix + shift))
            | SOME j => (String.concat["\nsome map:",Int.toString(ix),"-->","\n"];j)
        (* end case *))

    fun mapId2 (i, dict, shift) = (case IMap.find(dict, i)
           of NONE => (
                (*(concat["Error: ", Int.toString i, " is out of range\n"]);*)
                i+shift)
            | SOME j => j
         (* end case *))
    fun ixT(E.V V) = "V"^Int.toString(V)
      | ixT(E.C C) = "C"^Int.toString(C)
    fun rewriteSubst (e, subId, mx, paramShift, sumShift) = let
          fun insertIndex ([], _, dict, shift) = (dict, shift)
            | insertIndex (e::es, n, dict, _) = let
                val shift = (case e of E.V ix => ix - n | E.C i => i - n)
                (*val _ = (String.concat["\ninsert index:",Int.toString(n),"=>",ixT(e),"\n"])*)
                in
                  insertIndex(es, n+1, IMap.insert(dict, n, e), shift)
                end

          val (subMu, shift) = insertIndex(mx, 0, IMap.empty, 0)
          val shift' = Int.max(sumShift, shift)
          (* val _ = (String.concat["\nshift':",Int.toString(shift')])*)
          val insideComp = ref(false)
          fun mapMu (E.V i) =
                if(!insideComp)
                then ( ("\nmapMu -does not map index:"^Int.toString(i));E.V i)
                else mapIndex(i, subMu, shift')
            | mapMu c = c 
          fun mapAlpha mx = List.map mapMu mx
          fun mapSingle i = let
                val E.V v = if(!insideComp) then  (E.V i) else mapIndex(i, subMu, shift')
                in
                  v
                end
          fun mapSum l = List.map (fn (a, b, c) => (mapSingle a, b, c)) l
          fun mapParam id = mapId2(id, subId, 0)
         fun p()=if(!insideComp) then "insideCompT" else "insideCompF"
         fun apply e = ( (*("\n\neinexp \t:"^EinPP.expToString(e)^"-"^p());*)case e
                 of E.Const _ => e
                  | E.ConstR _ => e
                  | E.Tensor(id, mx) => E.Tensor(mapParam id, mapAlpha mx)
                  | E.Zero(mx) => E.Zero(mapAlpha mx)
                  | E.Delta(i, j) => E.Delta(mapMu i, mapMu j)
                  | E.Epsilon(i, j, k) => E.Epsilon(mapMu i, mapMu j, mapMu k)
                  | E.Eps2(i, j) => E.Eps2(mapMu i,mapMu j)
                  | E.Field(id, mx) => E.Field(mapParam id, mapAlpha mx)
                  | E.Lift e1 => E.Lift(apply e1)
                  | E.Conv (v, mx, h, ux) => E.Conv(mapParam v, mapAlpha mx, mapParam h, mapAlpha ux)
                  | E.Partial mx => E.Partial (mapAlpha mx)
                  | E.Apply(e1, e2) => E.Apply(apply e1, apply e2)
                  | E.Probe(f, pos) => E.Probe(apply f, apply pos)
                    (* does not remap indices in field *)
                  | E.Comp(e1, es) =>  let
                        val e1' = apply e1
                        val es' = List.map (fn(e2, n2)=> (insideComp:=true; (apply e2, n2))) es
                        in (insideComp:=false; E.Comp(e1', es')) end
                  | E.OField(E.DataFem id, e2, alpha) => E.OField(E.DataFem(mapParam id), apply e2, mapAlpha alpha)
                  | E.OField(E.BuildFem (id,s), e2, alpha)
                 => E.OField(E.BuildFem(mapParam id, mapParam s), apply e2, mapAlpha alpha)
                  | E.OField(ofld, e2, alpha) => E.OField(ofld, apply e2, mapAlpha alpha)
                  | E.Value _ => raise Fail "expression before expand"
                  | E.Img _ => raise Fail "expression before expand"
                  | E.Krn _ => raise Fail "expression before expand"
                  | E.Sum(c, esum) => E.Sum(mapSum c, apply esum)
                  | E.Op1(op1, e1) => E.Op1(op1, apply e1)
                  | E.Op2(op2, e1, e2) => E.Op2(op2, apply e1, apply e2)
                  | E.Opn(opn, e1) => E.Opn(opn, List.map apply e1)
                  | E.If(E.LT(e1,e2), e3, e4) =>  E.If(E.LT(apply e1, apply e2), apply e3, apply e4)
                  | E.If(E.GT(e1,e2), e3, e4) =>  E.If(E.GT(apply e1, apply e2), apply e3, apply e4)
                (* end case *))
          in
            apply e
          end

 (* params subst *)
    fun rewriteParams (params, params2, place) = let
          val beg = List.take(params, place)
          val next = List.drop(params, place+1)
          val params' = beg@params2@next
          val n= length params
          val n2 = length params2
          val nbeg = length beg
          val nnext = length next
          fun createDict (0, shift1, shift2, dict) = dict
            | createDict (n, shift1, shift2, dict) =
                createDict (n-1, shift1, shift2, IMap.insert (dict, n+shift1, n+shift2))
          val origId = createDict (nnext, place, place+n2-1, IMap.empty)
          val subId = createDict (n2, ~1, place-1, IMap.empty)
          in
            (params', origId, subId, nbeg)
          end

  (* Looks for params id that match substitution *)
    fun apply (e1 as E.EIN{params, index, body}, place, e2) = let
(*
          val _ = print(String.concat["\n*******************\n Apply:",EinPP.toString(e1)])
           val _ = print(String.concat["\nwith:",EinPP.toString(e2), " \nat:",Int.toString(place),"\n"])
*)
          val E.EIN{params=params2, index=index2, body=body2} = e2
          val changed = ref false
          val (params', origId, substId, paramShift) = rewriteParams(params, params2, place)
          val sumIndex = ref(length index)
          val insideComp = ref(false)

          fun rewrite (id, mx, e, shape) = let
                val comp = !insideComp
                (* note change here*)
                val x = if(comp) then (length index) else  !sumIndex
(*
                val _ = print(String.concat["\nInside rewrite:",EinPP.expToString(e)])
val _ = print(String.concat["mx:",Int.toString(length mx)," shape:",Int.toString(length shape)])
*)
                in
                  if (id = place)
                    then if (length mx = length shape)
                      then (
                        changed := true;
                        rewriteSubst (body2, substId, mx, paramShift, x))
                      else raise Fail "argument/parameter mismatch"
                    else (case e
                       of E.Tensor(id, mx) => E.Tensor(mapId(id, origId, 0), mx)
                        | E.Field(id, mx) => E.Field(mapId(id, origId, 0), mx)
                        |  _ => raise Fail "term to be replaced is not a Tensor or Fields"
                      (* end case *))
                end
          fun sumI e = let val (v,_,_) = List.last e in v end
          fun apply (b, shape) = (case b
                 of E.Tensor(id, mx) => rewrite (id, mx, b, shape)
                  | E.Field(id, mx) => rewrite (id, mx, b, shape)
                  | E.Zero(mx) => b
                  | E.Lift e1 => E.Lift(apply (e1, shape))
                  | E.Conv(v, mx, h, ux) => E.Conv(mapId(v, origId, 0), mx, mapId(h, origId, 0), ux)
                  | E.Apply(e1, e2) => E.Apply(apply (e1, shape), apply  (e2, shape))
                  | E.Probe(f, pos) => E.Probe(apply (f, shape), apply (pos, shape))
                  | E.Comp(e1, es) => let
                        val fouter = apply(e1, shape)
                        val es' = List.map (fn(e2,n2)=> (insideComp:=true; (apply (e2,n2),n2))) es
                        in
                        (insideComp := true;E.Comp(fouter, es'))
                        end
                  | E.OField(ofld, e2, alpha)   => E.OField(ofld, apply(e2,shape), alpha)
                  | E.Value _ => raise Fail "expression before expand"
                  | E.Img _ => raise Fail "expression before expand"
                  | E.Krn _ => raise Fail "expression before expand"
                  | E.Sum(indices, esum) => let
                      val (ix, _, _) = List.last indices
                      in
                        sumIndex := ix;
                        E.Sum(indices, apply (esum, shape))
                      end
                  | E.Op1(op1, e1) => E.Op1(op1, apply(e1, shape))
                  | E.Op2(op2, e1, e2) => E.Op2(op2, apply(e1, shape), apply(e2, shape))
                  | E.Opn(opn, es) => E.Opn(opn, List.map (fn e1=> apply(e1,shape)) es)
                  | E.If(comp, e3, e4) => let
                    val  c= (case comp
                    of E.GT(e1, e2) => E.GT(apply (e1, shape), apply  (e2, shape))
                    | E.LT(e1, e2)  => E.LT(apply (e1, shape), apply  (e2, shape))
                    (* end case*))
                    in E.If(c, apply (e3, shape), apply  (e4, shape)) end
                  | _ => b
                (* end case *))
          val body'' = apply (body,index2)
            val newbie = E.EIN{params=params', index=index, body=body''}
           (* val _ = (String.concat["\n result from apply:",EinPP.toString(newbie)])*)
          (*second argument is size of replacement*)
          in
            if (! changed)
              then SOME(newbie)
              else NONE
          end

    end
