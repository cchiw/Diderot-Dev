(* mk-low-ir.sml
 *
 * Helper code to build LowIR assigments using the AvailRHS infrastructure.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure MkLowIR : sig

  (* an environment that maps De Bruijn indices to their iteration-index value *)
    type index_env = int IntRedBlackMap.map

  (* ??? *)
    val lookupIdx : int IntRedBlackMap.map * int -> int
  (* ??? *)
    val lookupMu : int IntRedBlackMap.map * Ein.mu -> int

  (* make "x := <int-literal>" *)
    val intLit : AvailRHS.t * IntLit.t -> LowIR.var
  (* make "x := <real-literal>" *)
    val realLit : AvailRHS.t * RealLit.t -> LowIR.var
  (* make "x := <real-literal>", where the real literal is specified as an integer *)
    val intToRealLit : AvailRHS.t * int -> LowIR.var

  (* generate a reduction sequence using the given binary operator *)
    val reduce : AvailRHS.t * (AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var) * LowIR.var list
          -> LowIR.var

  (* integer arithmetic *)
    val intAdd : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var

  (* scalar arithmetic *)
    val realAdd : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realSub : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realMul : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realDiv : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realMax : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realMin : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val boolGT  : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val boolLT  : AvailRHS.t * LowIR.var * LowIR.var -> LowIR.var
    val realNeg : AvailRHS.t * LowIR.var -> LowIR.var
    val realAbs : AvailRHS.t * LowIR.var -> LowIR.var
    val realClamp : AvailRHS.t * LowIR.var * LowIR.var * LowIR.var -> LowIR.var
    val realSgn : AvailRHS.t * LowIR.var -> LowIR.var
    val realIf : AvailRHS.t * LowIR.var * LowIR.var * LowIR.var -> LowIR.var
  (* scalar math functions *)
    val realSqrt   : AvailRHS.t * LowIR.var -> LowIR.var
    val realCos    : AvailRHS.t * LowIR.var -> LowIR.var
    val realArcCos : AvailRHS.t * LowIR.var -> LowIR.var
    val realSin    : AvailRHS.t * LowIR.var -> LowIR.var
    val realArcSin : AvailRHS.t * LowIR.var -> LowIR.var
    val realTan    : AvailRHS.t * LowIR.var -> LowIR.var
    val realArcTan : AvailRHS.t * LowIR.var -> LowIR.var
    val realExp   : AvailRHS.t * LowIR.var -> LowIR.var
    val intPow   : AvailRHS.t * LowIR.var * int -> LowIR.var

 
 (* vector arithmetic *)
    val vecAdd   : AvailRHS.t * int * LowIR.var * LowIR.var -> LowIR.var
    val vecSub   : AvailRHS.t * int * LowIR.var * LowIR.var -> LowIR.var
    val vecScale : AvailRHS.t * int * LowIR.var * LowIR.var -> LowIR.var
    val vecMul   : AvailRHS.t * int * LowIR.var * LowIR.var -> LowIR.var
    val vecNeg   : AvailRHS.t * int * LowIR.var -> LowIR.var
    val vecSum   : AvailRHS.t * int * LowIR.var -> LowIR.var
    val vecDot   : AvailRHS.t * int * LowIR.var * LowIR.var -> LowIR.var

    val swap2 : AvailRHS.t * LowIR.var* LowIR.var * LowIR.var -> LowIR.var
    val swap3 : AvailRHS.t * LowIR.var* LowIR.var * LowIR.var* LowIR.var -> LowIR.var
    val swap4 : AvailRHS.t * LowIR.var* LowIR.var * LowIR.var * LowIR.var * LowIR.var-> LowIR.var
    val swap5 : AvailRHS.t * LowIR.var* LowIR.var * LowIR.var * LowIR.var * LowIR.var * LowIR.var-> LowIR.var
    val swap6 : AvailRHS.t * LowIR.var* LowIR.var * LowIR.var * LowIR.var * LowIR.var * LowIR.var * LowIR.var -> LowIR.var


    (* FEM arithmetic *)
    val ProbeInvF : AvailRHS.t * int IntRedBlackMap.map * meshElem.fnspace*Ein.alpha * LowIR.var * LowIR.var -> LowIR.var
    val ProbeF : AvailRHS.t * int IntRedBlackMap.map * meshElem.fnspace*Ein.alpha * LowIR.var * LowIR.var -> LowIR.var
    val ProbePhi : AvailRHS.t * int IntRedBlackMap.map * meshElem.fnspace*Ein.alpha * LowIR.var * LowIR.var  -> LowIR.var


  (* tensor operations *)
    val tensorIndex   : AvailRHS.t * index_env * LowIR.var * Ein.alpha -> LowIR.var
    val tensorIndexIX : AvailRHS.t * LowIR.var * int list -> LowIR.var

  (* make "x := [args]" *)
    val cons : AvailRHS.t * int list * LowIR.var list -> LowIR.var
  (* code for δ_{i,j} *)
    val delta : AvailRHS.t * index_env * Ein.mu * Ein.mu -> LowIR.var
  (* code for ε_{i,j} *)
    val epsilon2 : AvailRHS.t * index_env * Ein.mu * Ein.mu -> LowIR.var
  (* code for ε_{i,j,k} *)
    val epsilon3 : AvailRHS.t * index_env * Ein.mu * Ein.mu * Ein.mu -> LowIR.var

  (* evaluate δ_{i,j} *)
    val evalDelta : index_env * Ein.mu * Ein.mu -> int

  end = struct

    structure IR = LowIR
    structure V = IR.Var
    structure Ty = LowTypes
    structure Op = LowOps
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ME = meshElem
  (* an environment that maps De Bruijn indices to their iteration-index value *)
    type index_env = int IMap.map

   fun lookupIdx (mapp, id) = (case IMap.find(mapp, id)
          of SOME x => x
           | NONE => raise Fail(concat["lookupIdx(_, ", Int.toString id, "): out of bounds"])
         (* end case *))

   fun lookupMu (mapp, E.V id) = lookupIdx (mapp, id)
     | lookupMu (_, E.C i) = i

    val add = AvailRHS.addAssign

    fun intLit (avail, n) = add (avail, "intLit", Ty.intTy, IR.LIT(Literal.Int n))
    fun realLit (avail, r) =
        let
        (*val _  = print(String.concat["\n\t Literal: ", (Literal.toString (Literal.Real r))])*)
        in add (avail, "realLit", Ty.realTy, IR.LIT(Literal.Real r)) end
    fun intToRealLit (avail, n) = realLit (avail, RealLit.fromInt(IntInf.fromInt n))

    fun cons (avail, shp, args) =
          add (avail, "tensor", Ty.TensorTy shp, IR.CONS(args, Ty.TensorTy shp))

    fun reduce (avail, rator, []) = raise Fail "reduction with no arguments"
      | reduce (avail, rator, arg::args) = let
          fun gen (acc, []) = acc
            | gen (acc, x::xs) = gen (rator (avail, acc, x), xs)
          in
            gen (arg, args)
          end

  (* integer arithmetic *)
    local
      fun scalarOp2 rator (avail, x, y) = add (avail, "i", Ty.IntTy, IR.OP(rator, [x, y]))
    in
    val intAdd = scalarOp2 Op.IAdd
    end

  (* scalar arithmetic *)
    local
      
      fun scalarOp1 rator (avail, x) = add (avail, "r", Ty.realTy, IR.OP(rator, [x]))
      fun scalarOp1R rator (avail, x) = add (avail, "r", Ty.realTy, IR.OP(rator(Ty.realTy), [x]))
      fun scalarOp2 rator (avail, x, y) = add (avail, "r", Ty.realTy, IR.OP(rator, [x, y]))
      fun scalarOp2R rator (avail, x, y) = add (avail, "r", Ty.realTy, IR.OP(rator(Ty.realTy), [x, y]))
      fun scalarOp2B rator (avail, x, y) = add (avail, "r", Ty.BoolTy, IR.OP(rator(Ty.BoolTy), [x, y]))
      fun scalarOp3 rator (avail, x, y, z) = add(avail, "t", Ty.realTy, IR.OP(rator, [x, y, z]))
      fun scalarOp4 rator (avail, w, x, y, z) = add(avail, "t", Ty.realTy, IR.OP(rator, [w, x, y, z]))
      fun scalarOp5 rator (avail, v, w, x, y, z) = add(avail, "t", Ty.realTy, IR.OP(rator, [v,w, x, y, z]))
      fun scalarOp6 rator (avail, u,v, w, x, y, z) = add(avail, "t", Ty.realTy, IR.OP(rator, [u, v,w, x, y, z]))
      fun scalarOp7 rator (avail, t, u,v, w, x, y, z) = add(avail, "t", Ty.realTy, IR.OP(rator, [t, u, v,w, x, y, z]))
      
    in
    val realNeg = scalarOp1 Op.RNeg
    val realSqrt = scalarOp1 Op.Sqrt
    val realExp = scalarOp1 Op.Exp
    val realCos = scalarOp1 Op.Cos
    val realArcCos = scalarOp1 Op.ArcCos
    val realSin = scalarOp1 Op.Sin
    val realArcSin = scalarOp1 Op.ArcSin
    val realTan = scalarOp1 Op.Tan
    val realArcTan = scalarOp1 Op.ArcTan
    val realSgn = scalarOp1 Op.Sgn
    val realAbs = scalarOp1R Op.Abs
    val realClamp = scalarOp3 Op.RClamp
        
    val realSub = scalarOp2 Op.RSub
    val realDiv = scalarOp2 Op.RDiv
    val realAdd = scalarOp2 Op.RAdd
    val realMul = scalarOp2 Op.RMul
    val realMax = scalarOp2R Op.Max
    val realMin = scalarOp2R Op.Min
    val boolGT = scalarOp2B Op.GT
    val boolLT = scalarOp2B Op.LT
    
    val realClamp = scalarOp3 Op.RClamp
    val realIf = scalarOp3 Op.IfWrap
    
    
    val swap2 = scalarOp3 Op.swap2
    val swap3 = scalarOp4 Op.swap3
    val swap4 = scalarOp5 Op.swap4
    val swap5 = scalarOp6 Op.swap5
    val swap6 = scalarOp7 Op.swap6
    end (* local *)

  (* vector arithmetic *)
    local
      fun vecOp1 rator (avail, dim, x) =
            add (avail, "v", Ty.TensorTy[dim], IR.OP(rator dim, [x]))
      fun vecOp2 rator (avail, dim, x, y) =
            add (avail, "v", Ty.TensorTy[dim], IR.OP(rator dim, [x, y]))
    in
    val vecAdd = vecOp2 Op.VAdd
    val vecSub = vecOp2 Op.VSub
    val vecScale = vecOp2 Op.VScale
    val vecMul = vecOp2 Op.VMul
    val vecNeg = vecOp1 Op.VNeg
    fun vecSum (avail, dim, v) = add (avail, "vsm", Ty.realTy, IR.OP(Op.VSum dim, [v]))
    fun vecDot (avail, dim, u, v) = add (avail, "vdot", Ty.realTy, IR.OP(Op.VDot dim, [u, v]))
    end (* local *)


    (*probe fem ops*)
    fun probeFem rator (avail, mapp, fnspace, dx, f, pos) = let
    
        val px = List.map (fn v => lookupMu (mapp, v)) dx
        fun iter([], nx, ny, nz) = (nx, ny, nz)
          | iter(e1::es, nx, ny, nz) = (case e1
                of 0 =>iter(es, nx+1, ny, nz)
                | 1 => iter(es, nx, ny+1, nz)
                | 2 => iter(es, nx, ny, nz+1)
            (*end case*))
        val (nx, ny, nz) = iter (px, 0, 0, 0)
        val opr = rator(fnspace, nx,ny,nz)
        val _ = (LowOps.toString (opr))
        in
            add (avail, "r", Ty.realTy, IR.OP(opr, [f,pos]))
        end
    val ProbeF =    probeFem Op.ProbeF
    val ProbeInvF = probeFem Op.ProbeInvF
    val ProbePhi =  probeFem Op.ProbePhi
    
    
    (* limits *)
    fun allConst [E.C 0] = true
    | allConst [E.C 0, E.C 0] = true

    fun tensorIndex (avail, mapp, arg, []) = arg
      | tensorIndex (avail, mapp, arg, ixs) =
        (case (V.ty arg)
            of Ty.TensorTy[] =>
                (* are all the indices constant 0? *)
                (*if(allConst ixs) then arg
                else*) raise Fail (concat["indexing a real arg:",EinPP.expToString(E.Tensor(1,ixs))])
            | _ =>
                add (
                    avail, "r", Ty.realTy,
                    IR.OP(Op.TensorIndex(V.ty arg, List.map (fn ix => lookupMu(mapp, ix)) ixs), [arg]))
         (* end case *))

    fun tensorIndexIX (avail, arg, []) = arg
      | tensorIndexIX (avail, arg, [ix]) = let
          val Ty.TensorTy[d] = V.ty arg
          in
            add (avail, "r", Ty.realTy, IR.OP(Op.VIndex(d, ix), [arg]))
          end
      | tensorIndexIX (avail, arg, ixs) =
          add (avail, "r", Ty.realTy, IR.OP(Op.TensorIndex(V.ty arg, ixs), [arg]))

    fun evalDelta (mapp, i, j) = let
          val i' = lookupMu (mapp, i)
          val j' = lookupMu (mapp, j)
          in
            if (i' = j') then 1 else 0
          end

    fun delta (avail, mapp, i, j) = let
          val i' = lookupMu (mapp, i)
          val j' = lookupMu (mapp, j)
          in
            if (i' = j') then intToRealLit (avail, 1) else intToRealLit (avail, 0)
          end

    fun epsilon2 (avail, mapp, i, j) = let
          val i' = lookupMu (mapp, i)
          val j' = lookupMu (mapp, j)
          in
            if (i' = j')
              then intToRealLit (avail, 0)
            else if (j' > i')
              then intToRealLit (avail, 1)
              else intToRealLit (avail, ~1)
          end

    fun epsilon3 (avail, mapp, i, j, k) = let
          val i' = lookupMu (mapp, i)
          val j' = lookupMu (mapp, j)
          val k' = lookupMu (mapp, k)
          in
            if (i' = j' orelse j' = k' orelse i' = k')
              then intToRealLit (avail, 0)
            else if (j' > i')
              then if (j' > k' andalso k' > i')
                then intToRealLit (avail, ~1)
                else intToRealLit (avail, 1)
            else if (i' > k' andalso k' > j')
              then intToRealLit (avail, 1)
              else intToRealLit (avail, ~1)
          end
    fun intPow(avail, x, pow_n) = let
      fun pow (1, avail) = x
      | pow (2, avail) =  add (avail, "_Pow_2", Ty.realTy, IR.OP(Op.RMul, [x, x]))
      | pow (n, avail) = let
        fun half m = let
            val y = pow (m div 2, avail)
            val name = String.concat["_Pow", Int.toString (m), "_"]
            in add (avail, name, Ty.realTy, IR.OP(Op.RMul, [y, y])) end
        in if ((n mod 2) = 0)
            then half n
            else let
                val y = half (n-1)
                val name = String.concat["_Pow", Int.toString (n), "_"]
                in add (avail, name, Ty.realTy, IR.OP(Op.RMul, [x, y])) end
      end
      in
        pow (pow_n, avail)
      end

  end
