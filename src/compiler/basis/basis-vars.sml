(* basis-vars.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * This module defines the AST variables for the built in operators and functions.
 *)

structure BasisVars =
  struct
    local
      structure N = BasisNames
      structure Ty = Types
      structure MV = MetaVar

      fun --> (tys, ty) = Ty.T_Fun(tys, ty)
      infix -->

      val N2 = Ty.DimConst 2
      val N3 = Ty.DimConst 3

    (* short names for kinds *)
      val TK : unit -> Ty.meta_var = Ty.TYPE o MV.newTyVar
      fun DK () : Ty.meta_var = Ty.DIFF(MV.newDiffVar 0)
      val SK : unit -> Ty.meta_var = Ty.SHAPE o MV.newShapeVar
      val NK : unit -> Ty.meta_var = Ty.DIM o MV.newDimVar

      fun ty t = ([], t)
      fun all (kinds, mkTy : Ty.meta_var list -> Ty.ty) = let
            val tvs = List.map (fn mk => mk()) kinds
            in
              (tvs, mkTy tvs)
            end
      fun allNK mkTy = let
            val tv = MV.newDimVar()
            in
              ([Ty.DIM tv], mkTy tv)
            end

      fun poly (k, d, dd) = Ty.T_OField{diff=k, dim=d, shape=dd}
      fun poly' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))

      fun field (k, d, dd) = Ty.T_Field{diff=k, dim=d, shape=dd}
      fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
      fun tensor ds = Ty.T_Tensor(Ty.Shape ds)
      fun matrix d = tensor[d,d]
      fun dynSeq ty = Ty.T_Sequence(ty, NONE)

      fun monoVar (name, ty) = Var.newBasis (name, ([], ty))
      fun polyVar arg = Var.newBasis arg
    in

  (* overloaded operators; the naming convention is to use the operator name followed
   * by the argument type signature, where
   *    i  -- int
   *    b  -- bool
   *    r  -- real (tensor[])
   *    t  -- tensor[shape]
   *    I  -- image(d)[shape]
   *    f  -- field#k(d)[shape]
   *    s  -- field#k(d)[]
   *    d  -- ty{}
   *    T  -- ty
   *)

  (* concatenation of sequences *)
    val at_dT = polyVar (N.op_at, all([TK],
          fn [Ty.TYPE tv] => let
              val seqTyc = dynSeq(Ty.T_Var tv)
              in
                [seqTyc, Ty.T_Var tv] --> seqTyc
              end))
    val at_Td = polyVar (N.op_at, all([TK],
          fn [Ty.TYPE tv] => let
              val seqTyc = dynSeq(Ty.T_Var tv)
              in
                [Ty.T_Var tv, seqTyc] --> seqTyc
              end))
    val at_dd = polyVar (N.op_at, all([TK],
          fn [Ty.TYPE tv] => let
              val seqTyc = dynSeq(Ty.T_Var tv)
              in
                [seqTyc, seqTyc] --> seqTyc
              end))

    val add_ii = monoVar(N.op_add, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val add_tt = polyVar(N.op_add, all([SK], fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, t] --> t
            end))
    val add_ff = polyVar(N.op_add, all([DK,NK,SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [f, f] --> f
            end))
    val add_pp = polyVar(N.op_add, all([DK,DK,NK,SK],
        fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [f1, f2] --> f1
        end))

    val add_ft = polyVar(N.op_add, all([DK,NK,SK], (* field + scalar *)
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [f, t] --> f
            end))
    val add_tf = polyVar(N.op_add, all([DK,NK,SK], (* scalar + field *)
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, f] --> f
            end))

    val add_pt = polyVar(N.op_add, all([DK,NK,SK], (* field + scalar *)
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        val t = Ty.T_Tensor(Ty.ShapeVar dd)
        in
            [f, t] --> f
        end))

    val add_tp = polyVar(N.op_add, all([DK,NK,SK], (* scalar + field *)
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        val t = Ty.T_Tensor(Ty.ShapeVar dd)
        in
            [t, f] --> f
        end))


    val sub_ii = monoVar(N.op_sub, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val sub_tt = polyVar(N.op_sub, all([SK], fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, t] --> t
            end))
    val sub_ff = polyVar(N.op_sub, all([DK,NK,SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [f, f] --> f
            end))
    val sub_ft = polyVar(N.op_sub, all([DK,NK,SK], (* field - scalar *)
          fn [Ty.DIFF k, Ty.DIM d,Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [f, t] --> f
            end))
    val sub_tf = polyVar(N.op_sub, all([DK,NK,SK], (* scalar - field *)
          fn [Ty.DIFF k, Ty.DIM d,Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, f] --> f
            end))

    val sub_pp = polyVar(N.op_sub, all([DK,DK, NK,SK],
        fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
            val f1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val f2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
                [f1, f2] --> f1
            end))


    val sub_pt = polyVar(N.op_sub, all([DK,NK,SK], (* field - scalar *)
        fn [Ty.DIFF k, Ty.DIM d,Ty.SHAPE dd] => let
            val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [f, t] --> f
            end))
    val sub_tp = polyVar(N.op_sub, all([DK,NK,SK], (* scalar - field *)
        fn [Ty.DIFF k, Ty.DIM d,Ty.SHAPE dd] => let
            val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, f] --> f
            end))



  (* note that we assume that operators are tested in the order defined here, so that mul_rr
   * takes precedence over mul_rt and mul_tr!
   *)
    val mul_ii = monoVar(N.op_mul, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val mul_rr = monoVar(N.op_mul, [Ty.realTy, Ty.realTy] --> Ty.realTy)
    val mul_rt = polyVar(N.op_mul, all([SK], fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [Ty.realTy, t] --> t
            end))
    val mul_tr = polyVar(N.op_mul, all([SK], fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, Ty.realTy] --> t
            end))
    val mul_rf = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val t = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [Ty.realTy, t] --> t
            end))
val mul_rp = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
val t = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
in
[Ty.realTy, t] --> t
end))

    val mul_fr = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val t = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [t, Ty.realTy] --> t
            end))
val mul_pr = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
val t = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
in
[t, Ty.realTy] --> t
end))
    val mul_st = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            val g = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [f, t] --> g
            end))
val mul_lt = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
val t = Ty.T_Tensor(Ty.ShapeVar dd)
val g = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
in
[f, t] --> g
end))

    val mul_ts = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            val g = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [t, f] --> g
            end))
val mul_tl = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
val t = Ty.T_Tensor(Ty.ShapeVar dd)
val g = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
in
[t, f] --> g
end))
    val mul_ss = polyVar(N.op_mul, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
            val t = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            in
              [t, t] --> t
            end))

    val mul_ll = polyVar(N.op_mul, all([DK,DK, NK], fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d] => let
            val p1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val p2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            in
                [p1, p2] --> p1
            end))

    val mul_sf = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val a = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val b = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [a, b] --> b
            end))

    val mul_lp = polyVar(N.op_mul, all([DK,DK, NK,SK], fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
            val a = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val b = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
            [a, b] --> b
            end))


    val mul_fs = polyVar(N.op_mul, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val a = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val b = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [b, a] --> b
            end))

    val mul_pl = polyVar(N.op_mul, all([DK,DK,NK,SK], fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
        val a = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
        val b = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [b, a] --> b
        end))

    val div_ii = monoVar(N.op_div, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val div_rr = monoVar(N.op_div, [Ty.realTy, Ty.realTy] --> Ty.realTy)
    val div_tr = polyVar(N.op_div, all([SK], fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t, Ty.realTy] --> t
            end))
    val div_fr = polyVar(N.op_div, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val t = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [t, Ty.realTy] --> t
            end))
    val div_pr = polyVar(N.op_div, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
    val t = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
    in
    [t, Ty.realTy] --> t
    end))

    val div_ss = polyVar(N.op_mul, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
            val t = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            in
              [t, t] --> t
            end))
    val div_ll = polyVar(N.op_mul, all([DK,DK, NK], fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d] => let
        val p1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
        val p2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
        in
        [p1, p2] --> p1
        end))
    val div_fs = polyVar(N.op_div, all([DK,DK,NK,SK], fn [Ty.DIFF k, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            val s = Ty.T_Field{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            in
              [f, s] --> f
            end))

    val div_pl = polyVar(N.op_div, all([DK,DK,NK,SK], fn [Ty.DIFF k, Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        val s = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
        in
        [f, s] --> f
        end))

    val div_ts = polyVar(N.op_div, all([DK,NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            val s = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
            in
              [t, s] --> f
            end))
    val div_tl = polyVar(N.op_div, all([DK, NK,SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val t = Ty.T_Tensor(Ty.ShapeVar dd)
        val s = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
        val f = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [t, s] --> f
        end))

  (* power; we distinguish between integer and real exponents to allow x^2 to be compiled
   * as x*x.  The power operation of fields is restricted by the typechecker to constant
   * integer arguments.
   *)
    val pow_ri = monoVar(N.op_pow, [Ty.realTy, Ty.T_Int] --> Ty.realTy)
    val pow_rr = monoVar(N.op_pow, [Ty.realTy, Ty.realTy] --> Ty.realTy)
    val pow_si = polyVar (N.op_pow, all([DK, NK], fn [Ty.DIFF k, Ty.DIM d] => let
          val k = Ty.DiffVar(k, 0)
          val d = Ty.DimVar d
          val fld = field(k, d, Ty.Shape[])
          in
            [fld, Ty.T_Int] --> fld
          end))
    val convolve_vk = polyVar (N.op_convolve, all([DK, NK, SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
          val k = Ty.DiffVar(k, 0)
          val d = Ty.DimVar d
          val dd = Ty.ShapeVar dd
          in
            [Ty.T_Image{dim=d, shape=dd}, Ty.T_Kernel k] --> field(k, d, dd)
          end))
    val convolve_kv = polyVar (N.op_convolve, all([DK, NK, SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
          val k = Ty.DiffVar(k, 0)
          val d = Ty.DimVar d
          val dd = Ty.ShapeVar dd
          in
            [Ty.T_Kernel k, Ty.T_Image{dim=d, shape=dd}] --> field(k, d, dd)
          end))

  (* curl on 2d and 3d vector fields *)
    local
      val diff0 = Ty.DiffConst 0
      fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
    in
    val curl2D = polyVar (N.op_curl, all([DK], fn [Ty.DIFF k] => let
          val km1 = Ty.DiffVar(k, ~1)
          in
            [field' (Ty.DiffVar(k, 0), 2, [2])] --> field' (km1, 2, [])
          end))
    val curl3D = polyVar (N.op_curl, all([DK], fn [Ty.DIFF k] =>let
          val km1 = Ty.DiffVar(k, ~1)
          in
            [field' (Ty.DiffVar(k, 0), 3, [3])] --> field' (km1, 3, [3])
          end))

val curl2D_p = polyVar (N.op_curl, all([DK], fn [Ty.DIFF k] => let
val km1 = Ty.DiffVar(k, ~1)
in
[poly' (Ty.DiffVar(k, 0), 2, [2])] --> poly' (km1, 2, [])
end))
val curl3D_p = polyVar (N.op_curl, all([DK], fn [Ty.DIFF k] =>let
val km1 = Ty.DiffVar(k, ~1)
in
[poly' (Ty.DiffVar(k, 0), 3, [3])] --> poly' (km1, 3, [3])
end))
    end (* local *)

    val lt_ii = monoVar(N.op_lt, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val lt_rr = monoVar(N.op_lt, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)
    val lte_ii = monoVar(N.op_lte, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val lte_rr = monoVar(N.op_lte, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)
    val gte_ii = monoVar(N.op_gte, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val gte_rr = monoVar(N.op_gte, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)
    val gt_ii = monoVar(N.op_gt, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val gt_rr = monoVar(N.op_gt, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)

    val equ_bb = monoVar(N.op_equ, [Ty.T_Bool, Ty.T_Bool] --> Ty.T_Bool)
    val equ_ii = monoVar(N.op_equ, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val equ_ss = monoVar(N.op_equ, [Ty.T_String, Ty.T_String] --> Ty.T_Bool)
    val equ_rr = monoVar(N.op_equ, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)
    val neq_bb = monoVar(N.op_neq, [Ty.T_Bool, Ty.T_Bool] --> Ty.T_Bool)
    val neq_ii = monoVar(N.op_neq, [Ty.T_Int, Ty.T_Int] --> Ty.T_Bool)
    val neq_ss = monoVar(N.op_neq, [Ty.T_String, Ty.T_String] --> Ty.T_Bool)
    val neq_rr = monoVar(N.op_neq, [Ty.realTy, Ty.realTy] --> Ty.T_Bool)

    val neg_i = monoVar(N.op_neg, [Ty.T_Int] --> Ty.T_Int)
    val neg_t = polyVar(N.op_neg, all([SK], fn [Ty.SHAPE dd] => let
          val t = Ty.T_Tensor(Ty.ShapeVar dd)
          in
            [t] --> t
          end))
    val neg_f = polyVar(N.op_neg, all([DK, NK, SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
          val k = Ty.DiffVar(k, 0)
          val d = Ty.DimVar d
          val dd = Ty.ShapeVar dd
          in
            [field(k, d, dd)] --> field(k, d, dd)
          end))
    val neg_p = polyVar(N.op_neg, all([DK, NK, SK], fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dd = Ty.ShapeVar dd
    in
        [poly(k, d, dd)] --> poly(k, d, dd)
    end))

  (* clamp works on tensors, but there is also the boarder-control clamp function on images;
   * the arguments are (lo, hi, value), which is different than found in OpenCL and OpenGL.
   *)
    val clamp_rrt = polyVar (N.fn_clamp, all([SK,NK], fn [Ty.SHAPE dd, Ty.DIM d] => let
          val t = Ty.T_Tensor(Ty.ShapeExt(Ty.ShapeVar dd, Ty.DimVar d))
          in
            [Ty.realTy, Ty.realTy, t] --> t
          end))
    val clamp_ttt = polyVar (N.fn_clamp, all([SK], fn [Ty.SHAPE dd] => let
          val t = Ty.T_Tensor(Ty.ShapeVar dd)
          in
            [t, t, t] --> t
          end))

    val lerp3 = polyVar(N.fn_lerp, all([SK], fn [Ty.SHAPE dd] => let
          val t = Ty.T_Tensor(Ty.ShapeVar dd)
          in
            [t, t, Ty.realTy] --> t
          end))
    val lerp5 = polyVar(N.fn_lerp, all([SK], fn [Ty.SHAPE dd] => let
          val t = Ty.T_Tensor(Ty.ShapeVar dd)
          in
            [t, t, Ty.realTy, Ty.realTy, Ty.realTy] --> t
          end))
    val clerp3 = polyVar(N.fn_clerp, all([SK], fn [Ty.SHAPE dd] => let
        val t = Ty.T_Tensor(Ty.ShapeVar dd)
        in
          [t, t, Ty.realTy] --> t
        end))
    val clerp5 = polyVar(N.fn_clerp, all([SK], fn [Ty.SHAPE dd] => let
        val t = Ty.T_Tensor(Ty.ShapeVar dd)
        in
          [t, t, Ty.realTy, Ty.realTy, Ty.realTy] --> t
        end))

  (* Eigenvalues/vectors of a matrix; we only support this operation on 2x2 and 3x3 matrices, so
   * we overload the function.
   *)
    local
      fun evals d = monoVar (N.fn_evals, [matrix d] --> Ty.T_Sequence(Ty.realTy, SOME d))
      fun evecs d = monoVar (N.fn_evecs, [matrix d] --> Ty.T_Sequence(tensor[d], SOME d))
    in
    val evals2x2 = evals(Ty.DimConst 2)
    val evecs2x2 = evecs(Ty.DimConst 2)
    val evals3x3 = evals(Ty.DimConst 3)
    val evecs3x3 = evecs(Ty.DimConst 3)
    end (* local *)

  (***** non-overloaded operators, etc. *****)

  (* integer modulo *)
    val op_mod = monoVar(N.op_mod, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)

  (* pseudo-operator for probing a field *)
    val op_probe = polyVar (N.op_at, all([DK, NK, SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val dd = Ty.ShapeVar dd
            in
              [field(k, d, dd), tensor[d]] --> Ty.T_Tensor dd
            end))


  (* differentiation of scalar fields *)
    val op_D = polyVar (N.op_D, all([DK, NK],
          fn [Ty.DIFF k, Ty.DIM d] => let
            val k0 = Ty.DiffVar(k, 0)
            val km1 = Ty.DiffVar(k, ~1)
            val d = Ty.DimVar d
            in
              [field(k0, d, Ty.Shape[])] --> field(km1, d, Ty.Shape[d])
            end))


    (* differentiation of scalar fields *)
    val op_D_p = polyVar (N.op_D, all([DK, NK],
        fn [Ty.DIFF k, Ty.DIM d] => let
        val k0 = Ty.DiffVar(k, 0)
        val km1 = Ty.DiffVar(k, ~1)
        val d = Ty.DimVar d
        in
        [poly(k0, d, Ty.Shape[])] --> poly(km1, d, Ty.Shape[d])
        end))


  (* differentiation of higher-order tensor fields *)
    val op_Dotimes = polyVar (N.op_Dotimes, all([DK, NK, SK, NK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd, Ty.DIM d'] => let
            val k0 = Ty.DiffVar(k, 0)
            val km1 = Ty.DiffVar(k, ~1)
            val d = Ty.DimVar d
            val d' = Ty.DimVar d'
            val dd = Ty.ShapeVar dd
            in
              [field(k0, d, Ty.ShapeExt(dd, d'))]
                --> field(km1, d, Ty.ShapeExt(Ty.ShapeExt(dd, d'), d))
            end))
    val op_Dotimes_p = polyVar (N.op_Dotimes, all([DK, NK, SK, NK],
            fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd, Ty.DIM d'] => let
            val k0 = Ty.DiffVar(k, 0)
            val km1 = Ty.DiffVar(k, ~1)
            val d = Ty.DimVar d
            val d' = Ty.DimVar d'
            val dd = Ty.ShapeVar dd
            in
            [poly(k0, d, Ty.ShapeExt(dd, d'))]
            --> poly(km1, d, Ty.ShapeExt(Ty.ShapeExt(dd, d'), d))
            end))

   (* divergence differentiation of higher-order tensor fields *)
    val op_Ddot = polyVar (N.op_Ddot, all([DK, NK, SK, NK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd, Ty.DIM d'] => let
            val k0 = Ty.DiffVar(k, 0)
            val km1 = Ty.DiffVar(k, ~1)
            val d = Ty.DimVar d
            val d' = Ty.DimVar d'
            val dd' = Ty.ShapeVar dd
            in
              [field(k0, d, Ty.ShapeExt(dd', d'))] --> field(km1, d, dd')
            end))

    (* divergence differentiation of higher-order tensor fields *)
        val op_Ddot_p = polyVar (N.op_Ddot, all([DK, NK, SK, NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd, Ty.DIM d'] => let
        val k0 = Ty.DiffVar(k, 0)
        val km1 = Ty.DiffVar(k, ~1)
        val d = Ty.DimVar d
        val d' = Ty.DimVar d'
        val dd' = Ty.ShapeVar dd
        in
        [poly(k0, d, Ty.ShapeExt(dd', d'))] --> poly(km1, d, dd')
        end))

    val op_norm_t = polyVar (N.op_norm, all([SK],
          fn [Ty.SHAPE dd] => [Ty.T_Tensor(Ty.ShapeVar dd)] --> Ty.realTy))
    val op_norm_f = polyVar (N.op_norm, all([DK, NK, SK], fn [Ty.DIFF k,Ty.DIM d, Ty.SHAPE dd1] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val f1 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd1}
            val f2 = Ty.T_Field{diff = k, dim = d, shape = Ty.Shape []}
            in
              [f1] --> f2
            end))
    val op_norm_p = polyVar (N.op_norm, all([DK, NK, SK], fn [Ty.DIFF k,Ty.DIM d, Ty.SHAPE dd1] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val f1 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd1}
        val f2 = Ty.T_OField{diff = k, dim = d, shape = Ty.Shape []}
        in
            [f1] --> f2
        end))


  (* boolean operators; 'and' and 'or' are used to implement reductions *)
    val op_and = monoVar (Atom.atom "$and", [Ty.T_Bool, Ty.T_Bool] --> Ty.T_Bool)
    val op_or = monoVar (Atom.atom "$or", [Ty.T_Bool, Ty.T_Bool] --> Ty.T_Bool)
    val op_not = monoVar (N.op_not, [Ty.T_Bool] --> Ty.T_Bool)

  (* cross product *)
    local
      val crossTy = let
            val t = tensor[N3]
            in
              [t, t] --> t
            end
      val crossTy2 = let
            val t = tensor[N2]
            in
              [t, t] --> Ty.realTy
            end
    in
    val op_cross2_tt = monoVar (N.op_cross, crossTy2)
    val op_cross3_tt = monoVar (N.op_cross, crossTy)
    end (* local *)

    val op_cross2_ff  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, 2, [2])
            val t1 = field' (k0, 2, [])
            in
              [f, f] --> t1
            end))

val op_cross2_pp  = polyVar (N.op_cross, all([DK, DK], fn [Ty.DIFF k1, Ty.DIFF k2] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val k10 = Ty.DiffVar(k1, 0)
val k20 = Ty.DiffVar(k2, 0)
val f1 = field' (k10, 2, [2])
val f2 = field' (k20, 2, [2])
val t1 = field' (k10, 2, [])
in
[f1, f2] --> t1
end))

    val op_cross3_ff  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val f = field' (Ty.DiffVar(k, 0), 3, [3])
            in
              [f, f] --> f
            end))

val op_cross3_pp  = polyVar (N.op_cross, all([DK, DK], fn [Ty.DIFF k1,Ty.DIFF k2] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val f1 = field' (Ty.DiffVar(k1, 0), 3, [3])
val f2 = field' (Ty.DiffVar(k2, 0), 3, [3])
in
[f1, f2] --> f1
end))

    val op_cross2_ft  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, 2, [2])
            val t = tensor[N2]
            val t1 = field' (k0, 2, [])
            in
              [f, t] --> t1
            end))

val op_cross2_pt  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val k0 = Ty.DiffVar(k, 0)
val f = field' (k0, 2, [2])
val t = tensor[N2]
val t1 = field' (k0, 2, [])
in
[f, t] --> t1
end))

    val op_cross2_tf  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, 2, [2])
            val t = tensor[N2]
            val t1 = field' (k0, 2, [])
            in
              [t, f] --> t1
            end))

val op_cross2_tp  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val k0 = Ty.DiffVar(k, 0)
val f = field' (k0, 2, [2])
val t = tensor[N2]
val t1 = field' (k0, 2, [])
in
[t, f] --> t1
end))
    val op_cross3_ft  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val f = field' (Ty.DiffVar(k, 0), 3, [3])
            val t = tensor[N3]
            in
              [f, t] --> f
            end))

val op_cross3_pt  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val f = field' (Ty.DiffVar(k, 0), 3, [3])
val t = tensor[N3]
in
[f, t] --> f
end))

    val op_cross3_tf  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
            fun field' (k, d, dd) = field(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
            val f = field' (Ty.DiffVar(k, 0), 3, [3])
            val t = tensor[N3]
            in
              [t, f] --> f
            end))
val op_cross3_tp  = polyVar (N.op_cross, all([DK], fn [Ty.DIFF k] => let
fun field' (k, d, dd) = poly(k, Ty.DimConst d, Ty.Shape(List.map Ty.DimConst dd))
val f = field' (Ty.DiffVar(k, 0), 3, [3])
val t = tensor[N3]
in
[t, f] --> f
end))

  (* the inner product operator (including dot product) is treated as a special case in the
   * typechecker.  It is not included in the basis environment, but we define its type scheme
   * here.  There is an implicit constraint on its type to have the following scheme:
   *
   *     ALL[sigma1, d1, sigma2] . tensor[sigma1, d1] * tensor[d1, sigma2] -> tensor[sigma1, sigma2]
   *)
    val op_inner_tt = polyVar (N.op_dot, all([SK,SK,SK],
          fn [Ty.SHAPE s1, Ty.SHAPE s2, Ty.SHAPE s3] =>
              [Ty.T_Tensor(Ty.ShapeVar s1), Ty.T_Tensor(Ty.ShapeVar s2)]
                --> Ty.T_Tensor(Ty.ShapeVar s3)))
    val op_inner_tf = polyVar (N.op_dot, all([DK, NK, SK, SK, SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
              val k = Ty.DiffVar(k, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Tensor(Ty.ShapeVar dd1)
              val t2 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd2}
              val t3 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd3}
              in
                [t1, t2] --> t3
              end))
    val op_inner_ft = polyVar (N.op_dot, all([DK, NK, SK, SK, SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
              val k = Ty.DiffVar(k, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd1}
              val t2 = Ty.T_Tensor(Ty.ShapeVar dd2)
              val t3 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd3}
              in
                [t1, t2] --> t3
              end))
    val op_inner_ff = polyVar (N.op_dot, all([DK, DK, NK, SK, SK, SK],
          fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
              val k1 = Ty.DiffVar(k1, 0)
              val k2 = Ty.DiffVar(k2, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Field{diff = k1, dim = d, shape = Ty.ShapeVar dd1}
              val t2 = Ty.T_Field{diff = k2, dim = d, shape = Ty.ShapeVar dd2}
              val t3 = Ty.T_Field{diff = k1, dim = d, shape = Ty.ShapeVar dd3}
              in
                [t1, t2] --> t3
              end))

    val op_inner_tp = polyVar (N.op_dot, all([DK, NK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_Tensor(Ty.ShapeVar dd1)
            val t2 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))

    val op_inner_pt = polyVar (N.op_dot, all([DK, NK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_Tensor(Ty.ShapeVar dd2)
            val t3 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))

    val op_inner_pp = polyVar (N.op_dot, all([DK, DK, NK, SK, SK, SK],
        fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k1 = Ty.DiffVar(k1, 0)
            val k2 = Ty.DiffVar(k2, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k1, dim = d, shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_OField{diff = k2, dim = d, shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k1, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))

  (* the outer product operator is treated as a special case in the typechecker.  It is not
   * included in the basis environment, but we define its type scheme here.  There is an
   * implicit constraint on its type to have the following scheme:
   *
   *     ALL[sigma1, sigma2] . tensor[sigma1] * tensor[sigma2] -> tensor[sigma1, sigma2]
   *)
    val op_outer_tt = polyVar (N.op_outer, all([SK, SK, SK],
          fn [Ty.SHAPE s1, Ty.SHAPE s2, Ty.SHAPE s3] =>
              [Ty.T_Tensor(Ty.ShapeVar s1), Ty.T_Tensor(Ty.ShapeVar s2)]
                --> Ty.T_Tensor(Ty.ShapeVar s3)))
    val op_outer_tf = polyVar (N.op_outer, all([DK,NK,SK,SK,SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_Tensor(Ty.ShapeVar dd1)
            val t2 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd3}
            in
              [t1, t2] --> t3
            end))
    val op_outer_ft = polyVar (N.op_outer, all([DK, NK, SK, SK, SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
              val k = Ty.DiffVar(k, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd1}
              val t2 = Ty.T_Tensor(Ty.ShapeVar dd2)
              val t3 = Ty.T_Field{diff = k, dim = d, shape = Ty.ShapeVar dd3}
              in
                [t1, t2] --> t3
              end))
    val op_outer_ff = polyVar (N.op_outer, all([DK, DK, NK, SK, SK, SK],
          fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
              val k1 = Ty.DiffVar(k1, 0)
              val k2 = Ty.DiffVar(k2, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Field{diff = k1, dim = d, shape = Ty.ShapeVar dd1}
              val t2 = Ty.T_Field{diff = k2, dim = d, shape = Ty.ShapeVar dd2}
              val t3 = Ty.T_Field{diff = k1, dim = d, shape = Ty.ShapeVar dd3}
              in
                [t1, t2] --> t3
              end))
    val op_outer_tp = polyVar (N.op_outer, all([DK,NK,SK,SK,SK],
            fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_Tensor(Ty.ShapeVar dd1)
            val t2 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))
    val op_outer_pt = polyVar (N.op_outer, all([DK, NK, SK, SK, SK],
            fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_Tensor(Ty.ShapeVar dd2)
            val t3 = Ty.T_OField{diff = k, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))
        val op_outer_pp = polyVar (N.op_outer, all([DK, DK, NK, SK, SK, SK],
            fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, Ty.SHAPE dd3] => let
            val k1 = Ty.DiffVar(k1, 0)
            val k2 = Ty.DiffVar(k2, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k1, dim = d, shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_OField{diff = k2, dim = d, shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k1, dim = d, shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))

  (* the colon (or double-dot) product operator is treated as a special case in the
   * typechecker.  It is not included in the basis environment, but we define its type
   * scheme here.  There is an implicit constraint on its type to have the following scheme:
   *
   *     ALL[sigma1, d1, d2, sigma2] .
   *       tensor[sigma1, d1, d2] * tensor[d1, d2, sigma2] -> tensor[sigma1, sigma2]
   *)
    val op_colon_tt = polyVar (N.op_colon, all([SK,SK,SK],
          fn [Ty.SHAPE s1, Ty.SHAPE s2, Ty.SHAPE s3] =>
            [Ty.T_Tensor(Ty.ShapeVar s1), Ty.T_Tensor(Ty.ShapeVar s2)]
              --> Ty.T_Tensor(Ty.ShapeVar s3)))
    val op_colon_ff = polyVar (N.op_colon, all([DK,SK,NK,SK,SK],
          fn [Ty.DIFF k,Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE dd2,Ty.SHAPE dd3] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd3}
            in
              [t1,t2] --> t3
            end))
    val op_colon_ft = polyVar (N.op_colon, all([DK,SK,NK,SK,SK],
          fn [Ty.DIFF k,Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE s2,Ty.SHAPE dd3] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_Tensor(Ty.ShapeVar s2)
            val t3 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd3}
            in
              [t1, t2] --> t3
            end))
    val op_colon_tf = polyVar (N.op_colon, all([DK,SK,NK,SK,SK],
          fn [Ty.DIFF k,Ty.SHAPE s1, Ty.DIM d, Ty.SHAPE dd2,Ty.SHAPE dd3] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_Tensor(Ty.ShapeVar s1)
            val t2 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd3}
            in
              [t1,t2] --> t3
            end))


    val op_colon_pp = polyVar (N.op_colon, all([DK,DK, SK,NK,SK,SK],
        fn [Ty.DIFF k1,Ty.DIFF k2,Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE dd2,Ty.SHAPE dd3] => let
            val k10 = Ty.DiffVar(k1, 0)
            val k20 = Ty.DiffVar(k2, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k10, dim = d', shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_OField{diff = k20, dim = d', shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k10, dim = d', shape = Ty.ShapeVar dd3}
            in
            [t1,t2] --> t3
            end))
    val op_colon_pt = polyVar (N.op_colon, all([DK,SK,NK,SK,SK],
        fn [Ty.DIFF k,Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE s2,Ty.SHAPE dd3] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k0, dim = d', shape = Ty.ShapeVar dd1}
            val t2 = Ty.T_Tensor(Ty.ShapeVar s2)
            val t3 = Ty.T_OField{diff = k0, dim = d', shape = Ty.ShapeVar dd3}
            in
            [t1, t2] --> t3
            end))
    val op_colon_tp = polyVar (N.op_colon, all([DK,SK,NK,SK,SK],
        fn [Ty.DIFF k,Ty.SHAPE s1, Ty.DIM d, Ty.SHAPE dd2,Ty.SHAPE dd3] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val t1 = Ty.T_Tensor(Ty.ShapeVar s1)
            val t2 = Ty.T_OField{diff = k0, dim = d', shape = Ty.ShapeVar dd2}
            val t3 = Ty.T_OField{diff = k0, dim = d', shape = Ty.ShapeVar dd3}
            in
            [t1,t2] --> t3
            end))

  (* image size operation *)
    val fn_size = polyVar (N.fn_size, all([NK, SK],
            fn [Ty.DIM d, Ty.SHAPE dd] => let
                val d = Ty.DimVar d
                val dd = Ty.ShapeVar dd
                in
                  [Ty.T_Image{dim=d, shape=dd}] --> Ty.T_Sequence(Ty.T_Int, SOME d)
                end))

  (* functions that handle the boundary behavior of an image *)
    local
      fun img2img f = polyVar (f, all([NK, SK],
            fn [Ty.DIM d, Ty.SHAPE dd] => let
                val imgTy = Ty.T_Image{dim=Ty.DimVar d, shape=Ty.ShapeVar dd}
                in
                  [imgTy] --> imgTy
                end))
    in
    val image_border = polyVar (N.fn_border, all([NK, SK],
            fn [Ty.DIM d, Ty.SHAPE dd] => let
                val d = Ty.DimVar d
                val dd = Ty.ShapeVar dd
                in
                  [Ty.T_Image{dim=d, shape=dd}, Ty.T_Tensor dd]
                    --> Ty.T_Image{dim=d, shape=dd}
                end))
    val image_clamp = img2img N.fn_clamp
    val image_mirror = img2img N.fn_mirror
    val image_wrap = img2img N.fn_wrap
    end (* local *)

  (* is a point inside the domain of a field? *)
    val fn_inside = polyVar (N.fn_inside, all([DK, NK, SK],
            fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
                val k = Ty.DiffVar(k, 0)
                val d = Ty.DimVar d
                val dd = Ty.ShapeVar dd
                in
                  [Ty.T_Tensor(Ty.Shape[d]), field(k, d, dd)]
                    --> Ty.T_Bool
                end))

    val fn_length = polyVar (N.fn_length, all([TK],
            fn [Ty.TYPE tv] => [dynSeq(Ty.T_Var tv)] --> Ty.T_Int))

    val fn_abs_i = monoVar (N.fn_abs, [Ty.T_Int] --> Ty.T_Int)
    val fn_abs_r = monoVar (N.fn_abs, [Ty.realTy] --> Ty.realTy)
    val fn_max_i = monoVar (N.fn_max, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val fn_max_r = monoVar (N.fn_max, [Ty.realTy, Ty.realTy] --> Ty.realTy)
    val fn_min_i = monoVar (N.fn_min, [Ty.T_Int, Ty.T_Int] --> Ty.T_Int)
    val fn_min_r = monoVar (N.fn_min, [Ty.realTy, Ty.realTy] --> Ty.realTy)


    val fn_modulate_tt = polyVar (N.fn_modulate, all([NK],
          fn [Ty.DIM dd] => let
              (*val t = Ty.T_Tensor(Ty.ShapeVar dd)*)
              val t = tensor [Ty.DimVar dd]
              in
                [t, t] --> t
              end))

    val fn_modulate_tf = polyVar (N.fn_modulate, all([DK ,NK, NK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd] => let
              val k = Ty.DiffVar(k, 0)
              val d = Ty.DimVar d
              val t1 = tensor [Ty.DimVar dd]
              val t2 = Ty.T_Field{diff = k, dim = d, shape = Ty.Shape([Ty.DimVar dd])}
              in
                [t1, t2] --> t2
              end))

    val fn_modulate_ft = polyVar (N.fn_modulate, all([DK, NK, NK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd] => let
              val k = Ty.DiffVar(k, 0)
              val d = Ty.DimVar d
              val t1 = Ty.T_Field{diff = k, dim = d, shape = Ty.Shape([Ty.DimVar dd])}
              val t2 = tensor [Ty.DimVar dd]
              in
                [t1, t2] --> t1
              end))

    val fn_modulate_ff = polyVar (N.fn_modulate, all([DK, NK, NK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.DIM  dd] => let
              val k0 = Ty.DiffVar(k, 0)
              val d' = Ty.DimVar d
              val f1 = Ty.T_Field{diff = k0, dim = d', shape =Ty.Shape([Ty.DimVar dd])}
              in
                [f1,f1] --> f1
              end))

    val fn_modulate_tp = polyVar (N.fn_modulate, all([DK ,NK, NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = tensor [Ty.DimVar dd]
            val t2 = Ty.T_OField{diff = k, dim = d, shape = Ty.Shape([Ty.DimVar dd])}
            in
                [t1, t2] --> t2
            end))

    val fn_modulate_pt = polyVar (N.fn_modulate, all([DK, NK, NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val t1 = Ty.T_OField{diff = k, dim = d, shape = Ty.Shape([Ty.DimVar dd])}
            val t2 = tensor [Ty.DimVar dd]
            in
                [t1, t2] --> t1
            end))

    val fn_modulate_pp = polyVar (N.fn_modulate, all([DK,DK, NK, NK],
        fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.DIM  dd] => let
            val k10 = Ty.DiffVar(k1, 0)
            val k20 = Ty.DiffVar(k2, 0)
            val d' = Ty.DimVar d
            val f1 = Ty.T_OField{diff = k10, dim = d', shape =Ty.Shape([Ty.DimVar dd])}
            val f2 = Ty.T_OField{diff = k20, dim = d', shape =Ty.Shape([Ty.DimVar dd])}
            in
                [f1,f2] --> f1
            end))

    val fn_normalize_t = polyVar (N.fn_normalize, all([SK],
          fn [Ty.SHAPE dd] => let
            val t = Ty.T_Tensor(Ty.ShapeVar dd)
            in
              [t] --> t
            end))
    val fn_normalize_f = polyVar (N.fn_normalize, all([DK,NK,SK],
          fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val f1 = Ty.T_Field{diff = k0, dim = d', shape = Ty.ShapeVar dd1}
            in
              [f1] --> f1
            end))
    val fn_normalize_p = polyVar (N.fn_normalize, all([DK,NK,SK],
         fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd1] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val f1 = Ty.T_OField{diff = k0, dim = d', shape = Ty.ShapeVar dd1}
            in
              [f1] --> f1
            end))

    val fn_trace_t = polyVar (N.fn_trace, all([NK],
          fn [Ty.DIM d] => [matrix(Ty.DimVar d)] --> Ty.realTy))
    val fn_trace_f = polyVar (N.fn_trace, all([DK, NK, NK, SK],
          fn [Ty.DIFF k, Ty.DIM d1, Ty.DIM d2, Ty.SHAPE dd1] => let
              val k' = Ty.DiffVar(k, 0)
              val dim1 = Ty.DimVar d1
              val dim2 = Ty.DimVar d2
              val dshape = Ty.ShapeVar dd1
              val f = field(k', dim1, Ty.ShapeExt(Ty.ShapeExt(dshape, dim2), dim2))
              val h = field(k', dim1, dshape)
              in
                [f] --> h
              end))
    val fn_trace_p = polyVar (N.fn_trace, all([DK, NK, NK, SK],
        fn [Ty.DIFF k, Ty.DIM d1, Ty.DIM d2, Ty.SHAPE dd1] => let
        val k' = Ty.DiffVar(k, 0)
        val dim1 = Ty.DimVar d1
        val dim2 = Ty.DimVar d2
        val dshape = Ty.ShapeVar dd1
        val f = poly(k', dim1, Ty.ShapeExt(Ty.ShapeExt(dshape, dim2), dim2))
        val h = poly(k', dim1, dshape)
        in
        [f] --> h
        end))

    val fn_transpose_t = polyVar (N.fn_transpose, all([NK, NK],
          fn [Ty.DIM d1, Ty.DIM d2] =>
              [tensor[Ty.DimVar d1, Ty.DimVar d2]] --> tensor[Ty.DimVar d2, Ty.DimVar d1]))

    val fn_transpose_f = polyVar (N.fn_transpose, all([DK,NK,NK,NK],
          fn [Ty.DIFF k, Ty.DIM d,Ty.DIM a, Ty.DIM b] => let
              val k0 = Ty.DiffVar(k, 0)
              val d' = Ty.DimVar d
              val a' = Ty.DimVar a
              val b' = Ty.DimVar b
              val f = field(k0, d', Ty.Shape[a',b'])
              val h = field(k0, d', Ty.Shape[b',a'])
              in
                [f] --> h
              end))

    val fn_transpose_p = polyVar (N.fn_transpose, all([DK,NK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d,Ty.DIM a, Ty.DIM b] => let
            val k0 = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val a' = Ty.DimVar a
            val b' = Ty.DimVar b
            val f = poly(k0, d', Ty.Shape[a',b'])
            val h = poly(k0, d', Ty.Shape[b',a'])
            in
            [f] --> h
            end))


    val fn_concat_fv2 = polyVar(N.fn_concat, all([DK,NK],
        fn [Ty.DIFF k, Ty.DIM d] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2])}
        in
            [f1, f2] --> f3
        end))

    val fn_concat_pv2 = polyVar(N.fn_concat, all([DK,DK, NK],
        fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d] => let
            val f1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val f2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape []}
            val f3 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2])}
        in
            [f1, f2] --> f3
        end))


    val fn_concat_fm2 = polyVar(N.fn_concat, all([DK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2,  Ty.DimVar  dd1])}

        in
            [f1, f2] --> f3
        end))

    val fn_concat_pm2 = polyVar(N.fn_concat, all([DK, DK, NK,NK],
        fn [Ty.DIFF k1, Ty.DIFF k2, Ty.DIM d, Ty.DIM dd1] => let
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
        val f3 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2,  Ty.DimVar  dd1])}

        in
            [f1, f2] --> f3
        end))

    val fn_concat_fm3 = polyVar(N.fn_concat, all([DK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
            val f4 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3,  Ty.DimVar  dd1])}
        in
            [f1, f2, f3] --> f4
        end))

    val fn_concat_pm3 = polyVar(N.fn_concat, all([DK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1] => let
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
        val f3 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1]}
        val f4 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3,  Ty.DimVar  dd1])}
        in
            [f1, f2, f3] --> f4
        end))

    val fn_concat_ft2 = polyVar(N.fn_concat, all([DK,NK,NK, NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1, Ty.DIM dd2] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2,  Ty.DimVar  dd1, Ty.DimVar dd2])}

        in
            [f1, f2] --> f3
        end))

    val fn_concat_pt2 = polyVar(N.fn_concat, all([DK,DK, NK,NK, NK],
        fn [Ty.DIFF k1,Ty.DIFF k2, Ty.DIM d, Ty.DIM dd1, Ty.DIM dd2] => let
            val f1 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f2 = Ty.T_OField{diff = Ty.DiffVar(k2, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f3 = Ty.T_OField{diff = Ty.DiffVar(k1, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 2,  Ty.DimVar  dd1, Ty.DimVar dd2])}

            in
            [f1, f2] --> f3
            end))

    val fn_concat_fs3 = polyVar(N.fn_concat, all([DK,NK],
        fn [Ty.DIFF k, Ty.DIM d] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f4 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3])}

        in
            [f1, f2, f3] --> f4
        end))

    val fn_concat_ps3 = polyVar(N.fn_concat, all([DK,NK],
        fn [Ty.DIFF k, Ty.DIM d] => let
            val f1 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f2 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f3 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[]}
            val f4 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3])}

            in
                [f1, f2, f3] --> f4
            end))

    val fn_concat_ft3 = polyVar(N.fn_concat, all([DK,NK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1, Ty.DIM dd2] => let
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f3 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
            val f4 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3,  Ty.DimVar  dd1, Ty.DimVar dd2])}

        in
            [f1, f2, f3] --> f4
        end))

    val fn_concat_pt3 = polyVar(N.fn_concat, all([DK,NK,NK,NK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.DIM dd1, Ty.DIM dd2] => let
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
        val f3 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape[Ty.DimVar dd1, Ty.DimVar dd2]}
        val f4 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.Shape([Ty.DimConst 3,  Ty.DimVar  dd1, Ty.DimVar dd2])}

        in
        [f1, f2, f3] --> f4
        end))



(* determinant: restrict to 2x2 and 3x3*)
    val fn_det2_t = monoVar (N.fn_det, [matrix N2] --> Ty.realTy)

    val fn_det3_t = monoVar (N.fn_det, [matrix N3] --> Ty.realTy)

    val fn_det2_f = polyVar (N.fn_det, all([DK, NK], fn [Ty.DIFF k, Ty.DIM d] => let
            fun field' (k, dd) = field(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, [2, 2])
            val s = field' (k0, [])
            in
              [f] --> s
            end))

    val fn_det2_p = polyVar (N.fn_det, all([DK, NK], fn [Ty.DIFF k, Ty.DIM d] => let
            fun field' (k, dd) = poly(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, [2, 2])
            val s = field' (k0, [])
            in
            [f] --> s
            end))


    val fn_det3_f  = polyVar (N.fn_det, all([DK, NK], fn [Ty.DIFF k, Ty.DIM d] => let
            fun field' (k, dd) = field(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, [3,3])
            val s = field' (k0, [])
            in
              [f] --> s
            end))
    val fn_det3_p  = polyVar (N.fn_det, all([DK, NK], fn [Ty.DIFF k, Ty.DIM d] => let
            fun field' (k, dd) = poly(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, [3,3])
            val s = field' (k0, [])
            in
            [f] --> s
            end))

    val fn_inv2_t = let
          val t = matrix N2
          in
            monoVar (N.fn_inv, [t] --> t)
          end

    val fn_inv2_f = polyVar (N.fn_inv, all([DK, NK], fn [Ty.DIFF k, Ty.DIM dim] => let
            fun field' (k, d, dd) = field(k,  Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, dim, [2, 2])
            in
              [f] --> f
            end))

    val fn_inv2_p = polyVar (N.fn_inv, all([DK, NK], fn [Ty.DIFF k, Ty.DIM dim] => let
        fun field' (k, d, dd) = poly(k,  Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
        val k0 = Ty.DiffVar(k, 0)
        val f = field' (k0, dim, [2, 2])
        in
        [f] --> f
        end))

    val fn_inv3_f  = polyVar (N.fn_inv, all([DK, NK], fn [Ty.DIFF k, Ty.DIM dim] => let
            fun field' (k, d, dd) = field(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
            val k0 = Ty.DiffVar(k, 0)
            val f = field' (k0, dim, [3, 3])
            in
              [f] --> f
            end))

    val fn_inv3_p  = polyVar (N.fn_inv, all([DK, NK], fn [Ty.DIFF k, Ty.DIM dim] => let
        fun field' (k, d, dd) = poly(k, Ty.DimVar d, Ty.Shape(List.map Ty.DimConst dd))
        val k0 = Ty.DiffVar(k, 0)
        val f = field' (k0, dim, [3, 3])
        in
        [f] --> f
        end))

    val fn_inv3_t = let
          val t = matrix N3
          in
            monoVar (N.fn_inv, [t] --> t)
          end

    (*function composition*)
    val fn_comp = polyVar(N.fn_comp , all([DK, NK, NK, SK, SK],
            fn [Ty.DIFF k, Ty.DIM d0, Ty.DIM d1, Ty.SHAPE dd0, Ty.SHAPE dd1] => let
                (*val [d0] = dd1*)
                val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d0, shape = Ty.ShapeVar dd0}
                val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d1, shape = Ty.ShapeVar dd1}
            in
                [f1, f2] --> f1
            end
        ))
    val comp = polyVar(N.op_comp , all([DK, NK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d0, Ty.DIM d1, Ty.SHAPE dd0, Ty.SHAPE dd1] => let
            (*val [d0] = dd1*)
            val f1 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d0, shape = Ty.ShapeVar dd0}
            val f2 = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d1, shape = Ty.ShapeVar dd1}
        in
            [f1, f2] --> f1
        end
    ))

    val fn_comp_p = polyVar(N.fn_comp , all([DK, NK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d0, Ty.DIM d1, Ty.SHAPE dd0, Ty.SHAPE dd1] => let
        (*val [d0] = dd1*)
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d0, shape = Ty.ShapeVar dd0}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d1, shape = Ty.ShapeVar dd1}
        in
        [f1, f2] --> f1
        end
        ))
    val comp_p = polyVar(N.op_comp , all([DK, NK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d0, Ty.DIM d1, Ty.SHAPE dd0, Ty.SHAPE dd1] => let
        (*val [d0] = dd1*)
        val f1 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d0, shape = Ty.ShapeVar dd0}
        val f2 = Ty.T_OField{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d1, shape = Ty.ShapeVar dd1}
        in
        [f1, f2] --> f1
        end
        ))



  (* lifted unary math functions; these have both real and scalar-field forms *)
    local
      fun fn_r name = monoVar (name, [Ty.realTy] --> Ty.realTy)
      fun fn_s name = polyVar (N.fn_sqrt, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
            val k' = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val f = field(k', d', Ty.Shape[])
            in
              [f] --> f
            end))
        fun fn_p name = polyVar (N.fn_sqrt, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
            val k' = Ty.DiffVar(k, 0)
            val d' = Ty.DimVar d
            val p = poly(k', d', Ty.Shape[])
            in
                [p] --> p
            end))

    in
    val fn_sqrt_r = fn_r N.fn_sqrt
    val fn_sqrt_s = fn_s N.fn_sqrt
    val fn_sqrt_p = fn_p N.fn_sqrt
    val fn_cos_r  = fn_r N.fn_cos
    val fn_cos_s  = fn_s N.fn_cos
    val fn_cos_p  = fn_p N.fn_cos
    val fn_acos_r = fn_r N.fn_acos
    val fn_acos_s = fn_s N.fn_acos
    val fn_acos_p = fn_p N.fn_acos
    val fn_sin_r  = fn_r N.fn_sin
    val fn_sin_s  = fn_s N.fn_sin
    val fn_sin_p  = fn_p N.fn_sin
    val fn_asin_r = fn_r N.fn_asin
    val fn_asin_s = fn_s N.fn_asin
    val fn_asin_p = fn_p N.fn_asin
    val fn_tan_r  = fn_r N.fn_tan
    val fn_tan_s  = fn_s N.fn_tan
    val fn_tan_p  = fn_p N.fn_tan
    val fn_atan_r = fn_r N.fn_atan
    val fn_atan_s = fn_s N.fn_atan
    val fn_atan_p = fn_p N.fn_atan
    val fn_exp_r  = fn_r N.fn_exp
    val fn_exp_s  = fn_s N.fn_exp
    val fn_exp_p  = fn_p N.fn_exp
    end (* local *)


    val fn_maxF_s = polyVar (N.fn_maxF, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
        val k' = Ty.DiffVar(k, 0)
        val d' = Ty.DimVar d
        val f = field(k', d', Ty.Shape[])
        in
            [f, f] --> f
        end))


    val fn_minF_s = polyVar (N.fn_minF, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
        val k' = Ty.DiffVar(k, 0)
        val d' = Ty.DimVar d
        val f = field(k', d', Ty.Shape[])
        in
            [f, f] --> f
        end))

    val fn_maxF_l = polyVar (N.fn_maxF, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
        val k' = Ty.DiffVar(k, 0)
        val d' = Ty.DimVar d
        val f = poly(k', d', Ty.Shape[])
        in
            [f, f] --> f
        end))


    val fn_minF_l = polyVar (N.fn_minF, all([DK,NK], fn [Ty.DIFF k, Ty.DIM d] => let
        val k' = Ty.DiffVar(k, 0)
        val d' = Ty.DimVar d
        val f = poly(k', d', Ty.Shape[])
        in
            [f, f] --> f
        end))


  (* Math functions that have not yet been lifted to work on fields *)
    local
      fun mk (name, n) =
            monoVar(name, List.tabulate(n, fn _ => Ty.realTy) --> Ty.realTy)
    in
    val fn_atan2_rr = mk(N.fn_atan2, 2)
    val fn_ceil_r   = mk(N.fn_ceil, 1)
    val fn_erf_r    = mk(N.fn_erf, 1)
    val fn_erfc_r   = mk(N.fn_erfc, 1)
    val fn_floor_r  = mk(N.fn_floor, 1)
    val fn_fmod_rr  = mk(N.fn_fmod, 2)
    val fn_log_r    = mk(N.fn_log, 1)
    val fn_log10_r  = mk(N.fn_log10, 1)
    val fn_log2_r   = mk(N.fn_log2, 1)
    val fn_pow_rr   = mk(N.fn_pow, 2)
    val fn_round_r  = mk(N.fn_round, 1)
    val fn_trunc_r  = mk(N.fn_trunc, 1)
    end (* local *)

  (* Query functions *)
    local
      val implicit = fn [Ty.TYPE tv] => [Ty.realTy] --> dynSeq(Ty.T_Var tv)
      val realTy = fn [Ty.TYPE tv] => [Ty.realTy, Ty.realTy] --> dynSeq(Ty.T_Var tv)
      val vec2Ty = let
            val t = tensor[N2]
            in
              fn [Ty.TYPE tv] => [t, Ty.realTy] --> dynSeq(Ty.T_Var tv)
            end
      val vec3Ty = let
            val t = tensor[N3]
            in
              fn [Ty.TYPE tv] => [t, Ty.realTy] --> dynSeq(Ty.T_Var tv)
            end
    in
    val fn_sphere_im = polyVar (N.fn_sphere, all([TK], fn [Ty.TYPE tv] =>
          [Ty.realTy] --> dynSeq(Ty.T_Var tv)))
  (* queries with an explicit position *)
    val fn_sphere1_r = polyVar (N.fn_sphere, all([TK], fn [Ty.TYPE tv] =>
          [Ty.realTy, Ty.realTy] --> dynSeq(Ty.T_Var tv)))
    val fn_sphere2_t = polyVar (N.fn_sphere, all([TK], fn [Ty.TYPE tv] =>
          [tensor[N2], Ty.realTy] --> dynSeq(Ty.T_Var tv)))
    val fn_sphere3_t = polyVar (N.fn_sphere, all([TK], fn [Ty.TYPE tv] =>
          [tensor[N3], Ty.realTy] --> dynSeq(Ty.T_Var tv)))
    end (* local *)

  (* vector distance function *)
    local
      val vec2Ty = let
            val t = tensor[N2]
            in
              [t, t] --> Ty.realTy
            end
      val vec3Ty = let
            val t = tensor[N3]
            in
              [t, t] --> Ty.realTy
            end
    in
    val dist2_t  = monoVar (N.fn_dist, vec2Ty)
    val dist3_t  = monoVar (N.fn_dist, vec3Ty)
    end (* local *)

  (* Sets of strands *)
    local
      fun mkSetFn name = polyVar (name, all([TK], fn [Ty.TYPE tv] => [] --> dynSeq(Ty.T_Var tv)))
    in
    val set_active = mkSetFn N.set_active
    val set_all    = mkSetFn N.set_all
    val set_stable = mkSetFn N.set_stable
    end

  (* functions for getting the number of strands in a set *)
    local
      fun mkNumberOf name = monoVar (name, [] --> Ty.T_Int)
    in
    val fn_numActive = mkNumberOf N.fn_numActive
    val fn_numStable = mkNumberOf N.fn_numStable
    val fn_numStrands = mkNumberOf N.fn_numStrands
    end (* local *)

  (* reduction operators *)
    local
      fun reduction (name, elemTy) =
            monoVar (name, [dynSeq elemTy] --> elemTy)
    in
    val red_all         = reduction (N.fn_all, Ty.T_Bool)
    val red_exists      = reduction (N.fn_exists, Ty.T_Bool)
    val red_max_i       = reduction (N.fn_max, Ty.T_Int)
    val red_max_r       = reduction (N.fn_max, Ty.realTy)
    val red_mean        = reduction (N.fn_mean, Ty.realTy)
    val red_min_i       = reduction (N.fn_min, Ty.T_Int)
    val red_min_r       = reduction (N.fn_min, Ty.realTy)
    val red_product_i   = reduction (N.fn_product, Ty.T_Int)
    val red_product_r   = reduction (N.fn_product, Ty.realTy)
(* FIXME: allow sum on tensor types *)
    val red_sum_i       = reduction (N.fn_sum, Ty.T_Int)
    val red_sum_r       = reduction (N.fn_sum, Ty.realTy)
    val red_variance    = reduction (N.fn_variance, Ty.realTy)
    end (* local *)

  (***** internal variables *****)

  (* load image from nrrd *)
    val fn_load_image = polyVar (Atom.atom "$load_image", all([NK, SK],
            fn [Ty.DIM d, Ty.SHAPE dd] => let
                val d = Ty.DimVar d
                val dd = Ty.ShapeVar dd
                in
                  [Ty.T_String] --> Ty.T_Image{dim=d, shape=dd}
                end))

  (* load dynamic sequence from nrrd *)
    val fn_load_sequence = polyVar (Atom.atom "$load_seqeunce", all([TK],
            fn [Ty.TYPE tv] => [Ty.T_String] --> dynSeq(Ty.T_Var tv)))

  (* integer to real conversion *)
    val i2r = monoVar (Atom.atom "$i2r", [Ty.T_Int] --> Ty.realTy)

  (* identity matrix *)
    val identity = polyVar (Atom.atom "$id", allNK (fn dv => [] --> matrix(Ty.DimVar dv)))

  (* zero tensor *)
    val zero = polyVar (Atom.atom "$zero", all ([SK],
          fn [Ty.SHAPE dd] => [] --> Ty.T_Tensor(Ty.ShapeVar dd)))

  (* NaN tensor *)
    val nan = polyVar (Atom.atom "$nan", all ([SK],
          fn [Ty.SHAPE dd] => [] --> Ty.T_Tensor(Ty.ShapeVar dd)))

  (* sequence subscript *)
    val subscript = polyVar (Atom.atom "$sub", all ([TK, NK],
          fn [Ty.TYPE tv, Ty.DIM d] =>
            [Ty.T_Sequence(Ty.T_Var tv, SOME(Ty.DimVar d)), Ty.T_Int] --> Ty.T_Var tv))

    val dynSubscript = polyVar (Atom.atom "$dynsub", all ([TK],
          fn [Ty.TYPE tv] => [dynSeq(Ty.T_Var tv), Ty.T_Int] --> Ty.T_Var tv))

  (* range expressions *)
    val range = monoVar (Atom.atom "$range", [Ty.T_Int, Ty.T_Int] --> dynSeq Ty.T_Int)

  (* boolean and *)
    val and_b = monoVar (Atom.atom "$and", [Ty.T_Bool, Ty.T_Bool] --> Ty.T_Bool)

(* ----------------------------------------------------------------------------------*)
    (*function polynomial*)
    (* below we offer different number of input variables *)
    val fn_poly_1 = polyVar(N.fn_poly , all([DK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT] => let

        val k0 = Ty.DiffVar(k, 0)
        val t1 = Ty.T_Tensor(Ty.ShapeVar ddT)
        val f0 = Ty.T_Tensor(Ty.ShapeVar ddF)
        val f1 = Ty.T_OField{diff = k0, dim = Ty.DimVar d, shape = Ty.ShapeVar ddF}
        in
        [t1, f0] --> f1
        end
        ))

    val fn_poly_2 = polyVar(N.fn_poly , all([DK, NK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT, Ty.SHAPE ddT2] => let

        val k0 = Ty.DiffVar(k, 0)
        val t1 = Ty.T_Tensor(Ty.ShapeVar ddT)
        val t2 = Ty.T_Tensor(Ty.ShapeVar ddT2)
        val f0 = Ty.T_Tensor(Ty.ShapeVar ddF)
        val f1 = Ty.T_OField{diff = k0, dim = Ty.DimVar d, shape = Ty.ShapeVar ddF}
        in
        [t1, t2, f0] --> f1
        end
        ))

    val fn_poly_3 = polyVar(N.fn_poly , all([DK, NK, SK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT, Ty.SHAPE ddT2, Ty.SHAPE ddT3] => let

        val k0 = Ty.DiffVar(k, 0)
        val t1 = Ty.T_Tensor(Ty.ShapeVar ddT)
        val t2 = Ty.T_Tensor(Ty.ShapeVar ddT2)
        val t3 = Ty.T_Tensor(Ty.ShapeVar ddT3)
        val f0 = Ty.T_Tensor(Ty.ShapeVar ddF)
        val f1 = Ty.T_OField{diff = k0, dim = Ty.DimVar d, shape = Ty.ShapeVar ddF}
        in
        [t1, t2, t3, f0] --> f1
        end
        ))



(* ************************************* FEM FIELD ************************************* *)
(* -------------------  describe function space ------------------- *)
val ms_USq = polyVar (N.ms_USq, all([],
fn [] => let
val l = Ty.T_Int
val m= Ty.T_Mesh
in
[l, l] --> m
end))
val ms_UCq = polyVar (N.ms_UCq, all([],
fn [] => let
val l = Ty.T_Int
val m= Ty.T_Mesh
in
[l, l,l] --> m
end))

val re_Lagrange = polyVar (N.re_Lagrange, all([],
fn [] => [] --> Ty.T_Element
))
val re_P = polyVar (N.re_P, all([],
fn [] => [] --> Ty.T_Element
))



val fn_functionspace = polyVar (N.fn_functionspace, all([],
							fn [] => let
							    val m= Ty.T_Mesh
							    val e = Ty.T_Element
							    val d = Ty.T_Int
							    val fs= Ty.T_FnSpace
							in
							    [m, e, d] --> fs
							end))
val fn_tensorfunctionspace = polyVar (N.fn_tensorfunctionspace, all([],
							fn [] => let
							    val m= Ty.T_Mesh
							    val e = Ty.T_Element
							    val d = Ty.T_Int
							    val s = Ty.T_Sequence(Ty.T_Int,NONE)
							    val fs= Ty.T_FnSpace
							in
							    [m, e, d, s] --> fs
							end))
(* -------------------  convert pointer to ofield  ------------------- *)
    (* create pde solution. fem () *)
    val fn_convert_f = polyVar (N.fn_convert, all([DK, NK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val k = Ty.DiffVar(k, 1)
            val d = Ty.DimVar d
            val dd = Ty.ShapeVar dd
            val p = poly(k, d, dd)
            val f= Ty.T_FemFld{diff=k, dim=d, shape=dd}
            val Ff= Ty.T_Field{diff=k, dim=d, shape=dd}  (*change back*)
            val s = Ty.T_String
        in
            [f, s] --> p
        end))

    (* expects argument to have field pointer, describe fnspace, path to data *)
    val fn_convert_rm = polyVar (N.fn_convert, all([DK, NK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val dd = Ty.ShapeVar dd
            val f=  Ty.T_FemFld{diff=k, dim=d, shape=dd}
            val fs= Ty.T_FnSpace
            val s = Ty.T_String
            val p = poly(k, d, dd)
            val Ff= Ty.T_Field{diff=k, dim=d, shape=dd}  (*change back*)
        in
            [f, fs, s] --> p
        end))

(* -------------------  inside fem field  ------------------- *)
        (*inside fem field *)
        val fn_insideO = polyVar (N.fn_insideO, all([DK, NK, SK],
            fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
            val k = Ty.DiffVar(k, 0)
            val d = Ty.DimVar d
            val dd = Ty.ShapeVar dd
            val f =  Ty.T_OField{diff=k, dim=d, shape=dd}
            in
                [Ty.T_Tensor(Ty.Shape[d]), f] --> Ty.T_Bool
            end))

	val fn_convert_Tracker_rm = polyVar (N.fn_convert, all([DK, NK, SK],
           fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
           val k = Ty.DiffVar(k, 0)
           val d = Ty.DimVar d
           val dd = Ty.ShapeVar dd
           val f=  Ty.T_FemFld{diff=k, dim=d, shape=dd}
           val fs= Ty.T_FnSpace
           val s = Ty.T_String
           val r = Ty.T_Int
           val p = poly(k, d, dd)
           in
           [f, s,r] --> p
           end))

(* ************************************* Probe other field ************************************* *)
(* -------------------  probe ofield  ------------------- *)

    val fn_inst_1_FT = polyVar (N.fn_inst, all([DK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dF = Ty.ShapeVar ddF
        val dT = Ty.ShapeVar ddT
        val f =  Ty.T_OField{diff=k, dim=d, shape=dF}
        in
        [f, Ty.T_Tensor dT] --> Ty.T_Tensor dF
        end))

    val fn_inst_1_TF = polyVar (N.fn_inst, all([DK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dF = Ty.ShapeVar ddF
        val dT = Ty.ShapeVar ddT
        val f =  Ty.T_OField{diff=k, dim=d, shape=dF}
        in
            [Ty.T_Tensor dT, f] --> Ty.T_Tensor dF
        end))


    val fn_inst_2 = polyVar (N.fn_inst, all([DK, NK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT, Ty.SHAPE ddT2] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dF = Ty.ShapeVar ddF
        val dT = Ty.ShapeVar ddT
        val d2T = Ty.ShapeVar ddT2
        val f =  Ty.T_OField{diff=k, dim=d, shape=dF}
        in
        [f, Ty.T_Tensor dT, Ty.T_Tensor d2T] --> Ty.T_Tensor dF
        end))

    val fn_inst_3 = polyVar (N.fn_inst, all([DK, NK, SK, SK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT, Ty.SHAPE ddT2, Ty.SHAPE ddT3] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dF = Ty.ShapeVar ddF
        val dT = Ty.ShapeVar ddT
        val d2T = Ty.ShapeVar ddT2
        val d3T = Ty.ShapeVar ddT3
        val f =  Ty.T_OField{diff=k, dim=d, shape=dF}
        in
        [f, Ty.T_Tensor dT, Ty.T_Tensor d2T, Ty.T_Tensor d3T] --> Ty.T_Tensor dF
        end))

(* ************************************* Other tensor op ************************************* *)

(* -------------------  swap   ------------------- *)
    val fn_swap2 = polyVar(N.fn_swap, all([DK,NK,SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [Ty.T_Int, f,f] --> f
        end))

    val fn_swap3 = polyVar(N.fn_swap, all([DK,NK,SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [Ty.T_Int, f,f, f] --> f
        end))


    val fn_swap4 = polyVar(N.fn_swap, all([DK,NK,SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [Ty.T_Int, f,f, f, f] --> f
        end))


    val fn_swap5 = polyVar(N.fn_swap, all([DK,NK,SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [Ty.T_Int, f,f, f, f,f] --> f
        end))


    val fn_swap6 = polyVar(N.fn_swap, all([DK,NK,SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE dd] => let
        val f = Ty.T_Field{diff = Ty.DiffVar(k, 0), dim = Ty.DimVar d, shape = Ty.ShapeVar dd}
        in
        [Ty.T_Int, f,f, f, f,f,f] --> f
        end))
(* ************************************* Push FEM op to surface ************************************* *)
    val sp_getCell =  polyVar (N.sp_getCell, all([DK, NK, SK, SK],
        fn [Ty.DIFF k, Ty.DIM d, Ty.SHAPE ddF, Ty.SHAPE ddT] => let
        val k = Ty.DiffVar(k, 0)
        val d = Ty.DimVar d
        val dF = Ty.ShapeVar ddF
        val dT = Ty.ShapeVar ddT
        val f =  Ty.T_OField{diff=k, dim=d, shape=dF}
        in
        [f, Ty.T_Tensor dT] --> Ty.T_Int
        end))


    end (* local *)

  end
