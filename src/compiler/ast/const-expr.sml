(* const-expr.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * TODO: add support for evaluating constants from the command-line
 *)

structure ConstExpr : sig

  (* compile-time constant values *)
    datatype t
      = String of string
      | Bool of bool
      | Int of IntLit.t
      | Real of RealLit.t
      | Tensor of t list * Types.ty
      | Seq of t list * Types.ty
      | Expr of AST.expr        (* for more complicated tensor-valued expressions *)

  (* a property to attach to 'const' variables to track their value *)
    val define : Var.t * t -> unit
    val valueOf : Var.t -> t option

  (* convert a constant value to an AST expression *)
    val valueToExpr : t -> AST.expr

  (* return the type of a constant value *)
    val typeOfConst : t -> Types.ty

  end = struct

    structure L = Literal
    structure Ty = Types

  (* compile-time constant values *)
    datatype t
      = String of string
      | Bool of bool
      | Int of IntLit.t
      | Real of RealLit.t
      | Tensor of t list * Ty.ty
      | Seq of t list * Ty.ty
      | Expr of AST.expr        (* for more complicated tensor-valued expressions *)

  (* a property to attach to 'const' variables to track their value *)
    local
      val {peekFn : Var.t -> t option, setFn, ...} =
            Var.newProp (fn x => raise Fail("undefined constant " ^ Var.uniqueNameOf x))
    in
    val define = setFn
    val valueOf = peekFn
    end (* local *)

    fun typeOfConst (String _) = Ty.T_String
      | typeOfConst (Bool _) = Ty.T_Bool
      | typeOfConst (Int _) = Ty.T_Int
      | typeOfConst (Real _) = Ty.realTy
      | typeOfConst (Tensor(_, ty)) = ty
      | typeOfConst (Seq(_, ty)) = ty
      | typeOfConst (Expr e) = TypeOf.expr e

    fun valueToExpr (String s) = AST.E_Lit(L.String s)
      | valueToExpr (Bool b) = AST.E_Lit(L.Bool b)
      | valueToExpr (Int i) = AST.E_Lit(L.Int i)
      | valueToExpr (Real r) = AST.E_Lit(L.Real r)
      | valueToExpr (Tensor(vs, ty)) = AST.E_Tensor(List.map valueToExpr vs, ty)
      | valueToExpr (Seq(vs, ty)) = AST.E_Seq(List.map valueToExpr vs, ty)
      | valueToExpr (Expr e) = e

  end
