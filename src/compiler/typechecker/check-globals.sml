(* check-globals.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckGlobals : sig

  (* type check the global declarations of a program.  We partition the result
   * into constants (which can only depend on other constants), inputs (which
   * can depend on contants), and the other globals (functions and variables).
   *)
    val check : Env.t * Env.context * ParseTree.global_dcl list -> {
            const_dcls : AST.var_dcl list,
            input_dcls : (AST.var_dcl * string option) list,
            other_dcls : AST.global_dcl list,
            env : Env.t
          }

  end = struct

    structure PT = ParseTree
    structure Ty = Types
    structure TU = TypeUtil
    structure E = Env

    val err = TypeError.error

    datatype token = datatype TypeError.token

  (* tegged union of the different kinds of global declarations *)
    datatype dcl_kind
      = CONST of AST.var_dcl
      | INPUT of (AST.var_dcl * string option)
      | OTHER of AST.global_dcl
      | ERROR

  (* check the rhs initialization of a 'const' or 'input' declaration.  Return
   * (v, e), where v is the constant value and e is the AST version of the rhs.
   *)
    fun chkRHS (env, cxt, isInput, x', e) = let
          val (e', ty') = CheckExpr.check (env, cxt, e)
          val v = (case CheckConst.eval (cxt, isInput, e')
                 of SOME v => v
                  | NONE => ConstExpr.Expr e' (* error has already been reported *)
                (* end case *))
          val e' = ConstExpr.valueToExpr v
          val lhsTy = Var.monoTypeOf x'
          in
            case Util.coerceType (lhsTy, (e', ty'))
             of SOME e' => (v, e')
              | NONE => (
                  err (cxt, [
                      S "definition of ", V x', S " has wrong type\n",
                      S "  expected: ", TY lhsTy, S "\n",
                      S "  found:    ", TY ty'
                    ]);
                  (v, e'))
            (* end case *)
          end

    fun chkDcl (env, cxt, dcl) = (case dcl
           of PT.GD_Mark m => chkDcl (E.withEnvAndContext (env, cxt, m))
            | PT.GD_Const(ty, {span, tree=x}, optDefn) => let
                val ty = CheckType.check (env, cxt, ty)
                val x' = Var.new(x, span, Var.ConstVar, ty)
                val (v, e') = (case optDefn
                       of SOME e => chkRHS (env, cxt, false, x', e)
                        | NONE => raise Fail "FIXME: need to look for command-line definition"
                      (* end case *))
                in
                (* check that const variables have valid types *)
                  if not(TU.isValueType ty)
                    then err (cxt, [S "const variable ", V x', S " has invalid type ", TY ty])
                    else ();
                  E.checkForRedef (env, cxt, x);
                  ConstExpr.define(x', v);
                  (CONST(x', SOME e'), E.insertGlobal(env, cxt, x, x'))
                end
            | PT.GD_Input(ty, {span, tree=x}, optDesc, optDefn) => let
                val _ ="GD_Input pre "
                val ty = CheckType.check (env, cxt, ty)
                val x' = Var.new(x, span, Var.InputVar, ty)
                val rhs = (case optDefn
                       of NONE => NONE
                        | SOME e => SOME(#2 (chkRHS (env, cxt, true, x', e)))
                      (* end case *))
                val _ ="GD_Input middle "
                in
                (* check that input variables have valid types *)
                  if not(TU.isValueType ty orelse TU.isImageType ty orelse TU.isMeshType ty)
                    then err (cxt, [S "input variable ", V x', S " has invalid type ", TY ty])
                    else ();
                  E.checkForRedef (env, cxt, x);
                  E.recordProp (env, Properties.HasGlobals);
                  E.recordProp (env, Properties.HasInputs);
                  (INPUT((x', rhs), optDesc), E.insertGlobal(env, cxt, x, x'))
                end
            | PT.GD_Var varDcl => let
                val (x, x', optDefn) = CheckStmt.checkVarDecl (env, cxt, Var.GlobalVar, varDcl)
                in
                  E.checkForRedef (env, cxt, x);
                  Env.recordProp (env, Properties.HasGlobals);
                  (OTHER(AST.D_Var(x', optDefn)), E.insertGlobal(env, cxt, x, x'))
                end
            | PT.GD_Func(ty, {span, tree=f}, params, body) => let
                val ty' = CheckType.check(env, cxt, ty)
                val env' = E.functionScope (env, ty', f)
                val (params', env') = CheckParams.check (env', cxt, Var.FunParam, params)
                val body' = (case body
                       of PT.FB_Expr e => let
                            val eTy = CheckExpr.check (env', cxt, e)
                            in
                              case Util.coerceType(ty', eTy)
                               of SOME e' => AST.S_Return e'
                                | NONE => (
                                    err (cxt, [
                                        S "type of function body does not match return type\n",
                                        S "  expected: ", TY ty', S "\n",
                                        S "  found:    ", TY(#2 eTy)
                                      ]);
                                    AST.S_Block[])
                              (* end case *)
                            end
                        | PT.FB_Stmt s => CheckStmt.check(env', cxt, s)
                      (* end case *))
                val fnTy = Ty.T_Fun(List.map Var.monoTypeOf params', ty')
                val f' = Var.new (f, span, AST.FunVar, fnTy)
                in
(* QUESTION: should we also check for redefinition of basis functions? *)
                  E.checkForRedef (env, cxt, f);
                  (OTHER(AST.D_Func(f', params', body')), E.insertFunc(env, cxt, f, f'))
                end
                    | PT.GD_FieldFunc(ty, bindF, bindX, body) => (case CheckType.check(env, cxt, ty)
                 of ty' as Ty.T_Field{diff, dim, shape} => let
                      val f = Var.new(#tree bindF, #span bindF, Var.GlobalVar, ty')
                      val xTy = Ty.T_Tensor(Ty.Shape[dim])
                      val resTy = Ty.T_Tensor shape
(* QUESTION: should we check the arity of dim? *)
                      val x = Var.new(#tree bindX, #span bindX, Var.FunParam, xTy)
                      val env' = Env.insertLocal(
(* QUESTION: should we have a new kind of scope for these field functions? *)
                            Env.functionScope(env, resTy, #tree bindF),
                            cxt, #tree bindX, x)
                      val bodyTy = CheckExpr.check (env', cxt, body)
                      in
                        case Util.coerceType (resTy, bodyTy)
                         of SOME e => (
                                OTHER(AST.D_DiffFunc(f, [x], e)),
                                Env.insertGlobal(env, cxt, #tree bindF, f)
                              )
                          | NONE => (
                              err (cxt, [
                                  S "type of r.h.s. definition for '", A(#tree bindF),
                                  S "' does not match declaration",
                                  S "  expected: ", TY resTy, S "\n",
                                  S "  found:    ", TY(#2 bodyTy)
                                ]);
                              (ERROR, env))
                        (* end case *)
                      end
                  | ty' => (
                      err (cxt, [S "expected field type for '", A(#tree bindF), S "'"]);
                      (ERROR, env))
                (* end case *))        
          (* end case *))

    fun check (env, cxt, globs) = let
          fun chk (env, [], cdecls, idecls, gdecls) = {
                  const_dcls = List.rev cdecls,
                  input_dcls = List.rev idecls,
                  other_dcls = List.rev gdecls,
                  env = env
                }
            | chk (env, dcl::dcls, cdecls, idecls, gdecls) = (
                case chkDcl (env, cxt, dcl)
                 of (CONST dcl, env) => chk (env, dcls, dcl::cdecls, idecls, gdecls)
                  | (INPUT dcl, env) => chk (env, dcls, cdecls, dcl::idecls, gdecls)
                  | (OTHER dcl, env) => chk (env, dcls, cdecls, idecls, dcl::gdecls)
                  | (ERROR, env) => chk (env, dcls, cdecls, idecls, gdecls)
                (* end case *))
          in
            chk (env, globs, [], [], [])
          end

  end
