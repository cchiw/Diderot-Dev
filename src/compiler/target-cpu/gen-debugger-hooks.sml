(* gen-debugger-hooks.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure GenDebuggerHooks : sig

  (* generate the extra C wrapper functions used by the debugger to query the program
   * state.
   * NOTE: the names of these functions must agree with the names used in the JSON
   * description of the program's API.
   *)
    val gen : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  end = struct

    structure Env = CodeGenEnv
    structure IR = TreeIR
    structure TSV = TreeStateVar
    structure CL = CLang
    structure GenLib = GenLibraryInterface
    structure Util = GenInputsUtil

    val externC = ["extern \"C\""]

  (* Generate the get function for a state variable.  The basic form is:
   *
   *    extern "C" bool NS_STRAND_get_VAR (NS_world_t *wrld, uint32_t id, void *outPtr)
   *    {
   *        NS::world *w = reinterpret_cast<NS::world *>(wrld);
   *        if (w->_strands.validIndex(id) && w->_strands.isAlive(id)) {
   *            TY *out = reinterpret_cast<TY *>(outPtr);
   *            *out = w->_strands.XXX_state(id)->VAR;
   *            return false;
   *        }
   *        else {
   *            return true;
   *        }
   *    }
   *
   * where
   *    NS      - the name space
   *    STRAND  - the name of the strand type
   *    VAR     - the name of the variable
   *    TY      - the C type for representing the variable
   *    XXX     - either "in" or "local", depending on whether the state variable is
   *              shared or not.
   *)
    fun mkGetFn (env, strandName) = let
          val spec = Env.target env
          val ns = #namespace spec
          val wrldParam = CL.PARAM([], CL.T_Ptr(GenLib.worldTy spec), "wrld")
          val idParam = CL.PARAM([], CL.uint32, "id")
          val outParam = CL.PARAM([], CL.voidPtr, "outPtr")
          val wrldPtrTy = CL.T_Ptr(CL.T_Named(ns ^ "::world"))
          val wrldDcl = CL.mkDeclInit(wrldPtrTy, "w",
                CL.mkReinterpretCast(wrldPtrTy, CL.mkVar "wrld"))
          val strands = CL.mkIndirect(CL.mkVar "w", "_strands")
          val idV = CL.mkVar "id"
          fun getFn sv = let
                val fname = concat[ns, "_", strandName, "_get_", TSV.name sv]
		val state = CL.mkDispatch(
		      strands,
		      if TSV.isShared sv then "in_state" else "local_state",
		      [CL.mkVar "id"])
                in
                  CL.D_Func(
                    externC, CL.boolTy, [], fname,
                    [wrldParam, idParam, outParam],
                    CL.mkBlock[
                        wrldDcl,
                        CL.mkIfThenElse(
                          CL.mkBinOp(
                            CL.mkDispatch(strands, "validIndex", [idV]),
                            CL.#&&,
                            CL.mkDispatch(strands, "isAlive", [idV])),
                          CL.mkBlock[
			      Util.copyToC {
				  env = env,
				  ty = TSV.apiTy sv,
				  src = CL.mkIndirect(state, TSV.qname sv),
				  dst = CL.E_UnOp(CL.%*, Util.castAPIPtr{
				      env = env, ty = TSV.apiTy sv, src = CL.mkVar "outPtr"
				    })
				},
                              CL.mkReturn(SOME(CL.mkVar "false"))
                            ],
                          CL.mkReturn(SOME(CL.mkVar "true")))
                      ])
                end
          in
            getFn
          end

    fun gen (env, prog) = let
          val IR.Program{strand=IR.Strand{name, state, ...}, ...} = prog
          in
            CL.D_Comment["debugger hooks"] ::
            List.map (mkGetFn (env, Atom.toString name)) state
          end

  end
