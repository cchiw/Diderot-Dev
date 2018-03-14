(* gen-globals.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure GenGlobals : sig

  (* generate the struct declaration for the global variables (if present) *)
    val gen : {
            env : CodeGenEnv.t,
            consts : TreeIR.global_var list,
            inputs : TreeIR.input list,
            globals : TreeIR.global_var list
          } -> CLang.decl list

  end = struct

    structure GV = TreeGlobalVar
    structure CL = CLang
    structure RN = CxxNames
    structure Env = CodeGenEnv
    structure ToCxx = TreeToCxx

  (* make a field declaration for a global *)
    fun mkField env gv = CL.D_Var([], ToCxx.trType(env, GV.ty gv), [], GV.qname gv, NONE)

  (* is a global variable an image? *)
    fun isImage gv = (case GV.ty gv
           of TreeTypes.ImageTy _ => true
            | _ => false
          (* end case *))

  (* generate a statement to free the image data of an image *)
    fun freeImg gv = CL.mkExpStm(
          CL.mkDispatch(CL.mkIndirect(CL.mkVar "this", GV.qname gv), "free_data", []))

    fun gen {env, consts, inputs, globals} =
          if #hasGlobals(Env.target env)
            then let
	    (* collect all globals *)
(* FIXME: what about constants? *)
              val gvs = List.foldr (fn (inp, gvs) => Inputs.varOf inp :: gvs) globals inputs
            (* convert to fields *)
              val fields = List.map (mkField env) gvs
(* FIXME: we will need to do something about global sequences too at some point *)
(* QUESTION: who is responsible for freeing the memory if we are in a library? *)
            (* if there are any images, then define a destructor to free their storage *)
              val fields = (case List.filter isImage gvs
		     of [] => fields
                      | imgs => let
			  val body = CL.mkBlock(List.map freeImg imgs)
		          val dtor = CL.D_Destr([], [], "globals", SOME body)
                          in
                            fields @ [dtor]
                          end
                    (* end case *))
              in
                [CL.D_ClassDef{
                    name = "globals", args = NONE, from = NONE,
                    public = fields, protected = [], private = []
                  }]
              end
            else []

  end
