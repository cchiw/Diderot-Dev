(* cxx-names.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure CxxNames : sig

 (* the base type of the world struct *)
    val worldBaseTyName : string

  (* the world type *)
    val worldTyName : string
    val worldPtrTy : CLang.ty

  (* the global variables struct *)
    val globalTyName : string
    val globalPtrTy : CLang.ty

  (* the pointer to the command-line options struct *)
    val optionsPtrTy : CLang.ty

  (* standard names for accessing globals and state variables *)
    val workerVar    : CLang.var
    val workerParam  : CLang.param
    val worldVar     : CLang.var
    val worldParam   : CLang.param
    val globalsVar   : CLang.var
    val globalsParam : CLang.param
    val selfVar      : CLang.var
    val selfLocalVar : CLang.var
    val selfInVar    : CLang.var
    val selfOutVar   : CLang.var

  (* get the strand array struct *)
    val strandArray : CodeGenEnv.t -> CLang.exp

  (* the strand type *)
    val strandTyName : string -> string
    val strandTy     : string -> CLang.ty
    val localTyName  : string -> string         (* name of sub-struct that holds local state *)
    val sharedTyName : string -> string         (* name of sub-struct that holds shared state *)

    val inputsTyName : string
    val inputsPtrTy : CLang.ty

  (* image type names *)
    val imageTyName  : int -> string
    val qImageTyName : int -> string

  (* wrapper struct for tensor types (to enable overload resolution) *)
    val tensorStruct    : int list -> string
    val tensorTy        : int list -> CLang.ty
    val tensorRefStruct : int list -> string
    val tensorRefTy     : int list -> CLang.ty

  (* vector related names *)
    val vecTyName : int -> string
    val vecTy     : int -> CLang.ty
    val vscale    : int -> string
    val vsum      : int -> string
    val vdot      : int -> string
    val vclamp    : int -> string
    val vlerp     : int -> string
    val vceiling  : int -> string
    val vfloor    : int -> string
    val vround    : int -> string
    val vtrunc    : int -> string
    val vtoi      : int -> string
    val vcons     : int -> string
    val vload     : int -> string
    val vpack     : int -> string

  (* inside test: args are dim and support *)
    val inside    : int * int -> string

    val fn_makeTransformHelper : string -> string
    val fn_makeTransform : string -> string
    val fn_helpMakeJacobian : string -> string
    val fn_makeJacobian : string -> string
    val fn_helpInverseJacobian: string -> string
    val fn_makeJacobianInverse : string -> string
    val fn_helpJIs : string -> string
    val fn_jIs : string -> string
    val fn_helpTranslateCoordinates: string -> string
    val fn_makeFindCell : string -> string
    val fn_helpEvalBasis : string -> string
    val fn_ProbePhi : string -> string
    val fn_makeEval : string -> int -> int list -> string
    val fn_makeEvalRange : string -> int -> int list -> string
    val mkFemPrefix : meshElem.fnspace -> string
    val mkFemPrefixMD : meshElem.mesh * int ->string
    val fn_makePhiDerv : string -> string
    val checkCell: string
    val fn_makeFDerv : string -> string
    val fn_affineDerv : string -> int -> string
					     

    end = struct

    structure CL = CLang
    structure Env = CodeGenEnv

    val diderotNS = "diderot::"

  (* the base type of the world struct *)
    val worldBaseTyName = "world_base"

  (* the worker cache type *)
    val workerTyName = "worker_cache"
    val workerPtrTy = CL.T_Ptr(CL.T_Named workerTyName)

  (* the world type *)
    val worldTyName = "world"
    val worldPtrTy = CL.T_Ptr(CL.T_Named worldTyName)

  (* the global variables struct *)
    val globalTyName = "globals"
    val globalPtrTy = CL.T_Ptr(CL.T_Named globalTyName)

  (* the pointer to the command-line options struct *)
    val optionsPtrTy = CL.T_Ptr(CL.T_Named "diderot::options")

    val workerVar    = "worker"
    val workerParam  = CL.PARAM([], workerPtrTy, workerVar)
    val worldVar     = "wrld"
    val worldParam   = CL.PARAM([], worldPtrTy, worldVar)
    val globalsVar   = "glob"
    val globalsParam = CL.PARAM([], globalPtrTy, globalsVar)
    val selfVar      = "self"
    val selfLocalVar = "selfLocal"
    val selfInVar    = "selfIn"
    val selfOutVar   = "selfOut"

    fun strandArray env = CL.mkIndirect(CL.mkVar(Env.world env), "_strands")

    fun strandTyName s = s ^ "_strand"
    fun strandTy s = CL.T_Named(strandTyName s)
    fun localTyName s = s ^ "_local"
    fun sharedTyName s = s ^ "_shared"

    val inputsTyName = "cmd_line_inputs"
    val inputsPtrTy  = CL.T_Ptr(CL.T_Named inputsTyName)

    fun imageTyName d = String.concat["image", Int.toString d, "d"]
    fun qImageTyName d = String.concat[diderotNS, "image", Int.toString d, "d"]

    fun tensorStruct shape = "tensor_" ^ String.concatWithMap "_" Int.toString shape
    fun tensorTy shape = CL.T_Named(tensorStruct shape)
    fun tensorRefStruct shape = "tensor_ref_" ^ String.concatWithMap "_" Int.toString shape
    fun tensorRefTy shape = CL.T_Named(tensorRefStruct shape)

    fun vName name w = name ^ Int.toString w
    val vecTyName = vName "vec"
    fun vecTy w = CL.T_Named(vecTyName w)
    val vscale = vName "vscale"
    val vsum = vName "vsum"
    val vdot = vName "vdot"
    val vclamp = vName "vclamp"
    val vlerp = vName "vlerp"
    val vceiling = vName "vceiling"
    val vfloor = vName "vfloor"
    val vround = vName "vround"
    val vtrunc = vName "vtrunc"
    val vtoi = vName "vtoi"
    val vcons = vName "vcons"
    val vload = vName "vload"
    val vpack = vName "vpack"

    fun inside (d, s) = String.concat["inside", Int.toString d, "Ds", Int.toString s]
    (*fem function name*)
    fun setPrefix name prefix = String.concat[name,"_",prefix]
    val fn_makeTransformHelper = setPrefix "makeTransformHelp"
    val fn_makeTransform = setPrefix "makeTransform"
    val fn_helpMakeJacobian = setPrefix "helpMakeJacobian"
    val fn_makeJacobian = setPrefix "makeJacobian"
    val fn_helpInverseJacobian = setPrefix "helpInverseJacobian"
    val fn_makeJacobianInverse = setPrefix "makeJacobianInverse"
    val fn_helpJIs = setPrefix "helpJIs"
    val fn_jIs = setPrefix "jIs"
    val fn_helpTranslateCoordinates = setPrefix "helpTranslateCoordinates"
    val fn_makeFindCell = setPrefix  "wrapcell"
    val fn_helpEvalBasis = setPrefix "helpEvalBasis"
    val fn_ProbePhi = setPrefix "ProbePhi"
    fun fn_makeEval a level  shape =
      let
	  val shapeModDerv = shape
      in
	  "s_"^((String.concatWith "_" (List.map Int.toString shapeModDerv))^(setPrefix "makeEval" a)^"_"^(if level = 0 then "" else Int.toString(level)))
      end
    fun fn_makeEvalRange a level shape =
        let
            val shapeModDerv = shape
        in
            "s_"^((String.concatWith "_" (List.map Int.toString shapeModDerv))^(setPrefix "makeEvalRange" a)^"_"^(if level = 0 then "" else Int.toString(level)))
        end



    val fn_makePhiDerv = setPrefix "makePhiDerv"
    val fn_makeFDerv = setPrefix "makeFDerv"
    fun fn_affineDerv a level = (setPrefix "affineDerv" a)^"_"^(if level = 0 then "" else Int.toString(level))

    fun mkFemPrefixMD (mesh, d) = String.concat[meshElem.toStringMesh(mesh),"_",Int.toString(d)]
    fun mkFemPrefix fnspace = meshElem.toStringfnspace fnspace
    val checkCell = "checkCell"
    val GetCell  = "GetCell"

end
