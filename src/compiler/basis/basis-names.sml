(* basis-names.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Builtin names used for Basis functions.
 *)

structure BasisNames =
  struct

  (* import the operator names from parse-tree/operators.sml *)
    open Operators

  (* reduction operators *)
    val fn_all = Atom.atom "all"
    val fn_exists = Atom.atom "exists"
    val fn_max = Atom.atom "max"                (* also used as a regular function *)
    val fn_mean = Atom.atom "mean"
    val fn_min = Atom.atom "min"                (* also used as a regular function *)
    val fn_product = Atom.atom "product"
    val fn_sum = Atom.atom "sum"
    val fn_variance = Atom.atom "variance"

  (* function names *)
    val fn_border = Atom.atom "border"
    val fn_clamp = Atom.atom "clamp"
    val fn_det = Atom.atom "det"
    val fn_dist = Atom.atom "dist"
    val fn_evecs = Atom.atom "evecs"
    val fn_evals = Atom.atom "evals"
    val fn_inside = Atom.atom "inside"
    val fn_inv = Atom.atom "inv"
    val fn_length = Atom.atom "length"
    val fn_lerp = Atom.atom "lerp"
    val fn_clerp = Atom.atom "clerp"
    val fn_max = Atom.atom "max"
    val fn_min = Atom.atom "min"
    val fn_mirror = Atom.atom "mirror"
    val fn_modulate = Atom.atom "modulate"
    val fn_normalize = Atom.atom "normalize"
    val fn_numActive = Atom.atom "numActive"
    val fn_numStable = Atom.atom "numStable"
    val fn_numStrands = Atom.atom "numStrands"
    val fn_size = Atom.atom "size"
    val fn_trace = Atom.atom "trace"
    val fn_transpose = Atom.atom "transpose"
    val fn_wrap = Atom.atom "wrap"
    val fn_concat = Atom.atom "concat"
    val fn_comp = Atom.atom "compose"
    val fn_printIR = Atom.atom "printIR"        (*print EIN term*)
    val fn_krns = Atom.atom "KRNS"
    (* ofields math functions *)
    val fn_cfe = Atom.atom     "cfexp"        (*makes everything a field*)
    val fn_cfe_12 = Atom.atom "cfexpOne"      (* makes first var a tensor*)
    val fn_cfe_21 = Atom.atom "expression"    (*makes everything a tensor*)
    val fn_diff = Atom.atom "setDiffVar"       (* differentiate in respect to a var *)

    val fn_inst = Atom.atom "inst"              (*probe field *)
    val fn_instR = Atom.atom "instR"
    val fn_convert = Atom.atom "FEM"
    val fn_convertTracker = Atom.atom "convertTracker"
    val fn_insideO = Atom.atom "insideF"
    val fn_sumR = Atom.atom "SUM" 
    val fn_maxR = Atom.atom "MAX" 
    val fn_minR = Atom.atom "MIN" 
        
(* standard math functions *)
    val fn_abs = Atom.atom "abs"
    val fn_acos = Atom.atom "acos"
    val fn_asin = Atom.atom "asin"
    val fn_atan = Atom.atom "atan"
    val fn_atan2 = Atom.atom "atan2"
    val fn_ceil = Atom.atom "ceil"
    val fn_cos = Atom.atom "cos"
    val fn_erf = Atom.atom "erf"
    val fn_erfc = Atom.atom "erfc"
    val fn_exp = Atom.atom "exp"
    val fn_floor = Atom.atom "floor"
    val fn_fmod = Atom.atom "fmod"
    val fn_log = Atom.atom "log"
    val fn_log10 = Atom.atom "log10"
    val fn_log2 = Atom.atom "log2"
    val fn_pow = Atom.atom "pow"
    val fn_round = Atom.atom "round"
    val fn_sin = Atom.atom "sin"
    val fn_sqrt = Atom.atom "sqrt"
    val fn_tan = Atom.atom "tan"
    val fn_trunc = Atom.atom "trunc"
    val fn_swap = Atom.atom "swap"


  (* Query function names *)
    val fn_sphere = Atom.atom "sphere"

  (* Sets of strands *)
    val set_active = Atom.atom "active"
    val set_all    = Atom.atom "all"
    val set_stable = Atom.atom "stable"

  (* kernel names *)
    val kn_bspln3 = Atom.atom "bspln3"
    val kn_bspln5 = Atom.atom "bspln5"
    val kn_c4hexic = Atom.atom "c4hexic"
    val kn_ctmr = Atom.atom "ctmr"
    val kn_tent = Atom.atom "tent"
    val kn_c1tent = Atom.atom "c1tent" (* for backwards compatibility to vis12 *)
    val kn_c2tent = Atom.atom "c2tent" (* for backwards compatibility to vis12 *)
    val kn_c2ctmr = Atom.atom "c2ctmr" (* for backwards compatibility to vis12 *)

    (* mesh *)
    val ms_USq = Atom.atom "UnitSquareMesh"
    val ms_UCq = Atom.atom "UnitCubeMesh"
    (* reference elements *)
    val re_Lagrange = Atom.atom "Lagrange"
    val re_P = Atom.atom "P"
    (*function space*)
    val fn_functionspace = Atom.atom "FunctionSpace"
    val fn_tensorfunctionspace = Atom.atom "TensorFunctionSpace"
    (*push support*)
    val sp_getCell = Atom.atom "GetCell"

  end
