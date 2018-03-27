(* normalize.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Normalize : sig

    val rewrite : HighIR.program -> HighIR.program

    val promote : HighIR.program -> HighIR.program

  end = struct

    structure IR = HighIR
    structure Op = HighOps
    structure V = IR.Var
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntUnused               = ST.newCounter "high-opt:unused"

    structure UnusedElim = UnusedElimFn (
        structure IR = IR
        val cntUnused = cntUnused)

    fun useCount (IR.V{useCnt, ...}) = !useCnt

  (* adjust a variable's use count *)
    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)
    fun use x = (incUse x; x)

(*** OLD VERSION
    fun getEinApp x = (case V.getDef x
           of IR.EINAPP(e, arg) => SOME(e, arg)
            | _ => NONE
          (* end case *))
****)
  (* get the EIN application that "x" is bound to (if any).  Note that we are conservative
   * on following globals so as to avoid duplicating computation.
   *)
    fun getEinApp x = let
          fun getEinRHS (IR.EINAPP app) = SOME app
            | getEinRHS _ = NONE
          in
            case V.ty x
             of HighTypes.KernelTy => getEinRHS(V.getDef x)
              | HighTypes.FieldTy => getEinRHS(V.getDef x)
              | HighTypes.OFieldTy => getEinRHS(V.getDef x)
              | _ => getEinRHS(V.getLocalDef x)
            (* end case *)
          end

 (* doNormalize : EIN -> EIN
  * Orders EIN, normalizes it, then cleans the summation
  *)
    fun doNormalize e' = let
          val ordered = Reorder.transform e'
   (* val _ = print(String.concat["\n\npost do normalize:",EinPP.toString(e'),"--->"])*)

          val rtn = case NormalizeEin.transform ordered
             of NONE => ordered
              | SOME e => EinSums.clean e
            (* end case *)
(*val _ = print(String.concat["\n\n--> rtning: ",EinPP.toString(rtn),"\n\n\n"])*)
          in
            rtn
          end

(* FIXME: add documentation for this function's parameters and result *)
  (* Orders EIN, normalizes it, then cleans the summation orig-original EIN
   *
   *    changed         -- boolean that is true if any rewriting has been done
   *    params
   *    place
   *    newEinOp
   *    newArgs
   *    done
   *    arg		-- the argument that is bound to the application of newEinOp(newArgs)
   *    orig
   *    lhs
   *)
fun rewriteEin (changed, params, place, newEinOp, newArgs, done, arg, orig, lhs) = (
     ("\n\n*************************pre  app:"^EinPP.toString(newEinOp));
    case List.nth(params, place)
            of Ein.TEN(false, _) => (
                (changed, orig, place+1, done@[arg]))
	    | _ => (case Apply.apply (orig, place, newEinOp)
                    of SOME einOp => let
                        val _ = ("\n\n*************************post  app:"^EinPP.toString(einOp));
                         (* einOp is the result of the beta-reduction *)
                        in
                        (decUse arg; List.app incUse newArgs;
                        (true, einOp, place + length newArgs, done @ newArgs))
                        end 
                    | NONE => ( (* arg was unused in orig, so we can get rid of it *)
                        decUse arg;
                        (true, orig, place, done))
                    (* end case *))
          (* end case *))

(* FIXME: it would be much more efficient to do all of the substitutions in one go,
 * instead of repeatedly rewriting the term for each argument.
 *)
  (* doRHS: HighIR.var * rhs -> (var * rhs) list option
   * Looks at each argument to the original EINAPP.
   * If it is another EIN APP calls rewriteEin to do application
   * "place"-The Param-id for the EIN operator.
   * Keeps track of the place of the argument in substitution.
   *)
    fun doRHS (lhs, IR.EINAPP(ein, args)) = let
(*
val _ = print ("\n in doRHS central ein:"^EinPP.toString(ein))
    val _ = print(String.concat["highir type",HighTypes.toString(V.ty lhs)])
    val _ = print(String.concatWith",\t" (List.map (fn e1=> HighTypes.toString(V.ty e1)) args))
*)

            fun rewrite (false, _, _, [], _) = (NONE)
              | rewrite (true, einOp, _, [], args') =(
                SOME[(lhs, IR.EINAPP(doNormalize einOp, args'))])
             | rewrite (changed, einOp, place, x::xs, args') = (case getEinApp x
                 of NONE => (rewrite (changed, einOp, place+1, xs, args'@[x]))
                  | SOME(newE, newA) => let

                        val Ein.EIN{params, ...} = einOp
                       (* val _ =print ("\ninside ein"^EinPP.toString(einOp))*)
                      val (changed, einOp', place', done') =
                            rewriteEin (changed, params, place, newE, newA, args', x, einOp, lhs)
                      in
                        rewrite (changed, einOp', place', xs, done')
                      end
                (* end case *))
          in
            rewrite (false, ein, 0, args, [])
          end
      | doRHS _ = NONE

    structure Rewrite = RewriteFn (
      struct
        structure IR = IR
        val doAssign = doRHS

        fun doMAssign _ = NONE
        val elimUnusedVars = UnusedElim.reduce
      end)

    structure Promote = PromoteFn (IR)

    val rewrite = Rewrite.transform
    val promote = Promote.transform

  end
