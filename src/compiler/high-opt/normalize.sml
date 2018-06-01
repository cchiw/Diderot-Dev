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

  (* get the EIN application that "x" is bound to (if any).  Note that we are conservative
   * on following globals so as to avoid duplicating computation.
   *)
    fun getEinApp x = let
			fun getEinRHS (IR.EINAPP app) = ( "EINAPP";SOME app)
			| getEinRHS (IR.GLOBAL xy) = 
				("Global"(*;getEinApp (V.getLocalDef xy)*);NONE)
				(*Fxime global variable should be replaced for field-func*)
			| getEinRHS (IR.STATE _) = ("state";NONE)
			| getEinRHS (IR.VAR _) = ("var";NONE)
			| getEinRHS (IR.LIT _) = ("lit";NONE)
			| getEinRHS (IR.OP _) = ("op";NONE)
			| getEinRHS (IR.CONS _) = ("cons";NONE)
			| getEinRHS (IR.SEQ _) = ("seq";NONE)
			| getEinRHS (IR.APPLY _) = ("apply";NONE)
			| getEinRHS (IR.MAPREDUCE _) = ("mapreduce";NONE)
           (* | getEinRHS _ = NONE*)
           val t = V.ty x
           val _ = (HighTypes.toString(t))
          in
            case t
             of HighTypes.KernelTy => getEinRHS(V.getDef x)
              | HighTypes.FieldTy => getEinRHS(V.getDef x)
              | HighTypes.OFieldTy(_)=> getEinRHS(V.getDef x)
              | _ => (getEinRHS(V.getLocalDef x))
            (* end case *)
          end

fun useCount (HighIR.V{useCnt, ...}) = !useCnt
fun ll ([],cnt) = ""
| ll (a1::args,cnt) = String.concat[" ", Int.toString(cnt),"_", HighTypes.toString(HighIR.Var.ty a1), " ", HighIR.Var.name(a1),",", ll(args,cnt+1)]


 (* doNormalize : EIN -> EIN
  * Orders EIN, normalizes it, then cleans the summation
  *)
(*mkfield cfexp  needs args *)
fun doNormalize (e',args:HighIR.var list) = let
          val ordered = Reorder.transform e'
   (* val _ = (String.concat["\n\npost do normalize:",EinPP.toString(e'),"--->"])*)

          val rtn = case NormalizeEin.transform(ordered,args)
             of NONE => ordered
              | SOME e => EinSums.clean e
            (* end case *)
(*val _ = (String.concat["\n\n--> rtning: ",EinPP.toString(rtn),"\n\n\n"])*)
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
    case List.nth(params, place)
            of Ein.TEN(false, _) => (
                (changed, orig, place+1, done@[arg]))
	    | _ => (case Apply.apply (orig, place, newEinOp,newArgs,done)
                    of SOME einOp => let
                        val _ = (String.concat["\n\nAfter Application:",EinPP.toString(einOp), ":",ll(done @ newArgs,0)])
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

val _ = (String.concat["\n\n****************************************************\ndoRhs:",EinPP.toString(ein), ":",ll(args,0)])

            fun rewrite (false, _, _, [], _) = (NONE)
              | rewrite (true, einOp, _, [], args') =(
                SOME[(lhs, IR.EINAPP(doNormalize (einOp,args'), args'))])
              | rewrite (changed, einOp, place, x::xs, args') = let
val _  = (String.concat["\n\tPlace:",Int.toString(place)])
                in case getEinApp x
                 of NONE => (rewrite (changed, einOp, place+1, xs, args'@[x]))
                  | SOME(newE, newA) => let
                        val Ein.EIN{params, ...} = einOp
val _ = (String.concat["\n\n----------------------------------------\n\nBefore rewriting:",EinPP.toString(einOp), ":",ll(args'@(x::xs),0)])
                      val (changed, einOp', place', done') =
                            rewriteEin (changed, params, place, newE, newA, args', x, einOp, lhs)
val _ =(String.concat["\n\nDone rewriting:",EinPP.toString(einOp'), ":",ll(done'@xs,0)])
                      in
                        rewrite (changed, einOp', place', xs, done')
                      end
                (* end case *)
                end
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
