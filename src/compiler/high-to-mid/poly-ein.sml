(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin : sig

    val transform : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assign list

  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet
    structure SrcIR = HighIR
    structure DstIR = MidIR
    structure ScanE = ScanEin
    structure H = Helper
    structure P2 = PolyEin2
    structure P3 = PolyEin3


	val paramToString = H.paramToString
	val iterP = H.iterP
	val iterA = H.iterA	
   	fun getSx (E.Sum(sx, e)) = (sx, e)
      | getSx e = ([], e)

    (********************************** main *******************************)
    (* main function 
    * translate probe of cfexp to  poly terms 
    *)
    fun transform_Core (y, sx, ein as Ein.EIN{body, index, params}, args_orig, SeqId) = 
        let
            val _ = (concat["\n\n\n   transform core:",EinPP.toString(ein)])
        	val _ = ("\n\n*******************\n") 
            val _ = (H.einToString("core:", y, ein, args_orig)) 
             
            (*check to see that it is the right number of arguments*)
            val E.Probe(E.OField(E.CFExp cfexp_ids, exp_fn, E.Partial dx), expProbe, pty) = body
            val probe_ids = List.map (fn E.Tensor(tid, _) => tid) expProbe 
            val n_pargs = length(cfexp_ids)
            val n_probe = length(probe_ids)
            val _ = if(not(n_pargs = n_probe))
                    then raise  Fail(concat[" n_pargs:", Int.toString( n_pargs), "n_probe:", Int.toString(n_probe)])
                    else 1
            (* replace polywrap args/params with probed position(s) args/params *)  
            (* check RHS of all parameters used in function body *)
            val _ =  checkFieldFunc.checkParams(cfexp_ids, exp_fn, args_orig)
            (* Note: Add a command line flag to disable checking:
            * Helps when conditional or whatever doesn't use args and this is a mistake
            *)
            
            
             val _ = (H.bodyToString("\n\n  starting", exp_fn, args_orig))         
            val (args, params, e, rtn) = P2.polyArgs(params, exp_fn, args_orig,  SeqId, cfexp_ids, probe_ids)
            val _ = (H.bodyToString("\n\n  Step 2 replace arguments", e, args))
            (* need to flatten before merging polynomials in product *)
            val e = P3.rewriteMerge(e)
            val _ = (H.bodyToString("\n\n  Step 3 merge poly term", e, args))
           (* normalize ein by cleaning it up and differntiating*)
           
            val (e, params, args) = CleanParams.clean_Params (e, params, args)
            val _ = (H.bodyToString("\n\n  After cleaning up arguments", e, args)) 
            val e = P3.applyDx(E.Apply(E.Partial(dx), e))
            val _ = (H.bodyToString("\n\n  Step 4 differentiate ", e, args))
         in (args, params, e, rtn) end 
            
            
    (********************************** main *******************************)
    (* transform ein operator 
    * handles wrappers around cfexp ein expression, such as summation and reduction
    *)
    fun transform (y, ein, args) = 
        let
            val _ = (H.einToString("\n\n   Step 0 Wrapper", y, ein, args))
            val Ein.EIN{body, index, params} = ein
            val (sx, body) =  getSx body
		    val E.Probe(E.OField(E.CFExp cfexp_ids, exp_fn, E.Partial dx), expProbe, pty) = body
        	(* FIX ME variable is used in summation for sequences *)
        	val freshid = 101        	
            val (SeqId, sx_seq) = (case pty 
            		of SOME (_, n) => (SOME (E.V freshid ), [(freshid, 0, n-1)])
            		| NONE =>  (NONE, [])
            	(* end case *))
            val ein = Ein.EIN{body = body, index = index, params = params}
            val (args, params, body, rtn)  = transform_Core (y, sx, ein, args, SeqId) 
            (* Add summation wrapper back to ein expression *)
            val body = (case sx
            		of [] => body
                	|  _  => E.Sum(sx, body)
            	(* end case *))
            (* Add sequence wrapper back to ein expression *)        
            val body = (case pty
            		of SOME (opn, _) => E.OpR(opn, sx_seq, body) (*E.Sum(sx_seq, e)*)
            		| NONE =>  body
            	(* end case *))
            val ein = Ein.EIN{body = body, index = index, params = params}
            val _ = (H.einToString("\n\n   Step 5 ", y, ein, args))

            (********************************** done  *******************************)
            val newbie = (y, IR.EINAPP(ein, args))
            val stg_poly = Controls.get Ctl.stgPoly
            val _ =  if(stg_poly) then ScanE.readIR_Single(newbie, "tmp-poly") else 1
             val _ = (String.concat["\n\n*******************\n"])
        in
               rtn@[newbie]
        end


  end
