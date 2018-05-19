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

	val ll = H.ll
	val paramToString = H.paramToString
	val iterP = H.iterP
	val iterA = H.iterA
   		
   		
   	 fun getSx (E.Sum(sx, e)) = (sx, e)
      | getSx e = ([], e)
    fun getPty (E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty)) = pty
    fun getE (E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty)) = e
    fun getDx (E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty)) = dx
    fun getPargs (E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty)) = pargs
    fun getIDs (E.Probe(E.OField(E.CFExp pargs, e, E.Partial dx), expProbe, pty))  = 
    	List.map (fn E.Tensor(tid,_) => tid) expProbe 
    (********************************** main *******************************)
    (* main function 
    * translate probe of cfexp to  poly terms 
    *)
    fun transform_Core (y, sx, ein as Ein.EIN{body,index, params} , args, SeqId) =
        let
        	val _ = ("\n\n*******************\n") 
            val _ = (H.line("core:",y, ein,args))  
            (*check to see that it is the right number of arguments*)
            val cfexp_ids = getPargs body 
            val n_pargs = length(cfexp_ids)
            val probe_ids =  getIDs(body)
            val n_probe = length(probe_ids)
            val _ = if(not(n_pargs =n_probe))
                    then raise  Fail(concat[" n_pargs:", Int.toString( n_pargs),"n_probe:",Int.toString(n_probe)])
                    else 1
            (* replace polywrap args/params with probed position(s) args/params *)
            val e = getE body            
            val (args, params, e) = P2.polyArgs(params, e, args,  SeqId, cfexp_ids, probe_ids)
            val _ = (H.toStringBA("\n\n  Step 2 replace arguments",e,args))
            (* need to flatten before merging polynomials in product *)
            val e = P3.rewriteMerge(e)
            val _ = H.toStringBA("\n\n  Step 3 merge poly term",e,args)
           (* normalize ein by cleaning it up and differntiating*)
            val dx = getDx body  
            val e = P3.rewriteDifferentiate(E.Apply(E.Partial dx, e))
            val _ =( H.toStringBA("\n\n  Step 4 differentiate ",e,args))
         in (args, params, e) end 
            
            
    (********************************** main *******************************)
    (* transform ein operator 
    * handles wrappers around cfexp ein expression, such as summation and reduction
    *)
    fun transform (y, ein, args) =
        let
            val _ = (H.line("\n\n   Step 0 Wrapper",y, ein,args))
            val Ein.EIN{body,index, params} = ein
            val (sx, body) =  getSx body
		    val  pty = getPty body
        	(* FIX ME variable is used in summation for sequences *)
        	val freshid = 101
        	val sx_seq = [(freshid, 0, 3)] (*Fixme-find length of sequence*)
            val SeqId = (case pty 
            		of SOME _ => SOME (E.V freshid )
            		| NONE =>  NONE 
            	(* end case *))
             val ein = Ein.EIN{body=body, index=index, params=params}
            val (args, params, body)  = transform_Core (y, sx, ein, args, SeqId) 
            (* Add summation wrapper back to ein expression *)
            val body = (case sx
            		of [] => body
                	|  _  => E.Sum(sx, body)
            	(* end case *))
            (* Add sequence wrapper back to ein expression *)           
            val body = (case pty
            		of SOME opn => E.OpR(opn, sx_seq, body) (*E.Sum(sx_seq,e)*)
            		| NONE =>  body
            	(* end case *))
            val ein = Ein.EIN{body=body, index=index, params=params}
            val _ = (H.line("\n\n   Step 5 ",y, ein,args))

            (********************************** done  *******************************)
            val newbie = (y, IR.EINAPP(ein, args))
            val stg_poly = Controls.get Ctl.stgPoly
            val _ =  if(stg_poly) then ScanE.readIR_Single(newbie,"tmp-poly") else 1
             val _ = (String.concat["\n\n*******************\n"])
        in
               [newbie]
        end


  end
