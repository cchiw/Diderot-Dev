structure EvalFem : sig

    val sum_evaluate : MidIR.assign -> MidIR.assign list
    val scan_evaluate : MidIR.assign -> MidIR.assign list
    
  end = struct

    structure SrcIR = HighIR
    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure GVar = IR.GlobalVar
    structure E = Ein
    structure CF = cvtFile
    structure ME = meshElem
    structure FC = FindCellFem
    structure H = Helper
    
    
 	(* types *)
    val cellTy = Ty.intTy
    val pointTy = Ty.TensorTy []
    (* types created *)
    val basisTy = Ty.BasisDataTy
    val coordTy = Ty.coordTy
    val fcTy = Ty.fcTy                      (*int and vector*)
    (*Type wrapper*)
    val mappTy = Ty.MappTy
    val nodeTy = Ty.arrTy(Ty.intTy)            (*array of ints*)
    
 	(* at this point we have the correct cell number
    * and we are evaluating field with translated position
    *)
    fun mk_eval_ops(level,shape, y, index, space, sdim, dim, sBasisFunctions, vp, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) =
        let
			val newposTy = Ty.TensorTy[dim]
			(* creating new mid-ir variables*)
			val basisEval = V.new ("makeBasisEvaluation", Ty.StringTy)
			val cell = V.new ("cell", cellTy)
			val newpos = V.new ("newpos", newposTy)
			val node = V.new ("node", nodeTy)
			val tmp = if level = 0 then [""] else List.nth(sBasisDervs,level-1)
			val args = [vp, vL,mN,mP, vTC, vfindcell]
			val p12 =  (basisEval, IR.OP(Op.makeBasisEvaluation(space, sBasisFunctions,level, tmp), args))
			val p14 = (cell, IR.OP(Op.GetCell, [vfindcell,basisEval])) 
			val p15 = (newpos, IR.OP(Op.GetPos, [vfindcell,basisEval]))
			val p16 = (node, IR.OP(Op.GetNode, [cell, mN, mC]))
			val args = if level = 0 then [] else  [cell, mN, mP]
			val p19 = (y,IR.OP(Op.EvalFem(space, sdim,level,shape),[node,newpos,vX]@args))
			val Peval =  [p12,p14,p15,p16,p19]
        in
            Peval
        end

	 (* get name of data file*)
    fun evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,...}, args)) =
    	let
			(*deconstruct body *)
			val E.Probe(fld, [E.Tensor(pid,_)],_) = body
			val E.Probe(E.OField(ofield, E.Tensor(fid, _), E.Partial  dx), [E.Tensor(pid,_)], _) = body
			val vp = List.nth(args, pid)
			val body = fld
			val level = List.length(dx)
			val shape = List.rev(List.drop(List.rev(index),level))
			val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = FC.translate(body, vp, args)
			val nn = length(sBasisDervs)
			(*val _ = (String.concat["\nsBasisDervs: ",Int.toString(nn),"level:",Int.toString(level)])*)
			val _ = if(level>nn) then raise Fail ("unsupported level of diff") else 1
			val Peval =  mk_eval_ops(level, shape,y, index, space, sdim, dim, sBasisFunctions, vp, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs)
        in
            Pall@Peval
        end

    fun sum_evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,params}, args)) =
        let
            val E.Sum(sx, E.Probe(E.OField(ofield,E.Tensor(fid, alpha), E.Partial  dx), [pos], _)) = body
            fun cvtTerm (alpha, dx) = E.Probe(E.OField(ofield, E.Tensor(fid, alpha), E.Partial  dx), [pos], NONE)
            val E.FLD(dim, shape) = List.nth(params, fid)
            val (sid, lb, ub) = (case sx
                of [e1] => e1
                | _ => raise Fail "unhandled summation"
                (*end case*))
			val tshape = alpha@dx
			(*field term*)
			val nalpha = length(alpha)
			val ndx = length(dx)
			(*creating fresh indices for shape and derivative*)
			val fbeta  = List.tabulate(nalpha, fn i => E.V i)
			val dxbeta = List.tabulate(ndx, fn i    => E.V(nalpha+i))
			val exp = cvtTerm (fbeta, dxbeta)
			(*getting size from shape and derivative*)
			val ssize = ub+1
			(*getSize in alpha*)
			fun sortF ([],_) = []
			  | sortF((E.V v)::es,n) =
				if (sid=v) then ssize::sortF(es,n+1)
				else List.nth(index, v)::sortF(es,n+1)
			  | sortF((E.C _)::es,n) = List.nth(shape, n)::sortF(es,n+1)
			(*getSize in dx*)
			fun sortDx ([],_) = []
			  | sortDx((E.V v)::es,n) =
				if (sid=v) then ssize::sortDx(es,n+1)
				else dim::sortDx(es,n+1)
			  | sortDx((E.C _)::es,n) = dim::sortDx(es,n+1)
			val fsize = sortF(alpha, 0)
			val dxsize =sortF(dx, 0)
			val size = fsize@dxsize
			(*val _ = List.map (fn n => ("-"^Int.toString(n))) size*)
			(*evaluate ofields*)
			val vT = V.new ("center", Ty.TensorTy size)
			val ein = E.EIN{body= exp, index=size, params=params}
			val asgn = evaluate(vT, IR.EINAPP(ein, args))
			(*take summation*)
			val exp = E.Sum(sx, E.Tensor(0, tshape))
			val ein = E.EIN{body= exp, index=index, params=params}
			val asgnn = (y, IR.EINAPP(ein, [vT]))
    	in
             asgn @[asgnn]
   	 	end

    fun slice_evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,params}, args)) =
        let
			val E.Probe(E.OField(ofield, E.Tensor(fid, alpha), E.Partial dx), [pos],_) = body
			val E.FLD(dim, shape) = List.nth(params, fid)
			(*field term*)
			val nalpha = length(alpha)
			val ndx = length(dx)
			(*creating fresh indices for shape and derivative*)
			val fbeta  = List.tabulate(nalpha, fn i => E.V i)
			val dxbeta = List.tabulate(ndx, fn i    => E.V(nalpha+i))
			(*getting size from shape and derivative*)
			val fsize  = List.tabulate(nalpha, fn i => List.nth(shape, i))
			val dxsize = List.tabulate(ndx, fn i    => dim)
			val size = fsize@dxsize
			(*evaluate ofields*)
			val einF = E.EIN{
				body=E.Probe(E.OField(ofield,E.Tensor(fid, fbeta), E.Partial  dxbeta), [pos], NONE),
				index=size,
				params=params
				}
			val vT= V.new ("femfld", Ty.TensorTy size)
			val asgn = evaluate(vT, IR.EINAPP(einF, args))
			(*take slice*)
			val tshape = alpha@dx
			(*size of tensor replacement*)
			fun skipC [] = []
			| skipC ((E.V v0)::es) = List.nth(index, v0)::skipC(es)
			| skipC (e1::es) = skipC es
			(*tensor replacement *)
			val einT = E.EIN{
				body=  E.Tensor(0, tshape),
				index=skipC tshape,
				params=params
				}
			val asgnn = (y, IR.EINAPP(einT, [vT]))
        in
            asgn @[asgnn]
        end
        
    fun scan_evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,params}, args)) =
        let
            val E.Probe(E.OField(ofield, E.Tensor(fid, alpha), E.Partial  dx), [pos],_) = body
        in  
        	(case (List.find (fn E.C _ => true |  _ => false)  (alpha@dx))
            	of NONE     =>  evaluate (y, rhs)
            	| SOME _    =>  slice_evaluate (y, rhs)
            (* end case*))
        end
(*
    fn Teo(lhs, IR.EINAPP(ein as E.EIN{body,index,params}, args)) =
        let
                    
            val E.Probe(E.OField(ofield, E.Tensor(fid, alpha), px), pos, _) = body
            (* E.DataFem(datafileid, _) < parameter id for data file
             * E.Tensor(fid, alpha) < body  where fid is the field parameter
             * px < differentiation variable
             * pos < list of position expressions
             *)
           
            (* assumes current argument order *)
            val [vF, vP, vI, vK] = args
            (* in the future use parameter ids to get arguments
             like this...
             val vF = List.nth(args, fid)
             NTS put parameters for vI and vK in EIN expression
             
            *)
            val vR= V.new ("probe sq", Ty.TensorTy size)
            val s1 = (vR, IR.OP(Op.probeseq, [K, i])) (* n.t.s  look up probe seq *)
            
        
            (* parameter ids for cfexp tensors *)
            val qid = 0   (* f*)
            val tid = 1   (* t *)
            val posid = 2  (* pos *) 
            val rid = 3  (* R *)
            (* create two dummy variables for cfexp field arguments vQ and vT*)
            val vQ= V.new ("psdfdsfobe sq", Ty.TensorTy size)
            val vT= V.new ("probe sq", Ty.TensorTy size)
            val s2 = (vQ, IR.Lit(IR.Literal(0))
            val s3 = (vT, IR.Lit(IR.Literal(0))
            
            
            (*note order of variables should match ids *)
            val args = [vQ, vT, vP, vR]
            
            (* expression that represents body of cfexp *)
            (* note: inside EIN expression use qid as parameter id for f, and tid as parameter id for t *)  
            val exp_body = fld-to-exp (ofield, qid, tid) 
            val exp_in = [(qid, E.F), (tid, E.T)])  (*represent  *)
            val exp_pos = [E.Tensor(posid, []), E.Tensor(rid, [])] (*represent probed positions *)
         
            val ein_app = E.Probe(E.OField(E.CFEXP(exp_in, exp_body, px), exp_pos, NONE)
            val s = (lhs, IR.EINAPP(ein_app, args))
            in [s1, s2,....] end
            
    *)     
            
            
            

end

