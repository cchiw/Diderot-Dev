(* expand-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* handle for evaluating fem
* and checking inside a fem field
*)
structure BuildFem : sig

    val sum_evaluate : MidIR.assign -> MidIR.assign list
    val scan_evaluate : MidIR.assign -> MidIR.assign list
    val inside : MidIR.var * int * HighIR.var list * MidIR.var list -> MidIR.assign list
    val getCell: MidIR.var * MidIR.var list -> MidIR.assign list
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
    structure EF = ExpandFem
    structure DF = DataFem

    fun getRHSEINSrc x = (case SrcIR.Var.getDef x
        of  SrcIR.EINAPP (ein, args) => ((ein, args);print"ein-app")
        | SrcIR.LIT l => print(concat["\n\nSrcLIT expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.LIT l)])
        | SrcIR.OP l => print(concat["\n\nsrcOP expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.OP l)])
        | SrcIR.CONS l => print(concat["\n\nsrccons expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.CONS l)])
        | SrcIR.GLOBAL l => print(concat["\n\nsrcglobal expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.GLOBAL l)])
        | rhs => print(concat["\n\nsrcexpected rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString rhs])
        (* end case *))


    fun getRHSEIN x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => (ein, args)
        | IR.LIT l => raise Fail(concat["\n\nLIT expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.LIT l)])
        | IR.OP l => raise Fail(concat["\n\nOP expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.OP l)])
        | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.CONS l)])
        | IR.GLOBAL l => raise Fail(concat["\n\nglobal expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.GLOBAL l)])
        | rhs => raise Fail(concat["\n\nexpected rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString rhs])
        (* end case *))


    (* tanslate position. used by evaluate and inside
    * body- the fld part of the ein expressio
    * vp- position variable
    * other mid-ir args
    *)
    fun translate(body, vp, args) =
        let
            val _ = "\n\nbefore BF translate"
            val E.OField(ofield, E.Tensor(fid, _), dx) = body

            (*get components of fem field (element, mesh, datafile)*)
            val (vfs, mesh, element, datafile) = DF.defineField(ofield, args)

            (* get arguments from ids*)
            val vf = List.nth(args, fid)

            val vfs =  vf::vfs

            val (vX, PgetData, vB, vL, mN, mP, mC,Lc) = EF.getData(vfs)

            val (vfindcell, Pcell, dim, sdim, sBasisFunctions, space, vTC,sBasisDervs) = EF.expandFindCell(datafile, mesh, element, vp, vL, mN, mP,Lc)

        in
            (PgetData@Pcell, space, Int.fromLarge sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs)
        end



    (* get name of data file*)
    fun evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,...}, args)) =
    let
        (*deconstruct body *)
        val E.Probe(fld, E.Tensor(pid,_)) = body
        val E.Probe(E.OField(ofield, E.Tensor(fid, _), dx) , E.Tensor(pid,_)) = body
        val vp = List.nth(args, pid)

        val body = fld
	val level = List.length(dx)
	val shape = List.rev(List.drop(List.rev(index),level))
        val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = translate(body, vp, args)

        val nn = length(sBasisDervs)
        (*val _ = (String.concat["\nsBasisDervs: ",Int.toString(nn),"level:",Int.toString(level)])*)

        val _ = if(level>nn) then raise Fail ("unsupported level of diff") else 1
        val Peval =
	    EF.eval(level, shape,y, index, space, sdim, dim, sBasisFunctions, vp, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs)
        in
            Pall@Peval
        end




    fun sum_evaluate (y, rhs as IR.EINAPP(ein as E.EIN{body,index,params}, args)) =
        let
            val E.Sum(sx, E.Probe(E.OField(ofield,E.Tensor(fid, alpha), dx),pos)) = body
            fun cvtTerm (alpha, dx) = E.Probe(E.OField(ofield,E.Tensor(fid, alpha), dx), pos)
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


        val E.Probe(E.OField(ofield, E.Tensor(fid, alpha), dx), pos) = body
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
            body=E.Probe(E.OField(ofield,E.Tensor(fid, fbeta), dxbeta), pos),
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
            val E.Probe(E.OField(ofield, E.Tensor(fid, alpha), dx), pos) = body
        in  (case (List.find (fn E.C _ => true |  _ => false)  (alpha@dx))
            of NONE     =>  evaluate (y, rhs)
            | SOME _    =>  slice_evaluate (y, rhs)
            (* end case*))
        end


    (* check ifposition is inside field *)
    fun inside(y, dim, [vpH,fH], [vp, f]) =
        let
             val _ = getRHSEINSrc fH

            val (E.EIN{body,...}, args) = getRHSEIN f (*remy change here was f*) (*get EIN operator attached to variable*)
            val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = translate(body, vp, args)
            val p14 = (y, IR.OP(Op.checkCell, [vfindcell]))
        in
            Pall@[p14]
        end

    fun getCell(y, [f,vp]) =
        let
            val _ = "\n Before BF get cell"
            val (E.EIN{body,...}, args) = getRHSEIN f (*get EIN operator attached to variable*)
            val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = translate(body, vp, args)
            val p14 = (y, IR.OP(Op.sp_getCell, [vfindcell]))
            in
                Pall@[p14]
            end

end
