(* push-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)


(*Push fem operator*)
structure FemToMid =

    struct

    structure IR = MidIR
    structure Op = MidOps
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure DE = DerivativeEin
    structure GVar = IR.GlobalVar
    structure CF = cvtFile
    structure ME = meshElem

    fun getRHSOp x = (case IR.Var.getDef x
        of IR.LIT(Literal.String s) => s
        | rhs => raise Fail(concat[
            "expected rhs operator for ", IR.Var.toString x,
            " but found ", IR.RHS.toString rhs
            ])
        (* end case *))

    (* types *)
    val cellTy = Ty.intTy
    val pointTy = Ty.TensorTy []
    (* types created *)
    val basisTy = Ty.BasisDataTy
    val polyTy = Ty.polyTy
    val coordTy = Ty.coordTy
    val JITy = Ty.JITy
    val fcTy = Ty.fcTy                      (*int and vector*)
    val jacobiansTy = Ty.jacobiansTy
    val transformsTy = Ty.transformsTy
    (*Type wrapper*)
    val mappTy = Ty.MappTy
    val arrTy = Ty.arrTy
    val nodeTy = arrTy(Ty.intTy)            (*array of ints*)
    val brevTy = arrTy(polyTy)              (*arry of polys *)


    (* get data from field *)
    fun getData vfs = 
    	let
			val _ = "\n\nbefore expand-fem getdata"
			(* creating new mid-ir variables*)
			val vB = V.new ("basis", basisTy)
			val vL = V.new ("numcell", Ty.intTy) (*Why the symbol?*)
			val lC = V.new ("lastcell", Ty.optStruct)
			val mN = V.new ("celltonode",  mappTy(cellTy, nodeTy))
			val mP = V.new ("nodetopoint", mappTy(nodeTy, pointTy ))
			val mC = V.new ("nodetocoord", mappTy(nodeTy, coordTy))
			val vX = V.new ("coordinates", coordTy)
			(* creating mid-ir assignmment *)
			val pn = (lC, IR.OP(Op.GetTracker, vfs))
			val p0 = (vB, IR.OP(Op.BasisData, vfs))
			val p1 = (vL, IR.OP(Op.NumCells,  vfs))
			val p3 = (mN, IR.OP(Op.CellToNode, vfs))
			val p4 = (mP, IR.OP(Op.NodeToPoint, vfs))
			val p5 = (mC, IR.OP(Op.NodeToCoord, vfs))
			val p6 = (vX, IR.OP(Op.Coordinates, vfs))
			val PgetData = [p0,pn,p1,p3,p4,p5,p6]
        in (vX, PgetData, vB, vL, mN, mP, mC,lC) end


    fun expandFindCell(datafile, mesh, elem, vp, vL, mN, mP,lC) = 
    	let
			val _ = "\n\nbefore expand-find cell"
			val (dim, (gdim,sdim, degree), avgPos, gBasisFunctions, gbasisJacobian, testString, sBasisFunctions, isAffineTF,sBasisDervs) =  CF.cvtFile datafile
			(*note that nnodes is unused*)
			(*note evalBasisFunctions depends on V, but others depend on mesh/dim*)
			(*for each op add dependencies so a unique name is made *)
			(*make all the code before find cell*)
			val vTC = V.new ("JIs", Ty.StringTy)
			val space = meshElem.Space(dim, ME.NoneMesh, elem, degree)
			val p10 = (vTC, IR.OP(Op.makeTranslateCoordinates (space, dim, avgPos, gBasisFunctions, gbasisJacobian), []))
			val vfindcell = V.new ("fc", fcTy)
			val isAffine = if(isAffineTF) then 1 else 0
			val space = meshElem.Space(dim,mesh, elem, degree)
			val p11 = (vfindcell, IR.OP(Op.makeFindCellExpand(space,  testString, isAffine,dim,Int.fromLarge gdim), [vp, vL,mN,mP, vTC,lC]))
			val Pcell = [p10, p11]
		in
			(vfindcell, Pcell, dim, sdim, sBasisFunctions, space, vTC,sBasisDervs)
		end
		

    (*post find cell*)
    fun eval(level,shape, y, index, space, sdim, dim, sBasisFunctions, vp, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) =
        let
			val newposTy = Ty.TensorTy[dim]
			(* creating new mid-ir variables*)
			val basisEval = V.new ("makeBasisEvaluation", Ty.StringTy)
			val cell = V.new ("cell", cellTy)
			val newpos = V.new ("newpos", newposTy)
			val node = V.new ("node", nodeTy)
			(* val coordsrelevant = V.new ("coordsrel", crevTy) *)
			(* val basisrelevant = V.new ("basisrel", brevTy) *)
			val p12 = if level = 0
				then (basisEval, IR.OP(Op.makeBasisEvaluation(space, sBasisFunctions,level,[""]), [vp, vL,mN,mP, vTC, vfindcell]))
				else (basisEval, IR.OP(Op.makeBasisEvaluation(space, sBasisFunctions,level,List.nth(sBasisDervs,level-1)), [vp, vL,mN,mP, vTC, vfindcell]))
			val p14 = (cell, IR.OP(Op.GetCell, [vfindcell,basisEval])) (*not temp here*)
			val p15 = (newpos, IR.OP(Op.GetPos, [vfindcell,basisEval]))
			val p16 = (node, IR.OP(Op.GetNode, [cell, mN, mC]))
			(* val p17 = (basisrelevant, IR.OP(Op.ProbeNodeC, [node, vX])) *)
			(* val p18 = (coordsrelevant, IR.OP(Op.ProbeNodeB, [node, vB])) *)
			val p19 = if level = 0
				then (y , IR.OP(Op.EvalFem(space, sdim,level,shape),[node,newpos,vX]))
				else (y , IR.OP(Op.EvalFem(space, sdim,level,shape),[node,newpos,vX,cell, mN, mP]))
			val Peval =  [p12,p14,p15,p16,p19]

        in
            Peval
        end

end
