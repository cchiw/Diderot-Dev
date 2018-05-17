structure FindCellFem : sig
 
    val inside : MidIR.var * int * HighIR.var list * MidIR.var list -> MidIR.assign list
    val getCell: MidIR.var * MidIR.var list -> MidIR.assign list
    val translate: Ein.ein_exp * MidIR.var * MidIR.var list ->  (MidIR.var * MidIR.rhs) list * meshElem.fnspace * int * int * 
             string list * MidIR.var * MidIR.var * MidIR.var * MidIR.var * 
             MidIR.var * MidIR.var * MidIR.var * MidIR.var * string list list
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
        
        
    (* translate position. used by evaluate and inside
    * body- the fld part of the ein expressio
    * vp- position variable
    * other mid-ir args
    *)
    fun translate(body, vp, args) =
        let
            val E.OField(ofield, E.Tensor(fid, _), E.Partial  dx) = body
            (*get components of fem field (element, mesh, datafile)*)        
			(* get name of data file, and define compoments of the fem field (mesh, element)*)
			 val (vfss, mesh, element, datafile) =
				(case ofield
					of E.DataFem dataid => let
						val (mesh, element)  = (ME.NoneMesh, ME.NoneE)
						val datafile = H.getRHSS (List.nth(args, dataid))
						in
							([], mesh, element, datafile)
						end
					| E.BuildFem (fnspaceid, path) => let
						(*fn space argument*)
						val vfnspace = List.nth(args, fnspaceid)
						val (mesh, element, d ) =  (H.getRHSMesh vfnspace ,  H.getRHSElement vfnspace, H.getRHSDegree vfnspace)
						val dd : int = case (Int.fromString(d))
							of SOME(i) => i
							| _ => raise Fail "Expected integer for dimension, but got something else"
						(* translate to fnspace to string*)
						val space  = String.concat[ME.toStringMesh mesh, "_",  ME.toStringElement element, "_",Int.toString(dd)]
						val pathtospace = H.getRHSS (List.nth(args,path))
						val datafile = String.concat[pathtospace,space,".json"]
						val _ = print(datafile)
						in
							([], mesh, element, datafile)
						end
				(* end case*))
			(*set field variables *)
			 val vf = List.nth(args, fid)
             val vfs =  vf::vfss            
			(* get data from field *)
			val _ = "\n\nbefore expand-fem getdata"
			(* creating new mid-ir variables*)
			val vB = V.new ("basis", basisTy)
			val vL = V.new ("numcell", Ty.intTy)
			val lC = V.new ("lastcell", Ty.optStruct)
			val mN = V.new ("celltonode",  mappTy(cellTy, nodeTy))
			val mP = V.new ("nodetopoint", mappTy(nodeTy, pointTy ))
			val mC = V.new ("nodetocoord", mappTy(nodeTy, coordTy))
			val vX = V.new ("coordinates", coordTy)
			(* creating mid-ir assignmment *)
			val pn = (lC, IR.OP(Op.GetTracker, vfs))
			val p0 = (vB, IR.OP(Op.BasisData, vfs))
			val p1 = (vL, IR.OP(Op.NumCells, vfs))
			val p3 = (mN, IR.OP(Op.CellToNode, vfs))
			val p4 = (mP, IR.OP(Op.NodeToPoint, vfs))
			val p5 = (mC, IR.OP(Op.NodeToCoord, vfs))
			val p6 = (vX, IR.OP(Op.Coordinates, vfs))
			val PgetData = [p0,pn,p1,p3,p4,p5,p6]
		(*expand to translate position and find cell operators*)
		(*expandFindCell(datafile, mesh, elem, vp, vL, mN, mP,lC) *) 
			val _ = "\n\nbefore expand-find cell"
			val (dim, (gdim,sdim, degree), avgPos, gBasisFunctions, gbasisJacobian, testString, sBasisFunctions, isAffineTF,sBasisDervs) =  CF.cvtFile datafile
			val vTC = V.new ("JIs", Ty.StringTy)
			val space = meshElem.Space(dim, ME.NoneMesh, element, degree)
			val p10 = (vTC, IR.OP(Op.makeTranslateCoordinates (space, dim, avgPos, gBasisFunctions, gbasisJacobian), []))
			val vfindcell = V.new ("fc", fcTy)
			val isAffine = if(isAffineTF) then 1 else 0
			val space = meshElem.Space(dim,mesh, element, degree)
			val p11 = (vfindcell, IR.OP(Op.makeFindCellExpand(space, testString, isAffine, dim, Int.fromLarge gdim), [vp, vL,mN,mP, vTC,lC]))
			val Pcell = [p10, p11]
        in
            (PgetData@Pcell, space, Int.fromLarge sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs)
        end

 	(* check ifposition is inside field *)
    fun inside(y, dim, [vpH,fH], [vp, f]) =
        let
        	val _ = H.getRHSEINSrc fH
            val (E.EIN{body,...}, args) = H.getRHSEIN f (*remy change here was f*) (*get EIN operator attached to variable*)
            val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = translate(body, vp, args)
            val p14 = (y, IR.OP(Op.checkCell, [vfindcell]))
        in
            Pall@[p14]
        end

    fun getCell(y, [f,vp]) =
        let
            val (E.EIN{body,...}, args) = H.getRHSEIN f (*get EIN operator attached to variable*)
            val (Pall, space, sdim, dim, sBasisFunctions, vL, mN, mP, vTC, vfindcell, mC, vX, vB,sBasisDervs) = translate(body, vp, args)
            val p14 = (y, IR.OP(Op.sp_getCell, [vfindcell]))
        in
                Pall@[p14]
         end
         
end
