(* tree-ops.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Operators for Tree IR.
 *
 * Note: this file is generated from gen/ir/tree-ops.spec and gen/ir/tree-ops.in.
 *)

structure TreeOps =
  struct

  (* required helper functions for types *)
    type ty = TreeTypes.t
    val samety = TreeTypes.same
    val hashty = TreeTypes.hash
    val tyToString = TreeTypes.toString

    type mesh = meshElem.mesh
    val samemesh = meshElem.sameMesh
    val hashmesh = meshElem.hashMesh
    val meshToString = meshElem.toStringMesh

    type element = meshElem.element
    val sameelement = meshElem.sameElement
    val hashelement = meshElem.hashElement
    val elementToString = meshElem.toStringElement

    type fnspace = meshElem.fnspace
    val samefnspace = meshElem.samefnspace
    val hashfnspace= meshElem.hashfnspace
    val fnspaceToString = meshElem.toStringfnspace



  (* required helper functions for type lists *)
    type tys = ty list
    fun sametys (tys1, tys2) = ListPair.allEq samety (tys1, tys2)
    fun hashtys tys = List.foldl (fn (ty, s) => hashty ty + 0w3 * s) 0w0 tys
    fun tysToString tys = String.concat["[", String.concatWithMap "," tyToString tys, "]" ]

  (* required helper functions for the int type *)
    fun sameint (i1 : int, i2) = (i1 = i2)
    fun hashint i = Word.fromInt i
    fun intToString i = Int.toString i

  (* required helper functions for the string type *)
    fun samestring (s1 : string, s2) = (s1 = s2)
    val hashstring = HashString.hashString
    fun stringToString s = String.concat["\"", s, "\""]

  (* required helper functions for the shape type *)
    type shape = int list
    val sameshape : (shape * shape -> bool) = (op =)
    fun hashshape shp =
          List.foldl (fn (i, w) => Word.xorb(Word.<<(w, 0w1), Word.fromInt i))
            (Word.fromInt(List.length shp)) shp
    fun shapeToString shp = String.concat["[", String.concatWithMap "," Int.toString shp, "]"]

  (* required helper functions for the mask type *)
    type mask = bool list
    val samemask : (mask * mask -> bool) = (op =)
    fun hashmask m =
          List.foldl (fn (false, w) => w+w | (true, w) => w+w+0w1)
            (Word.fromInt(List.length m)) m
    fun maskToString m =
          String.concat(List.map (fn true => "_" | false => ":") m)

  (* required helper functions for the index control type type *)
    type idxctl = IndexCtl.t
    val sameidxctl = IndexCtl.same
    val hashidxctl = IndexCtl.hash
    val idxctlToString = IndexCtl.toString


    type intList = int list
    fun sameintList(tys1, tys2) = ListPair.allEq sameint (tys1, tys2)
    fun hashintList tys =  List.foldl (fn (ty, s) => hashint ty + 0w3 * s) 0w0 tys
    fun intListToString  e = "int list"


    type realList = real list
    fun samereal(e1:real,e2:real)=  (abs(e1-e2)<0.001)
    fun samerealList(tys1, tys2) = ListPair.allEq samereal (tys1, tys2)
    fun hashrealList tys =  List.foldl (fn (ty, s) =>   0w3 * s) 0w0 tys
    fun realListToString  e = "real list"

    type stringList = string list
    fun samestringList(tys1, tys2) = ListPair.allEq samestring (tys1, tys2)
    fun hashstringList tys =  List.foldl (fn (ty, s) => hashstring ty + 0w3 * s) 0w0 tys
    fun stringListToString e = "string list"

    type stringLists = string list list
    fun samestringLists(tys1, tys2) = ListPair.allEq samestringList (tys1, tys2)
    fun hashstringLists tys = List.foldl (fn (ty, s) => hashstringList ty + 0w3 * s) 0w0 tys
    fun stringListsToString e = "string list list "

    datatype rator
      = IAdd
      | ISub
      | IMul
      | IDiv
      | IMod
      | INeg
      | RAdd
      | RSub
      | RMul
      | RDiv
      | RNeg
      | RClamp
      | RLerp
      | RCeiling
      | RFloor
      | RRound
      | RTrunc
      | RealToInt
      | LT of ty
      | LTE of ty
      | EQ of ty
      | NEQ of ty
      | GT of ty
      | GTE of ty
      | BAnd
      | BOr
      | BNot
      | Abs of ty
      | Max of ty
      | Min of ty
      | VAdd of int * int
      | VSub of int * int
      | VScale of int * int
      | VMul of int * int
      | VNeg of int * int
      | VSum of int * int
      | VDot of int * int
      | VIndex of int * int * int
      | VClamp of int * int
      | VMapClamp of int * int
      | VLerp of int * int
      | VCeiling of int * int
      | VFloor of int * int
      | VRound of int * int
      | VTrunc of int * int
      | VToInt of int * int
      | TensorIndex of ty * shape
      | ProjectLast of ty * shape
      | TensorCopy of shape
      | TensorRef of shape
      | EigenVecs2x2
      | EigenVecs3x3
      | EigenVals2x2
      | EigenVals3x3
      | Select of ty * int
      | Subscript of ty
      | MkDynamic of ty * int
      | Append of ty * ty
      | Prepend of ty * ty
      | Concat of ty
      | Range
      | Length of ty
      | SphereQuery of int * ty
      | Sqrt
      | Cos
      | ArcCos
      | Sin
      | ArcSin
      | Tan
      | ArcTan
      | Exp
      | Sgn
      | IntToReal
      | NumStrands of StrandSets.t
      | Transform of ImageInfo.t
      | Translate of ImageInfo.t
      | BaseAddress of ImageInfo.t
      | ControlIndex of ImageInfo.t * idxctl * int
      | LoadVoxel of ImageInfo.t
      | Inside of VectorLayout.t * ImageInfo.t * int
      | IndexInside of ImageInfo.t * int
      | ImageDim of ImageInfo.t * int
      | MathFn of MathFns.t
      | Dimension
      | NumCells
      | GetTracker
      | BasisData
      | CellToNode
      | NodeToPoint
      | NodeToCoord
      | Coordinates
      | makeTranslateCoordinates of fnspace * int * realList * stringList * stringLists
      | makeFindCellExpand of fnspace * string * int * int * int
      | makeBasisEvaluation of fnspace * stringList * int * stringList
      | makeFindCellPush of int * string
      | GetCell
      | GetPos
      | GetNode
      | ProbeNodeC
      | ProbeNodeB
      | ProbeF of fnspace * int * int * int
      | ProbeInvF of fnspace * int * int * int
      | ProbePhi of fnspace * int * int * int
      | EvalFem of fnspace * int * int * intList
      | checkCell
      | IfWrap
      | sp_getCell
      | swap2
      | swap3
      | swap4
      | swap5
      | swap6

    fun resultArity IAdd = 1
      | resultArity ISub = 1
      | resultArity IMul = 1
      | resultArity IDiv = 1
      | resultArity IMod = 1
      | resultArity INeg = 1
      | resultArity RAdd = 1
      | resultArity RSub = 1
      | resultArity RMul = 1
      | resultArity RDiv = 1
      | resultArity RNeg = 1
      | resultArity RClamp = 1
      | resultArity RLerp = 1
      | resultArity RCeiling = 1
      | resultArity RFloor = 1
      | resultArity RRound = 1
      | resultArity RTrunc = 1
      | resultArity RealToInt = 1
      | resultArity (LT _) = 1
      | resultArity (LTE _) = 1
      | resultArity (EQ _) = 1
      | resultArity (NEQ _) = 1
      | resultArity (GT _) = 1
      | resultArity (GTE _) = 1
      | resultArity BAnd = 1
      | resultArity BOr = 1
      | resultArity BNot = 1
      | resultArity (Abs _) = 1
      | resultArity (Max _) = 1
      | resultArity (Min _) = 1
      | resultArity (VAdd _) = 1
      | resultArity (VSub _) = 1
      | resultArity (VScale _) = 1
      | resultArity (VMul _) = 1
      | resultArity (VNeg _) = 1
      | resultArity (VSum _) = 1
      | resultArity (VDot _) = 1
      | resultArity (VIndex _) = 1
      | resultArity (VClamp _) = 1
      | resultArity (VMapClamp _) = 1
      | resultArity (VLerp _) = 1
      | resultArity (VCeiling _) = 1
      | resultArity (VFloor _) = 1
      | resultArity (VRound _) = 1
      | resultArity (VTrunc _) = 1
      | resultArity (VToInt _) = 1
      | resultArity (TensorIndex _) = 1
      | resultArity (ProjectLast _) = 1
      | resultArity (TensorCopy _) = 1
      | resultArity (TensorRef _) = 1
      | resultArity EigenVecs2x2 = 2
      | resultArity EigenVecs3x3 = 2
      | resultArity EigenVals2x2 = 1
      | resultArity EigenVals3x3 = 1
      | resultArity (Select _) = 1
      | resultArity (Subscript _) = 1
      | resultArity (MkDynamic _) = 1
      | resultArity (Append _) = 1
      | resultArity (Prepend _) = 1
      | resultArity (Concat _) = 1
      | resultArity Range = 1
      | resultArity (Length _) = 1
      | resultArity (SphereQuery _) = 1
      | resultArity Sqrt = 1
      | resultArity Cos = 1
      | resultArity ArcCos = 1
      | resultArity Sin = 1
      | resultArity ArcSin = 1
      | resultArity Tan = 1
      | resultArity ArcTan = 1
      | resultArity Exp = 1
      | resultArity Sgn = 1
      | resultArity IntToReal = 1
      | resultArity (NumStrands _) = 1
      | resultArity (Transform _) = 1
      | resultArity (Translate _) = 1
      | resultArity (BaseAddress _) = 1
      | resultArity (ControlIndex _) = 1
      | resultArity (LoadVoxel _) = 1
      | resultArity (Inside _) = 1
      | resultArity (IndexInside _) = 1
      | resultArity (ImageDim _) = 1
      | resultArity (MathFn _) = 1
      | resultArity Dimension = 1
      | resultArity NumCells = 1
      | resultArity GetTracker = 1
      | resultArity BasisData = 1
      | resultArity CellToNode = 1
      | resultArity NodeToPoint = 1
      | resultArity NodeToCoord = 1
      | resultArity Coordinates = 1
      | resultArity (makeTranslateCoordinates _) = 1
      | resultArity (makeFindCellExpand _) = 1
      | resultArity (makeBasisEvaluation _) = 1
      | resultArity (makeFindCellPush _) = 1
      | resultArity GetCell = 1
      | resultArity GetPos = 1
      | resultArity GetNode = 1
      | resultArity ProbeNodeC = 1
      | resultArity ProbeNodeB = 1
      | resultArity (ProbeF _) = 1
      | resultArity (ProbeInvF _) = 1
      | resultArity (ProbePhi _) = 1
      | resultArity (EvalFem _) = 1
      | resultArity checkCell = 1
      | resultArity IfWrap = 1
      | resultArity sp_getCell = 1
      | resultArity swap2 = 1
      | resultArity swap3 = 1
      | resultArity swap4 = 1
      | resultArity swap5 = 1
      | resultArity swap6 = 1

    fun arity IAdd = 2
      | arity ISub = 2
      | arity IMul = 2
      | arity IDiv = 2
      | arity IMod = 2
      | arity INeg = 1
      | arity RAdd = 2
      | arity RSub = 2
      | arity RMul = 2
      | arity RDiv = 2
      | arity RNeg = 1
      | arity RClamp = 3
      | arity RLerp = 3
      | arity RCeiling = 1
      | arity RFloor = 1
      | arity RRound = 1
      | arity RTrunc = 1
      | arity RealToInt = 1
      | arity (LT _) = 2
      | arity (LTE _) = 2
      | arity (EQ _) = 2
      | arity (NEQ _) = 2
      | arity (GT _) = 2
      | arity (GTE _) = 2
      | arity BAnd = 2
      | arity BOr = 2
      | arity BNot = 1
      | arity (Abs _) = 1
      | arity (Max _) = 2
      | arity (Min _) = 2
      | arity (VAdd _) = 2
      | arity (VSub _) = 2
      | arity (VScale _) = 2
      | arity (VMul _) = 2
      | arity (VNeg _) = 2
      | arity (VSum _) = 1
      | arity (VDot _) = 2
      | arity (VIndex _) = 1
      | arity (VClamp _) = 3
      | arity (VMapClamp _) = 3
      | arity (VLerp _) = 3
      | arity (VCeiling _) = 1
      | arity (VFloor _) = 1
      | arity (VRound _) = 1
      | arity (VTrunc _) = 1
      | arity (VToInt _) = 1
      | arity (TensorIndex _) = 1
      | arity (ProjectLast _) = 1
      | arity (TensorCopy _) = 1
      | arity (TensorRef _) = 1
      | arity EigenVecs2x2 = 1
      | arity EigenVecs3x3 = 1
      | arity EigenVals2x2 = 1
      | arity EigenVals3x3 = 1
      | arity (Select _) = 1
      | arity (Subscript _) = 2
      | arity (MkDynamic _) = 1
      | arity (Append _) = 2
      | arity (Prepend _) = 2
      | arity (Concat _) = 2
      | arity Range = 2
      | arity (Length _) = 1
      | arity (SphereQuery _) = 2
      | arity Sqrt = 1
      | arity Cos = 1
      | arity ArcCos = 1
      | arity Sin = 1
      | arity ArcSin = 1
      | arity Tan = 1
      | arity ArcTan = 1
      | arity Exp = 1
      | arity Sgn = 1
      | arity IntToReal = 1
      | arity (NumStrands _) = 0
      | arity (Transform _) = 1
      | arity (Translate _) = 1
      | arity (BaseAddress _) = 1
      | arity (ControlIndex _) = 2
      | arity (LoadVoxel _) = 2
      | arity (Inside _) = 2
      | arity (IndexInside _) = 2
      | arity (ImageDim _) = 1
      | arity (MathFn _) = ~1
      | arity Dimension = 1
      | arity NumCells = 1
      | arity GetTracker = 1
      | arity BasisData = 1
      | arity CellToNode = 1
      | arity NodeToPoint = 1
      | arity NodeToCoord = 1
      | arity Coordinates = 1
      | arity (makeTranslateCoordinates _) = 0
      | arity (makeFindCellExpand _) = 6
      | arity (makeBasisEvaluation _) = 6
      | arity (makeFindCellPush _) = 6
      | arity GetCell = 1
      | arity GetPos = 1
      | arity GetNode = 2
      | arity ProbeNodeC = 2
      | arity ProbeNodeB = 2
      | arity (ProbeF _) = 2
      | arity (ProbeInvF _) = 2
      | arity (ProbePhi _) = 2
      | arity (EvalFem _) = 2
      | arity checkCell = 1
      | arity IfWrap = 3
      | arity sp_getCell = 1
      | arity swap2 = 2
      | arity swap3 = 3
      | arity swap4 = 4
      | arity swap5 = 5
      | arity swap6 = 6

    fun isPure (MkDynamic _) = false
      | isPure (Append _) = false
      | isPure (Prepend _) = false
      | isPure (Concat _) = false
      | isPure _ = true

    fun same (IAdd, IAdd) = true
      | same (ISub, ISub) = true
      | same (IMul, IMul) = true
      | same (IDiv, IDiv) = true
      | same (IMod, IMod) = true
      | same (INeg, INeg) = true
      | same (RAdd, RAdd) = true
      | same (RSub, RSub) = true
      | same (RMul, RMul) = true
      | same (RDiv, RDiv) = true
      | same (RNeg, RNeg) = true
      | same (RClamp, RClamp) = true
      | same (RLerp, RLerp) = true
      | same (RCeiling, RCeiling) = true
      | same (RFloor, RFloor) = true
      | same (RRound, RRound) = true
      | same (RTrunc, RTrunc) = true
      | same (RealToInt, RealToInt) = true
      | same (LT(a0), LT(b0)) = samety(a0, b0)
      | same (LTE(a0), LTE(b0)) = samety(a0, b0)
      | same (EQ(a0), EQ(b0)) = samety(a0, b0)
      | same (NEQ(a0), NEQ(b0)) = samety(a0, b0)
      | same (GT(a0), GT(b0)) = samety(a0, b0)
      | same (GTE(a0), GTE(b0)) = samety(a0, b0)
      | same (BAnd, BAnd) = true
      | same (BOr, BOr) = true
      | same (BNot, BNot) = true
      | same (Abs(a0), Abs(b0)) = samety(a0, b0)
      | same (Max(a0), Max(b0)) = samety(a0, b0)
      | same (Min(a0), Min(b0)) = samety(a0, b0)
      | same (VAdd(a0,a1), VAdd(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VSub(a0,a1), VSub(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VScale(a0,a1), VScale(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VMul(a0,a1), VMul(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VNeg(a0,a1), VNeg(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VSum(a0,a1), VSum(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VDot(a0,a1), VDot(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VIndex(a0,a1,a2), VIndex(b0,b1,b2)) = sameint(a0, b0) andalso sameint(a1, b1) andalso sameint(a2, b2)
      | same (VClamp(a0,a1), VClamp(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VMapClamp(a0,a1), VMapClamp(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VLerp(a0,a1), VLerp(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VCeiling(a0,a1), VCeiling(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VFloor(a0,a1), VFloor(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VRound(a0,a1), VRound(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VTrunc(a0,a1), VTrunc(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (VToInt(a0,a1), VToInt(b0,b1)) = sameint(a0, b0) andalso sameint(a1, b1)
      | same (TensorIndex(a0,a1), TensorIndex(b0,b1)) = samety(a0, b0) andalso sameshape(a1, b1)
      | same (ProjectLast(a0,a1), ProjectLast(b0,b1)) = samety(a0, b0) andalso sameshape(a1, b1)
      | same (TensorCopy(a0), TensorCopy(b0)) = sameshape(a0, b0)
      | same (TensorRef(a0), TensorRef(b0)) = sameshape(a0, b0)
      | same (EigenVecs2x2, EigenVecs2x2) = true
      | same (EigenVecs3x3, EigenVecs3x3) = true
      | same (EigenVals2x2, EigenVals2x2) = true
      | same (EigenVals3x3, EigenVals3x3) = true
      | same (Select(a0,a1), Select(b0,b1)) = samety(a0, b0) andalso sameint(a1, b1)
      | same (Subscript(a0), Subscript(b0)) = samety(a0, b0)
      | same (MkDynamic(a0,a1), MkDynamic(b0,b1)) = samety(a0, b0) andalso sameint(a1, b1)
      | same (Append(a0,a1), Append(b0,b1)) = samety(a0, b0) andalso samety(a1, b1)
      | same (Prepend(a0,a1), Prepend(b0,b1)) = samety(a0, b0) andalso samety(a1, b1)
      | same (Concat(a0), Concat(b0)) = samety(a0, b0)
      | same (Range, Range) = true
      | same (Length(a0), Length(b0)) = samety(a0, b0)
      | same (SphereQuery(a0,a1), SphereQuery(b0,b1)) = sameint(a0, b0) andalso samety(a1, b1)
      | same (Sqrt, Sqrt) = true
      | same (Cos, Cos) = true
      | same (ArcCos, ArcCos) = true
      | same (Sin, Sin) = true
      | same (ArcSin, ArcSin) = true
      | same (Tan, Tan) = true
      | same (ArcTan, ArcTan) = true
      | same (Exp, Exp) = true
      | same (Sgn, Sgn) = true
      | same (IntToReal, IntToReal) = true
      | same (NumStrands(a0), NumStrands(b0)) = StrandSets.same(a0, b0)
      | same (Transform(a0), Transform(b0)) = ImageInfo.same(a0, b0)
      | same (Translate(a0), Translate(b0)) = ImageInfo.same(a0, b0)
      | same (BaseAddress(a0), BaseAddress(b0)) = ImageInfo.same(a0, b0)
      | same (ControlIndex(a0,a1,a2), ControlIndex(b0,b1,b2)) = ImageInfo.same(a0, b0) andalso sameidxctl(a1, b1) andalso sameint(a2, b2)
      | same (LoadVoxel(a0), LoadVoxel(b0)) = ImageInfo.same(a0, b0)
      | same (Inside(a0,a1,a2), Inside(b0,b1,b2)) = VectorLayout.same(a0, b0) andalso ImageInfo.same(a1, b1) andalso sameint(a2, b2)
      | same (IndexInside(a0,a1), IndexInside(b0,b1)) = ImageInfo.same(a0, b0) andalso sameint(a1, b1)
      | same (ImageDim(a0,a1), ImageDim(b0,b1)) = ImageInfo.same(a0, b0) andalso sameint(a1, b1)
      | same (MathFn(a0), MathFn(b0)) = MathFns.same(a0, b0)
      | same (Dimension, Dimension) = true
      | same (NumCells, NumCells) = true
      | same (GetTracker, GetTracker) = true
      | same (BasisData, BasisData) = true
      | same (CellToNode, CellToNode) = true
      | same (NodeToPoint, NodeToPoint) = true
      | same (NodeToCoord, NodeToCoord) = true
      | same (Coordinates, Coordinates) = true
      | same (makeTranslateCoordinates(a0,a1,a2,a3,a4), makeTranslateCoordinates(b0,b1,b2,b3,b4)) = samefnspace(a0, b0) andalso sameint(a1, b1) andalso samerealList(a2, b2) andalso samestringList(a3, b3) andalso samestringLists(a4, b4)
      | same (makeFindCellExpand(a0,a1,a2,a3,a4), makeFindCellExpand(b0,b1,b2,b3,b4)) = samefnspace(a0, b0) andalso samestring(a1, b1) andalso sameint(a2, b2) andalso sameint(a3, b3) andalso sameint(a4, b4)
      | same (makeBasisEvaluation(a0,a1,a2,a3), makeBasisEvaluation(b0,b1,b2,b3)) = samefnspace(a0, b0) andalso samestringList(a1, b1) andalso sameint(a2, b2) andalso samestringList(a3, b3)
      | same (makeFindCellPush(a0,a1), makeFindCellPush(b0,b1)) = sameint(a0, b0) andalso samestring(a1, b1)
      | same (GetCell, GetCell) = true
      | same (GetPos, GetPos) = true
      | same (GetNode, GetNode) = true
      | same (ProbeNodeC, ProbeNodeC) = true
      | same (ProbeNodeB, ProbeNodeB) = true
      | same (ProbeF(a0,a1,a2,a3), ProbeF(b0,b1,b2,b3)) = samefnspace(a0, b0) andalso sameint(a1, b1) andalso sameint(a2, b2) andalso sameint(a3, b3)
      | same (ProbeInvF(a0,a1,a2,a3), ProbeInvF(b0,b1,b2,b3)) = samefnspace(a0, b0) andalso sameint(a1, b1) andalso sameint(a2, b2) andalso sameint(a3, b3)
      | same (ProbePhi(a0,a1,a2,a3), ProbePhi(b0,b1,b2,b3)) = samefnspace(a0, b0) andalso sameint(a1, b1) andalso sameint(a2, b2) andalso sameint(a3, b3)
      | same (EvalFem(a0,a1,a2,a3), EvalFem(b0,b1,b2,b3)) = samefnspace(a0, b0) andalso sameint(a1, b1) andalso sameint(a2, b2) andalso sameintList(a3, b3)
      | same (checkCell, checkCell) = true
      | same (IfWrap, IfWrap) = true
      | same (sp_getCell, sp_getCell) = true
      | same (swap2, swap2) = true
      | same (swap3, swap3) = true
      | same (swap4, swap4) = true
      | same (swap5, swap5) = true
      | same (swap6, swap6) = true
      | same _ = false

    fun hash IAdd = 0w3
      | hash ISub = 0w5
      | hash IMul = 0w7
      | hash IDiv = 0w11
      | hash IMod = 0w13
      | hash INeg = 0w17
      | hash RAdd = 0w19
      | hash RSub = 0w23
      | hash RMul = 0w29
      | hash RDiv = 0w31
      | hash RNeg = 0w37
      | hash RClamp = 0w41
      | hash RLerp = 0w43
      | hash RCeiling = 0w47
      | hash RFloor = 0w53
      | hash RRound = 0w59
      | hash RTrunc = 0w61
      | hash RealToInt = 0w67
      | hash (LT(a0)) = 0w71 + hashty a0
      | hash (LTE(a0)) = 0w73 + hashty a0
      | hash (EQ(a0)) = 0w79 + hashty a0
      | hash (NEQ(a0)) = 0w83 + hashty a0
      | hash (GT(a0)) = 0w89 + hashty a0
      | hash (GTE(a0)) = 0w97 + hashty a0
      | hash BAnd = 0w101
      | hash BOr = 0w103
      | hash BNot = 0w107
      | hash (Abs(a0)) = 0w109 + hashty a0
      | hash (Max(a0)) = 0w113 + hashty a0
      | hash (Min(a0)) = 0w127 + hashty a0
      | hash (VAdd(a0,a1)) = 0w131 + hashint a0 + hashint a1
      | hash (VSub(a0,a1)) = 0w137 + hashint a0 + hashint a1
      | hash (VScale(a0,a1)) = 0w139 + hashint a0 + hashint a1
      | hash (VMul(a0,a1)) = 0w149 + hashint a0 + hashint a1
      | hash (VNeg(a0,a1)) = 0w151 + hashint a0 + hashint a1
      | hash (VSum(a0,a1)) = 0w157 + hashint a0 + hashint a1
      | hash (VDot(a0,a1)) = 0w163 + hashint a0 + hashint a1
      | hash (VIndex(a0,a1,a2)) = 0w167 + hashint a0 + hashint a1 + hashint a2
      | hash (VClamp(a0,a1)) = 0w173 + hashint a0 + hashint a1
      | hash (VMapClamp(a0,a1)) = 0w179 + hashint a0 + hashint a1
      | hash (VLerp(a0,a1)) = 0w181 + hashint a0 + hashint a1
      | hash (VCeiling(a0,a1)) = 0w191 + hashint a0 + hashint a1
      | hash (VFloor(a0,a1)) = 0w193 + hashint a0 + hashint a1
      | hash (VRound(a0,a1)) = 0w197 + hashint a0 + hashint a1
      | hash (VTrunc(a0,a1)) = 0w199 + hashint a0 + hashint a1
      | hash (VToInt(a0,a1)) = 0w211 + hashint a0 + hashint a1
      | hash (TensorIndex(a0,a1)) = 0w223 + hashty a0 + hashshape a1
      | hash (ProjectLast(a0,a1)) = 0w227 + hashty a0 + hashshape a1
      | hash (TensorCopy(a0)) = 0w229 + hashshape a0
      | hash (TensorRef(a0)) = 0w233 + hashshape a0
      | hash EigenVecs2x2 = 0w239
      | hash EigenVecs3x3 = 0w241
      | hash EigenVals2x2 = 0w251
      | hash EigenVals3x3 = 0w257
      | hash (Select(a0,a1)) = 0w263 + hashty a0 + hashint a1
      | hash (Subscript(a0)) = 0w269 + hashty a0
      | hash (MkDynamic(a0,a1)) = 0w271 + hashty a0 + hashint a1
      | hash (Append(a0,a1)) = 0w277 + hashty a0 + hashty a1
      | hash (Prepend(a0,a1)) = 0w281 + hashty a0 + hashty a1
      | hash (Concat(a0)) = 0w283 + hashty a0
      | hash Range = 0w293
      | hash (Length(a0)) = 0w307 + hashty a0
      | hash (SphereQuery(a0,a1)) = 0w311 + hashint a0 + hashty a1
      | hash Sqrt = 0w313
      | hash Cos = 0w317
      | hash ArcCos = 0w331
      | hash Sin = 0w337
      | hash ArcSin = 0w347
      | hash Tan = 0w349
      | hash ArcTan = 0w353
      | hash Exp = 0w359
      | hash Sgn = 0w367
      | hash IntToReal = 0w373
      | hash (NumStrands(a0)) = 0w379 + StrandSets.hash a0
      | hash (Transform(a0)) = 0w383 + ImageInfo.hash a0
      | hash (Translate(a0)) = 0w389 + ImageInfo.hash a0
      | hash (BaseAddress(a0)) = 0w397 + ImageInfo.hash a0
      | hash (ControlIndex(a0,a1,a2)) = 0w401 + ImageInfo.hash a0 + hashidxctl a1 + hashint a2
      | hash (LoadVoxel(a0)) = 0w409 + ImageInfo.hash a0
      | hash (Inside(a0,a1,a2)) = 0w419 + VectorLayout.hash a0 + ImageInfo.hash a1 + hashint a2
      | hash (IndexInside(a0,a1)) = 0w421 + ImageInfo.hash a0 + hashint a1
      | hash (ImageDim(a0,a1)) = 0w431 + ImageInfo.hash a0 + hashint a1
      | hash (MathFn(a0)) = 0w433 + MathFns.hash a0
      | hash Dimension = 0w439
      | hash NumCells = 0w443
      | hash GetTracker = 0w449
      | hash BasisData = 0w457
      | hash CellToNode = 0w461
      | hash NodeToPoint = 0w463
      | hash NodeToCoord = 0w467
      | hash Coordinates = 0w479
      | hash (makeTranslateCoordinates(a0,a1,a2,a3,a4)) = 0w487 + hashfnspace a0 + hashint a1 + hashrealList a2 + hashstringList a3 + hashstringLists a4
      | hash (makeFindCellExpand(a0,a1,a2,a3,a4)) = 0w491 + hashfnspace a0 + hashstring a1 + hashint a2 + hashint a3 + hashint a4
      | hash (makeBasisEvaluation(a0,a1,a2,a3)) = 0w499 + hashfnspace a0 + hashstringList a1 + hashint a2 + hashstringList a3
      | hash (makeFindCellPush(a0,a1)) = 0w503 + hashint a0 + hashstring a1
      | hash GetCell = 0w509
      | hash GetPos = 0w521
      | hash GetNode = 0w523
      | hash ProbeNodeC = 0w541
      | hash ProbeNodeB = 0w547
      | hash (ProbeF(a0,a1,a2,a3)) = 0w557 + hashfnspace a0 + hashint a1 + hashint a2 + hashint a3
      | hash (ProbeInvF(a0,a1,a2,a3)) = 0w563 + hashfnspace a0 + hashint a1 + hashint a2 + hashint a3
      | hash (ProbePhi(a0,a1,a2,a3)) = 0w569 + hashfnspace a0 + hashint a1 + hashint a2 + hashint a3
      | hash (EvalFem(a0,a1,a2,a3)) = 0w571 + hashfnspace a0 + hashint a1 + hashint a2 + hashintList a3
      | hash checkCell = 0w577
      | hash IfWrap = 0w587
      | hash sp_getCell = 0w593
      | hash swap2 = 0w599
      | hash swap3 = 0w601
      | hash swap4 = 0w607
      | hash swap5 = 0w613
      | hash swap6 = 0w617

    fun toString IAdd = "IAdd"
      | toString ISub = "ISub"
      | toString IMul = "IMul"
      | toString IDiv = "IDiv"
      | toString IMod = "IMod"
      | toString INeg = "INeg"
      | toString RAdd = "RAdd"
      | toString RSub = "RSub"
      | toString RMul = "RMul"
      | toString RDiv = "RDiv"
      | toString RNeg = "RNeg"
      | toString RClamp = "RClamp"
      | toString RLerp = "RLerp"
      | toString RCeiling = "RCeiling"
      | toString RFloor = "RFloor"
      | toString RRound = "RRound"
      | toString RTrunc = "RTrunc"
      | toString RealToInt = "RealToInt"
      | toString (LT(a0)) = concat["LT<", tyToString a0, ">"]
      | toString (LTE(a0)) = concat["LTE<", tyToString a0, ">"]
      | toString (EQ(a0)) = concat["EQ<", tyToString a0, ">"]
      | toString (NEQ(a0)) = concat["NEQ<", tyToString a0, ">"]
      | toString (GT(a0)) = concat["GT<", tyToString a0, ">"]
      | toString (GTE(a0)) = concat["GTE<", tyToString a0, ">"]
      | toString BAnd = "BAnd"
      | toString BOr = "BOr"
      | toString BNot = "BNot"
      | toString (Abs(a0)) = concat["Abs<", tyToString a0, ">"]
      | toString (Max(a0)) = concat["Max<", tyToString a0, ">"]
      | toString (Min(a0)) = concat["Min<", tyToString a0, ">"]
      | toString (VAdd(a0,a1)) = concat["VAdd<", intToString a0, ",", intToString a1, ">"]
      | toString (VSub(a0,a1)) = concat["VSub<", intToString a0, ",", intToString a1, ">"]
      | toString (VScale(a0,a1)) = concat["VScale<", intToString a0, ",", intToString a1, ">"]
      | toString (VMul(a0,a1)) = concat["VMul<", intToString a0, ",", intToString a1, ">"]
      | toString (VNeg(a0,a1)) = concat["VNeg<", intToString a0, ",", intToString a1, ">"]
      | toString (VSum(a0,a1)) = concat["VSum<", intToString a0, ",", intToString a1, ">"]
      | toString (VDot(a0,a1)) = concat["VDot<", intToString a0, ",", intToString a1, ">"]
      | toString (VIndex(a0,a1,a2)) = concat["VIndex<", intToString a0, ",", intToString a1, ",", intToString a2, ">"]
      | toString (VClamp(a0,a1)) = concat["VClamp<", intToString a0, ",", intToString a1, ">"]
      | toString (VMapClamp(a0,a1)) = concat["VMapClamp<", intToString a0, ",", intToString a1, ">"]
      | toString (VLerp(a0,a1)) = concat["VLerp<", intToString a0, ",", intToString a1, ">"]
      | toString (VCeiling(a0,a1)) = concat["VCeiling<", intToString a0, ",", intToString a1, ">"]
      | toString (VFloor(a0,a1)) = concat["VFloor<", intToString a0, ",", intToString a1, ">"]
      | toString (VRound(a0,a1)) = concat["VRound<", intToString a0, ",", intToString a1, ">"]
      | toString (VTrunc(a0,a1)) = concat["VTrunc<", intToString a0, ",", intToString a1, ">"]
      | toString (VToInt(a0,a1)) = concat["VToInt<", intToString a0, ",", intToString a1, ">"]
      | toString (TensorIndex(a0,a1)) = concat["TensorIndex<", tyToString a0, ",", shapeToString a1, ">"]
      | toString (ProjectLast(a0,a1)) = concat["ProjectLast<", tyToString a0, ",", shapeToString a1, ">"]
      | toString (TensorCopy(a0)) = concat["TensorCopy<", shapeToString a0, ">"]
      | toString (TensorRef(a0)) = concat["TensorRef<", shapeToString a0, ">"]
      | toString EigenVecs2x2 = "EigenVecs2x2"
      | toString EigenVecs3x3 = "EigenVecs3x3"
      | toString EigenVals2x2 = "EigenVals2x2"
      | toString EigenVals3x3 = "EigenVals3x3"
      | toString (Select(a0,a1)) = concat["Select<", tyToString a0, ",", intToString a1, ">"]
      | toString (Subscript(a0)) = concat["Subscript<", tyToString a0, ">"]
      | toString (MkDynamic(a0,a1)) = concat["MkDynamic<", tyToString a0, ",", intToString a1, ">"]
      | toString (Append(a0,a1)) = concat["Append<", tyToString a0, ",", tyToString a1, ">"]
      | toString (Prepend(a0,a1)) = concat["Prepend<", tyToString a0, ",", tyToString a1, ">"]
      | toString (Concat(a0)) = concat["Concat<", tyToString a0, ">"]
      | toString Range = "Range"
      | toString (Length(a0)) = concat["Length<", tyToString a0, ">"]
      | toString (SphereQuery(a0,a1)) = concat["SphereQuery<", intToString a0, ",", tyToString a1, ">"]
      | toString Sqrt = "Sqrt"
      | toString Cos = "Cos"
      | toString ArcCos = "ArcCos"
      | toString Sin = "Sin"
      | toString ArcSin = "ArcSin"
      | toString Tan = "Tan"
      | toString ArcTan = "ArcTan"
      | toString Exp = "Exp"
      | toString Sgn = "Sgn"
      | toString IntToReal = "IntToReal"
      | toString (NumStrands(a0)) = concat["NumStrands<", StrandSets.toString a0, ">"]
      | toString (Transform(a0)) = concat["Transform<", ImageInfo.toString a0, ">"]
      | toString (Translate(a0)) = concat["Translate<", ImageInfo.toString a0, ">"]
      | toString (BaseAddress(a0)) = concat["BaseAddress<", ImageInfo.toString a0, ">"]
      | toString (ControlIndex(a0,a1,a2)) = concat["ControlIndex<", ImageInfo.toString a0, ",", idxctlToString a1, ",", intToString a2, ">"]
      | toString (LoadVoxel(a0)) = concat["LoadVoxel<", ImageInfo.toString a0, ">"]
      | toString (Inside(a0,a1,a2)) = concat["Inside<", VectorLayout.toString a0, ",", ImageInfo.toString a1, ",", intToString a2, ">"]
      | toString (IndexInside(a0,a1)) = concat["IndexInside<", ImageInfo.toString a0, ",", intToString a1, ">"]
      | toString (ImageDim(a0,a1)) = concat["ImageDim<", ImageInfo.toString a0, ",", intToString a1, ">"]
      | toString (MathFn(a0)) = concat["MathFn<", MathFns.toString a0, ">"]
      | toString Dimension = "Dimension"
      | toString NumCells = "NumCells"
      | toString GetTracker = "GetTracker"
      | toString BasisData = "BasisData"
      | toString CellToNode = "CellToNode"
      | toString NodeToPoint = "NodeToPoint"
      | toString NodeToCoord = "NodeToCoord"
      | toString Coordinates = "Coordinates"
      | toString (makeTranslateCoordinates(a0,a1,a2,a3,a4)) = concat["makeTranslateCoordinates<", fnspaceToString a0, ",", intToString a1, ",", realListToString a2, ",", stringListToString a3, ",", stringListsToString a4, ">"]
      | toString (makeFindCellExpand(a0,a1,a2,a3,a4)) = concat["makeFindCellExpand<", fnspaceToString a0, ",", stringToString a1, ",", intToString a2, ",", intToString a3, ",", intToString a4, ">"]
      | toString (makeBasisEvaluation(a0,a1,a2,a3)) = concat["makeBasisEvaluation<", fnspaceToString a0, ",", stringListToString a1, ",", intToString a2, ",", stringListToString a3, ">"]
      | toString (makeFindCellPush(a0,a1)) = concat["makeFindCellPush<", intToString a0, ",", stringToString a1, ">"]
      | toString GetCell = "GetCell"
      | toString GetPos = "GetPos"
      | toString GetNode = "GetNode"
      | toString ProbeNodeC = "ProbeNodeC"
      | toString ProbeNodeB = "ProbeNodeB"
      | toString (ProbeF(a0,a1,a2,a3)) = concat["ProbeF<", fnspaceToString a0, ",", intToString a1, ",", intToString a2, ",", intToString a3, ">"]
      | toString (ProbeInvF(a0,a1,a2,a3)) = concat["ProbeInvF<", fnspaceToString a0, ",", intToString a1, ",", intToString a2, ",", intToString a3, ">"]
      | toString (ProbePhi(a0,a1,a2,a3)) = concat["ProbePhi<", fnspaceToString a0, ",", intToString a1, ",", intToString a2, ",", intToString a3, ">"]
      | toString (EvalFem(a0,a1,a2,a3)) = concat["EvalFem<", fnspaceToString a0, ",", intToString a1, ",", intToString a2, ",", intListToString a3, ">"]
      | toString checkCell = "checkCell"
      | toString IfWrap = "IfWrap"
      | toString sp_getCell = "sp_getCell"
      | toString swap2 = "swap2"
      | toString swap3 = "swap3"
      | toString swap4 = "swap4"
      | toString swap5 = "swap5"
      | toString swap6 = "swap6"

  end
