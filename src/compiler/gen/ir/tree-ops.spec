# specification of operators for LowIR version of the IR.  Each line (other than comments)
# specifies an operator using five fields, which are separated by ":".  The fields are
#       name
#       argument type           (optional)
#       result arity
#       arity
#       comment                 (optional)
#
# Operations with effects are denoted by a "!" as the first character of the line.
#
# integer arithmetic operations
IAdd : : 1 : 2 : integer addition
ISub : : 1 : 2 : integer subtraction
IMul : : 1 : 2 : integer multiplication
IDiv : : 1 : 2 : integer division
IMod : : 1 : 2 : integer modulo
INeg : : 1 : 1 : integer negation
#
# scalar arithmetic operations
RAdd : : 1 : 2 : scalar (real) addition
RSub : : 1 : 2 :
RMul : : 1 : 2 :
RDiv : : 1 : 2 :
RNeg : : 1 : 1 :
#
# RClamp(lo, hi, x) -- clamps x to the range lo..hi
RClamp : : 1 : 3 : clamp argument to range
# RLerp(a, b, t) -- computes a + t*(b-a)
RLerp : : 1 : 3 : linear interpolation between 0 and 1
#
# compute integral parts of reals
RCeiling  : : 1 : 1 : compute real ceiling of a vector
RFloor    : : 1 : 1 : compute real floor of a vector
RRound    : : 1 : 1 : compute real rounding to nearest integral real of a vector
RTrunc    : : 1 : 1 : compute real truncation to integral real of a vector
RealToInt : : 1 : 1 : convert real to int
#
# comparisons (integer and scalar)
LT : ty : 1 : 2 :
LTE : ty : 1 : 2 :
EQ : ty : 1 : 2 :
NEQ : ty : 1 : 2 :
GT : ty : 1 : 2 :
GTE : ty : 1 : 2 :
BAnd : : 1 : 2 : boolean and
BOr : : 1 : 2 : boolean or
BNot : : 1 : 1 : boolean negation
Abs : ty : 1 : 1 :
Max : ty : 1 : 2 :
Min : ty : 1 : 2 :
#
# vector arithmetic operations: arguments are width and padded width.
VAdd   : int * int : 1 : 2 : vector addition
VSub   : int * int : 1 : 2 : vector subtraction
VScale : int * int : 1 : 2 : vector scaling
VMul   : int * int : 1 : 2 : vector element-wise multiplication
VNeg   : int * int : 1 : 2 : vector negation
VSum   : int * int : 1 : 1 : sum elements of a vector
VDot   : int * int : 1 : 2 : dot product of two vectors
# VIndex<d,w,i> -- project i'th element (0-based) of vector with type VecTy(d,w)
VIndex  : int * int * int : 1 : 1 : project
#
# VClamp<w,pw>(lo, hi, x) -- clamps x to the range lo..hi
VClamp : int * int : 1 : 3 : clamp argument to range
# VMapClamp<w,pw>(lo, hi, x) -- clamps x[alpha] to the range lo[alpha]..hi[alpha], where
# x, lo, and hi are all d-element vectors
VMapClamp : int * int : 1 : 3 : clamp argument to range
# VLerp<w,pw>(a, b, t) -- computes a + t*(b-a)
VLerp : int * int : 1 : 3 : linear interpolation between 0 and 1
#
# compute integral parts of vectors
VCeiling : int * int : 1 : 1 : compute real ceiling of a vector
VFloor   : int * int : 1 : 1 : compute real floor of a vector
VRound   : int * int : 1 : 1 : compute real rounding to nearest integral real of a vector
VTrunc   : int * int : 1 : 1 : compute real truncation to integral real of a vector
VToInt   : int * int : 1 : 1 : convert vector to int sequence
#
### tensor operations
#
# TensorIndex<ty,idxs>(T) returns the scalar T[idxs], where T has type ty
# and the indices are 0-based.
TensorIndex : ty * shape : 1 : 1 :
#
# ProjectLast<ty,idxs>(T) returns the vector T[idxs,:], where the indices are 0-based
ProjectLast : ty * shape : 1 : 1 :
#
# Copy the a tensor reference to a new tensor value
TensorCopy : shape : 1 : 1 :
#
# Reference a tensor value
TensorRef : shape : 1 : 1 :
#
### matrix operations
#
EigenVecs2x2 : : 2 : 1 : Eigen vectors and values for 2x2 matrix
EigenVecs3x3 : : 2 : 1 : Eigen vectors and values for 3x3 matrix
EigenVals2x2 : : 1 : 1 : Eigen values for 2x2 matrix
EigenVals3x3 : : 1 : 1 : Eigen values for 3x3 matrix
#
### tuple operations
#
# Select<ty,i>(u)  -- select ith element of tuple; ty is tuple type
Select : ty * int : 1 : 1 :
#
### operations on sequences
#
# Subscript<ty>(u,i) -- select ith element of sequence; ty is type of sequence
Subscript : ty : 1 : 2 :
# MkDynamic<ty,n> -- make a sequence with type ty[n] into a dynamic sequence
!MkDynamic : ty * int : 1 : 1 : make a fixed-length sequence dynamic
# Append<seqTy,elemTy>(seq,elem) -- append the element to the end of the sequence
!Append : ty * ty : 1 : 2 : append an element onto a dynamic sequence; ty is element type
# Prepend<seqTy,elemTy>(elem,seq) -- prepend the element to the front of the sequence
!Prepend : ty * ty : 1 : 2 : prepend an element onto a dynamic sequence; ty is element type
!Concat : ty : 1 : 2 : concatenate two dynamic sequences; ty is element type
# Range(lo,hi) -- create a sequence with values [lo, lo+1, ..., hi]
Range : : 1 : 2 : create a range sequence
# Length<ty> -- return the length of a sequence with type ty[]
Length : ty : 1 : 1 : return the length of a dynamic sequence
#
# SphereQuery<dim,seqTy>(pos, radius)
SphereQuery : int * ty : 1 : 2 : find strands within a sphere
#
Sqrt   : : 1 : 1 : returns the sqrt
Cos    : : 1 : 1 : returns the cosine
ArcCos : : 1 : 1 : returns the arccosine
Sin    : : 1 : 1 : returns the sine
ArcSin : : 1 : 1 : returns the arcsine
Tan    : : 1 : 1 : returns the tangent
ArcTan : : 1 : 1 : returns the arctangent
Exp    : : 1 : 1 : returns "e" raised to its argument
Sgn    : : 1 : 1 : return the sign
#
# convert an integer to a real
IntToReal : : 1 : 1 :
#
# Strand operations
#
# NumStrands<S>() -- denotes the number of strands in the strand set S
#
NumStrands : StrandSets.t : 1 : 0 :
#
### image operations
#
Transform : ImageInfo.t : 1 : 1 : Pulls transformation matrix from image.
Translate : ImageInfo.t : 1 : 1 : Pulls translation vector from image.
#
# BaseAddress<I>(V) -- the base address of image V with info I
BaseAddress : ImageInfo.t : 1 : 1 : image base address
#
# ControlIndex<I,ctl,d>(V,i) -- border control for the d'th axis of the image V and index i
ControlIndex : ImageInfo.t * idxctl * int : 1 : 2 :
#
# LoadVoxel<I>(V,offp) -- load a voxel value from the address `a+offp`, where `a` is
# the base address of the image `V` and `offp` is the offset (in datum-size units) of
# the voxel to be loaded.
LoadVoxel : ImageInfo.t : 1 : 2 : load a voxel value
#
# Inside<layout,I,s>(x,V) -- tests to see if the image-space position x is inside the volume
# occupied by the image V.  I is the image info and s is the border width
Inside : VectorLayout.t * ImageInfo.t * int : 1 : 2 :
#
# IndexInside<I,s>(n,V) -- tests to see if the index sequence n is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded (see LoadVoxels)
IndexInside : ImageInfo.t * int : 1 : 2 :
#
# ImageDim<I,i>(V) -- returns the i'th dimension of the image
ImageDim : ImageInfo.t * int : 1 : 1 :
#
### other operations
#
# unlifted math functions
MathFn : MathFns.t : 1 : * : math function
# fem functions
# get data from fem field
Dimension : : 1 : 1 : # dimension
NumCells : : 1 : 1 : # number of cells
GetTracker : : 1 : 1 : # number of cells
BasisData : : 1 : 1 : # basis data
CellToNode : : 1 : 1 : # Cell To Node map
NodeToPoint : : 1 : 1 : #Node To Point map
NodeToCoord : : 1 : 1  : #Node To Coord Map
Coordinates : : 1 : 1 : #Coordinates
#expand find cell
makeTranslateCoordinates :fnspace*int * realList* stringList * stringLists: 1 : 0 : # make translate coords
makeFindCellExpand: fnspace* string*int*int*int : 1 : 6 : #make find cell
makeBasisEvaluation: fnspace*stringList*int* stringList :1:6:

# push find cell
makeFindCellPush  : int * string : 1 : 6 : #FindCell
# post find cel
GetCell  : : 1 : 1 : #get cell
GetPos  : : 1 : 1 : #get pos
GetNode : : 1 : 2 : # getNode
ProbeNodeC: : 1 : 2 : # probe_node and get relevant coordinates
ProbeNodeB: : 1 : 2 : # probe_node and get relevant coordinates
# last step get solution
ProbeF : fnspace*int*int*int : 1 :2 : #probe a field, levels of differentiation on each axis
ProbeInvF :fnspace*int*int*int : 1 : 2 : #probe an inv field, levels of differentiation on each axis
ProbePhi :fnspace *int*int*int : 1 : 2 : #probe an inv field, levels of differentiation on each axis
#the actual last step to get the solution
EvalFem : fnspace * int * int * intList: 1 : 2 : #what it says on the tin
checkCell: : 1: 1 : #check if result of findcell is TF
#ifwrap
IfWrap:: 1 :3 : #if expression. creates in-line function else.
sp_getCell: : 1: 1: #convert fcTy to cell
# swap expression
swap2: : 1 :2 : #swap 2
swap3: : 1 :3 : #swap 3
swap4: : 1 :4 : #swap 4
swap5: : 1 :5 : #swap 5
swap6: : 1 :6 : #swap 6