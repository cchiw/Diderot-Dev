# specification of operators for HighIR version of the IR.  Each line (other than comments)
# specifies an operator using five fields, which are separated by ":".  The fields are
#       name
#       argument type           (optional)
#       result arity
#       arity
#       comment                 (optional)
#
# Operations with effects are denoted by a "!" as the first character of the line.
#
# integer operations
IAdd : : 1 : 2 : integer addition
ISub : : 1 : 2 : integer subtraction
IMul : : 1 : 2 : integer multiplication
IDiv : : 1 : 2 : integer division
IMod : : 1 : 2 : integer modulo
INeg : : 1 : 1 : integer negation
LT : ty : 1 : 2 :
LTE : ty : 1 : 2 :
EQ : ty : 1 : 2 :
NEQ : ty : 1 : 2 :
GT : ty : 1 : 2 :
GTE : ty : 1 : 2 :
Power : : 1 : 2 : raise real to integer power
BAnd : : 1 : 2 : boolean and
BOr : : 1 : 2 : boolean or
BNot : : 1 : 1 : boolean negation
Abs : ty : 1 : 1 :
Max : ty : 1 : 2 :
Min : ty : 1 : 2 :
# Clamp<ty>(lo, hi, x) -- clamps x to the range lo..hi, where lo and hi are scalars and x
# has type ty
Clamp : ty : 1 : 3 : clamp argument to range
# MapClamp<ty>(lo, hi, x) -- clamps x[alpha] to the range lo[alpha]..hi[alpha], where
# x, lo, and hi all have type ty (which is a non-scalar tensor type).
MapClamp : ty : 1 : 3 : clamp argument to range
# Lerp<ty>(a, b, t) -- computes a + t*(b-a), where a and b have type ty and t has type real
Lerp : ty : 1 : 3 : linear interpolation between 0 and 1
#
#
#FieldPtr: diffgrad,diffcurl
FieldPtr: int*int: 1 : 1:
#Probed domty,rngty, diffgrad,diffcurl
ProbePtr: ty * ty*int*int : 1 : 2:

### vector operations
#
Eigen2x2 : : 2 : 1 : Eigenvector and values for 2x2 matrix
Eigen3x3 : : 2 : 1 : Eigenvector and values for 3x3 matrix
# Zero<ty>() -- zero tensor
Zero : ty : 1 : 0 : all zeros tensor
TensorIndex : ty * shape : 1 : 1 : tensor index (special case of slice)
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
!Append : ty : 1 : 2 : append an element onto a dynamic sequence
!Prepend : ty : 1 : 2 : prepend an element onto a dynamic sequence
!Concat : ty : 1 : 2 : concatenate two dynamic sequences
# Range(lo,hi) -- create a sequence with values [lo, lo+1, ..., hi]
Range : : 1 : 2 : create a range sequence
# Length<ty> -- return the length of a sequence with type ty[]
Length : ty : 1 : 1 : return the length of a dynamic sequence
#
# SphereQuery<dim,seqTy>(pos, radius)
SphereQuery : int * ty : 1 : 2 : find strands within a sphere
#
# conversions
IntToReal : : 1 : 1 :
TruncToInt : : 1 : 1 :
RoundToInt : : 1 : 1 :
CeilToInt : : 1 : 1 :
FloorToInt : : 1 : 1 :
#
# Strand operations
#
# NumStrands<S>() -- denotes the number of strands in the strand set S
#
NumStrands : StrandSets.t : 1 : 0 :
#
# Strands<ty,S>() -- denotes the list of strands in the strand set S; ty is the strand type
Strands : ty * StrandSets.t : 1 : 0 :
#
# image/field operations
Kernel       : Kernel.t * int  : 1 : 0 : Kernel<h, k>, where h is the kernel and k is level of differentiation
KrnMultipleTwoD : : 1 : 2  : Multiple Kernels for each dimension
KrnMultipleThreeD : : 1 : 3  : Multiple Kernels for each dimension
KrnMultipleFourD : : 1 : 4  : Multiple Kernels for each dimension
# Inside<I,s>(n,V) -- tests to see if the index sequence n is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded (see LoadVoxels)
Inside : ImageInfo.t * int : 1 : 2 :
# ImageDim<I,i>(V) -- returns the i'th dimension of the image
ImageDim : ImageInfo.t * int : 1 : 1 :
# BorderCtlDefault<I>(V,t) -- wrap the image V with the border control that returns a default value t
BorderCtlDefault : ImageInfo.t : 1 : 2 :
# BorderCtlClamp<I>(V) -- wrap the image V with the border control that clamps the index
BorderCtlClamp : ImageInfo.t : 1 : 1 :
# BorderCtlMirror<I>(V) -- wrap the image V with the border control that mirrors the index
BorderCtlMirror : ImageInfo.t : 1 : 1 :
# BorderCtlWrap<I>(V) -- wrap the image V with the border control that wraps the index
BorderCtlWrap : ImageInfo.t : 1 : 1 :
#
# nrrd file loading
LoadSeq   : ty * string : 1 : 0 : load sequence from nrrd file
LoadImage : ty * string : 1 : 0 : load image from nrrd file
#
# Kill all active strands
!KillAll : : 0 : 0 :
#
# Stabilize all active strands
!StabilizeAll : : 0 : 0 :
#
# printing support for debugging
!Print : tys : 0 : * : print strings
#
# unlifted math functions
MathFn : MathFns.t : 1 : * : math function
#build mesh/element/space
BuildMesh : mesh : 1 : 2:
BuildElement : element : 1 : 2:
BuildSpace : : 1 : 3:
#Inside fem: dimension
InsideFEM: int : 1 : 2: inside fem
InsideBASE: int : 1 : 2: inside base
#Inside: dimension
InsidePtr: int : 1 : 2:
# surface code. (F,pos)->cell
sp_getCell: : 1: 2:
#printIR
printIR  :  : 1 : 0 : output EIN IR in highToMid to text file