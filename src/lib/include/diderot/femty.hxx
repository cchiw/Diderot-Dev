/*! \file tensor.hxx
 *
 * \author John Reppy
 *
 * Base classes for the generated tensor_shape and tensor_ref_shape classes generated
 * by the compiler.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_FEMTY_HXX_
#define _DIDEROT_FEMTY_HXX_

#include <iostream>
using namespace std;
namespace diderot {
    namespace __details {
    } // namespace __details
} // namespace diderot

typedef double FT;

//creating some types as holders
//(*PDE solution*)
typedef void* FEMSrcTy; //use this if using a pointer to a field
//typedef FT FEMSrcTy; //use this if using a real value
//----------------------- types -----------------------
typedef const FT* avgPosRefTy;
typedef FT* newposTy ; // Ty.TensorTy[defaultN]
typedef FT* crevTy; //Ty.TensorTy[defaultN
//----------------------- structures -----------------------
struct fcTy {int cell; newposTy pos;}; //2-d array of ints
struct polynomial {int degree;}; //representation of polynomials
struct BasisDataTy  {int n; polynomial *basis_polynomial;};//Data from PDE solution
struct polyTy {FT data;};
struct coordTy {FT * data;};
struct jacobiansTy {FT data;}; //2-d array of ints
struct JITy {FT data;};
struct transformsTy {FT data;};
struct MappTy {int row; int col; int * data;}; //secretly 2-d array of ints
struct FloatMapTy {int row; int col; FT * data;}; //There is a tension between map types at this level and higher ones.
//----------------------- arrTy -----------------------
struct arrTy {int col; int * data;}; //2-d array of ints
struct arrFty {int col; FT * data;};
typedef arrTy NodeTy;// arrTy(Ty.intTy)
typedef arrFty brevTy; //arrTy(Ty.FTTy)

struct optStruct {int32_t * tracker; int32_t * Nbrs;};

#endif // !_DIDEROT_FEMTY_HXX_
