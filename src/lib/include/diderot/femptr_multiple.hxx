/*! \file tensor.hxx
 *
 * \author Charisee Chiw and Teo Collins
 *
 * Library for fem
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_FEMPTR_MULTIPLE_HXX_
#define _DIDEROT_FEMPTR_MULTIPLE_HXX_

#include "femty.hxx"
#include <stdio.h>


//----------------------- get data from field f -----------------------



inline int NumCells3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s) { return 8; }
inline int Dimension3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s) {return default_int; }

inline MappTy CellToNode3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s)  {
  struct Function *g = (struct Function * )f;
  int * C = (int *)g->CellToNode;
  // printf("The first three are %d, %d, %d",C[3],C[4],C[5]);
  // int data[8][3] = {{0, 1, 2},
  // 		    {1, 2, 3},
  // 		    {2, 3, 4},
  // 		    {1, 3, 5},
  // 		    {3, 4, 6},
  // 		    {3, 5, 7},
  // 		    {3, 6, 7},
  // 		    {6, 7, 8}}; //8 by 3
  // int ** d = new int*[8];
  // for(int i = 0; i < 8; i+=1){
  //   d[i] = new int[3];
  //   for (int j = 0; j < 3; j+=1){
  //     d[i][j] = data[i][j];
  //   }
  // }

  return {g->NumCells,g->Gdim,C};
}
inline FloatMapTy NodeToPoint3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s) {
  struct Function *g = (struct Function * )f;
  FT * C = (FT *)g->NodeToPoint;
  // FT data[2][9] =
  //   {
  //     {0.0,0.0,0.5,0.5,1.0,0.0,1.0,0.5,1.0},
  //     {0.0,0.5,0.0,0.5,0.0,1.0,0.5,1.0,1.0}
  //   }; //2 by 9 and a transpose was needed here.
  // FT ** d = new float*[9];
  // for(int i = 0; i < 9; i+=1){
  //   d[i] = new float[2];
  //   for (int j = 0; j < 2; j+=1){
  //     d[i][j] = data[j][i];
  //   }
  // }

  return {g->NumCells+1,g->dim,C};
}

inline MappTy NodeToCoord3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s) {
struct Function *g = (struct Function * )f;
  int * C = (int *)g->NodeToCoords;

  // int data[9][6] = {{ 3,  4,  5,  1,  2,  0},
  // 		    { 4,  5,  8,  7,  6,  1},
  // 		    { 5,  8, 11,  9, 10,  7},
  // 		    { 4,  8, 14, 13, 12,  6},
  // 		    { 8, 11, 17, 16, 15,  9},
  // 		    { 8, 14, 20, 18, 19, 13},
  // 		    { 8, 17, 20, 21, 19, 15},
  // 		    {17, 20, 24, 22, 23, 21}};

  // int ** d = new int*[9];
  // for(int i = 0; i < 9; i+=1){
  //   d[i] = new int[6];
  //   for (int j = 0; j < 6; j+=1){
  //     d[i][j] = data[i][j];
  //   }
  // }
  

  return {g->NumCells,g->Sdim,C};
}
inline coordTy Coordinates3(FEMSrcTy f, FEMSrcTy m,FEMSrcTy s) {
   FT data[25] = { 0.00000000e+00,   6.25000000e-02,   3.46944695e-18, 0.00000000e+00,   0.00000000e+00,   0.00000000e+00, 1.25000000e-01,   1.25000000e-01,   2.50000000e-01, 1.87500000e-01,   1.04083409e-17,   0.00000000e+00,  1.04083409e-17,   1.87500000e-01,   0.00000000e+00, 3.75000000e-01,   2.50000000e-01,   5.00000000e-01,  2.50000000e-01,   3.75000000e-01,   5.00000000e-01, 5.62500000e-01,   7.50000000e-01,   7.50000000e-01,1.00000000e+00};

   FT * data2 = new FT[25]();
   for(int i = 0; i < 25; i++){data2[i]=data[i];}

  coordTy vcoordTy = {data2};return vcoordTy;
}


#endif // !_DIDEROT_FEMPTR_HXX_
