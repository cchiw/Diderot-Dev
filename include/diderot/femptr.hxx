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

#ifndef _DIDEROT_FEMPTR_HXX_
#define _DIDEROT_FEMPTR_HXX_

#include "femty.hxx"
#include <stdio.h>

//temporary struct
struct Function {
  int dim;
  int Gdim;
  int Sdim;
  int  NumCells;
  void * GetTracker; 
  void * CellToNode;
  void * NodeToCoords;
  void * NodeToPoint;
  FT * Coords;
  void  * Nbrs;
};

//----------------------- tmp data -----------------------
FT default_list [4] = {5,1,4,7};
FT default_float = 5.4;
int default_int = 1;
int default_length = 4;
int default_int_list [4] = {1,2,3,4};
NodeTy default_node  = { 4, (int *) default_int_list};
//----------------------- get data from field f -----------------------
inline BasisDataTy BasisData(FEMSrcTy f) {
    polynomial p ={default_int};
    int n = default_length;
    polynomial bp [4]={p,p,p,p};
    BasisDataTy vb = {n, bp};
    return vb;
}


inline int NumCells1(FEMSrcTy f) {struct Function *g = (struct Function * )f; return (g->NumCells); }
inline int Dimension1(FEMSrcTy f) {return default_int; }


int data [4]= {5,1,7,4};
FT data2[4] = {5.0,1.0,2.0,3.0};
MappTy vMapp = {4, 5, (int *) data}; //warning stupid and evil
FloatMapTy vMapp2 = {4,5, (FT *)data2 };
inline MappTy CellToNode1(FEMSrcTy f)  {
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

inline  optStruct GetTracker(FEMSrcTy f){
    struct Function *g = (struct Function * )f;
    int32_t * t = (int32_t *)g->Nbrs;
    //     for(int i = 0; i < g->NumCells*10; i++){
    //       printf("Cell %d is %d\n",i,t[i]);
    //      }
    // exit(0);
    int32_t * a = (int32_t *)g->GetTracker;
    // printf("The initial tracker is %d\n",*a);
    // exit(0);
    return {a,t};
}


inline FloatMapTy NodeToPoint1(FEMSrcTy f) {
  struct Function *g = (struct Function * )f;
  FT * C = (FT *)g->NodeToPoint;
  //FT * D = (FT *) g->NodeToPoint;
  //printf("The first three are %f,%f,%f,%f\n",D[0],D[1],D[2],D[3]);
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


inline MappTy NodeToCoord1(FEMSrcTy f) {
struct Function *g = (struct Function * )f;
  int * C = (int *)g->NodeToCoords;
  //printf("The first 6 are: %d,%d,%d,%d,%d,%d",C[0],C[1],C[2],C[3],C[4],C[5]);

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

inline coordTy Coordinates1(FEMSrcTy f) {
  struct Function *g = (struct Function * )f;
  FT * C = (FT *)g->Coords;
  // for(int i = 0; i < 100; i++){
  //   printf("NValue: %f\n",C[i]);
  // }
  // printf("Ergm\n");
  // //FT (D)[25][2][2] = (FT [25][2][2]) C;
  // for(int i = 0; i < 25; i++){
  //   int j = 4*i;
  //   printf("NValue: %f\n",C[j]);
  // }
  // // for(int i = 0; i <= 25; i++){
  // //   printf("NValue: %f\n",D[0][1][i]);
  // // }
  // // for(int i = 0; i <= 25; i++){
  // //   printf("NValue: %f\n",D[1][0][i]);
  // // }
  // // for(int i = 0; i <= 25; i++){
  // //   printf("NValue: %f\n",D[1][1][i]);
  // // }
  // exit(0);
  //printf("The first four are %f,%f,%f,%f--",C[0],C[1],C[2],C[3]);
   // FT data[25] = { 0.00000000e+00,   6.25000000e-02,   3.46944695e-18, 0.00000000e+00,   0.00000000e+00,   0.00000000e+00, 1.25000000e-01,   1.25000000e-01,   2.50000000e-01, 1.87500000e-01,   1.04083409e-17,   0.00000000e+00,  1.04083409e-17,   1.87500000e-01,   0.00000000e+00, 3.75000000e-01,   2.50000000e-01,   5.00000000e-01,  2.50000000e-01,   3.75000000e-01,   5.00000000e-01, 5.62500000e-01,   7.50000000e-01,   7.50000000e-01,1.00000000e+00};

   // FT * data2 = new float[25]();
   // for(int i = 0; i < 25; i++){data2[i]=data[i];}

  coordTy vcoordTy = {C};return vcoordTy;
}

inline bool IsAffine (FEMSrcTy f) { return true; }

//----------------------- get data from basis data -----------------------
inline int HighestOrder(BasisDataTy b) {
    int d= 0;
    int n = b.n; //length of b.basis_polynomial
    polynomial *xptr = b.basis_polynomial; //get polynomial
    for (int i=0; i<n; i++){
        int degree =  xptr->degree;
        d= max(d, degree); //where degree is the highest exponent present
        //++xptr;
    }
    return d;
}
inline avgPosRefTy AvgPosRef (BasisDataTy b) {FT  *ptr = NULL;  return ptr;}

//----------------------- get cell helpers -----------------------
int GetCell  (fcTy fc) { return fc.cell; }
newposTy GetPos  (fcTy fc) { return fc.pos;}

//----------------------- probe phi and coord -----------------------
inline int ProbeF (crevTy f, newposTy x) { return default_int; }//not done
inline newposTy ProbeInvF (arrTy f, newposTy x) { return x; }
// inline FT ProbePhi(brevTy a, newposTy b) {
//   FT result = 0;
//   FT *weights = a.data;
//   int len = a.col;
//   FT * res = new FT [len];
//   FT * basisAtPos = helpEvalBasis(b,res);
  
//   for(int itter = 0; itter < len; itter++)
//     {
//       result += basisAtPos[itter]*weights[itter];
//     }

//   delete a.data;//allocated in ProbeNodeC -> need for reogranization
//   return(result);
  

// }

//------------------------------------------------------------------------
// inline transformsTy GetTransforms(BasisDataTy b, MappTy mn, MappTy  mp, int dim) {
//     int pcell = default_int; //default
//     int *mn_data = mn.data[pcell];
//     int n = mn.row;//should this not be the number of columns, Teo?
//     FT *np = NULL;
//     for (int node=0; node<n; node++){
//         np[node] = mn_data[node];
//     }
//     polynomial *basis_polys = b.basis_polynomial;
//     FT *listOfPolys = NULL;
//     //...
//     transformsTy vtransformsTy = {default_float};
//     return vtransformsTy;
// }
inline jacobiansTy  GetJacobians(BasisDataTy b, MappTy mn, MappTy  mp, int dim) {
    jacobiansTy  vjacobiansTy  = {default_float};return  vjacobiansTy;
}
inline JITy JIs(jacobiansTy vJ, avgPosRefTy AvgPosRef) {
    JITy vJITy = {default_float}; return vJITy;
}
NodeTy GetNode (int cell, MappTy mC,MappTy mN) //can this be inlined?
{
  if (cell == -1){NodeTy  vnodeTy = {0,0}; return(vnodeTy);}
  int * nodes = &mN.data[mN.col*cell];
  NodeTy  vnodeTy  = {mN.col, nodes};
  return vnodeTy;
  
  
}

inline FT ** getPoints(int cell, MappTy nM, FloatMapTy pM, FT ** A){
  int * nodes = &nM.data[nM.col*cell];
  int maxItter = nM.col;
  FT * points = pM.data;
  for(int i = 0; i < maxItter; i++){
    int c = nodes[i];
    FT *a = &points[pM.col*c];
    //for(int j = 0; j < pM.row; j++){printf("For %d,%d We have a %f\n",i,j,a[j]);}
    A[i] = a; //caution about how points are initially formatedcu
  }
  return(A);
}
inline brevTy ProbeNodeC(NodeTy node, coordTy c, FT * array, int start,int mult) { //we are really hoping this gets inlined
  int maxItter = node.col;
  //printf("The max itter is %d\n",maxItter);
  for(int i = 0; i < maxItter; i++)
    {
      //printf("The node data is %d",node.data[i]);
      int id = start + mult * node.data[i];

      //printf("The id is %d",id);

      array[i] = c.data[id];
      //printf("With start %d, we have value %f\n",start,array[i]);
    }
  brevTy res = {maxItter, array};


  return(res);
}

arrTy ProbeNodeB(NodeTy node, BasisDataTy v) {arrTy  varrTy  = default_node; return varrTy; }

#endif // !_DIDEROT_FEMPTR_HXX_

