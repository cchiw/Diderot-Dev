(* gen-tys-and-ops.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenFem : sig
	      
	      val makeTransformHelper :{prefix:string, dim:int, rowAcc:int list, basisFunctions:string list} -> string list
														       
	      val makeTransform : {prefix:string,dim:int} -> string list
								    
	      val helpMakeJacobian :    {prefix:string,dim:int, colAcc:int list, basisJacobian:string list list, dimDimTuples:(int * int) list} -> string list
																			  
	      val makeJacobian: {prefix:string, dim:int} -> string list

	      val helpInverseJacobian :{prefix:string,dim:int, colAcc:int list, basisJacobian:string list list,  dimDimTuples:(int * int) list} -> string list
	      val jacobianInverse:{prefix:string,dim:int} -> string list

 
	      val helpJIs : {prefix:string, dim:int, rowAcc:int list, avgPos:real list, helpInverseJacobian:string list} -> string list

	      val jIs : {prefix:string, gdim : int, dim : int} -> string list
																   
	      val helpTranslateCoordinates:{prefix:string, dim:int, rowAcc:int list, avgPos:real list} -> string list
														 
	      val wrapCell:{prefixM:string,prefixME:string, dim:int, testString:string, isAffine:bool, gdim: int} -> string list
	      val makeBasisEvaluation :{prefixME:string, sBasisFunctions:string list, sBasisItter:int list} -> string list
	      val makeProbePhi :{prefixME:string, nnodes:int} -> string list
	      val makeEvaluate : {prefixME : string,sdim : int, dim: int , dlevel : int, shape : int list, namespace : string} -> string list
	      val makePhiDerv : {prefix : string, sdim : int,dim : int, dlevel : int, s_derivativeInfoList : string list} -> string list
	      val derivativeAffineCase : {prefix : string ,sdim : int , dim : int ,dlevel : int} -> string list
	  end =  struct

structure RN = CxxNames
structure TS = TargetSpec
		   
    val float = "double" (*set me!*)
		
    (* function name *)
     val fn_makeTransformHelper = RN.fn_makeTransformHelper
     val fn_makeTransform = RN.fn_makeTransform
     val fn_helpMakeJacobian = RN.fn_helpMakeJacobian
     val fn_makeJacobian = RN.fn_makeJacobian
     val fn_helpInverseJacobian = RN.fn_helpInverseJacobian
     val fn_makeJacobianInverse = RN.fn_makeJacobianInverse
     val fn_helpJIs = RN.fn_helpJIs
     val fn_jIs = RN.fn_jIs
     val fn_helpTranslateCoordinates = RN.fn_helpTranslateCoordinates
     val fn_makeFindCell = RN.fn_makeFindCell
     val fn_helpEvalBasis =  RN.fn_helpEvalBasis
     val fn_ProbePhi = RN.fn_ProbePhi
     val fn_makeEvaluate = RN.fn_makeEval
     val fn_makePhiDerv = RN.fn_makePhiDerv
     val fn_makeFDerv = RN.fn_makeFDerv
     val fn_affineDerv = RN.fn_affineDerv
 
      val namespace = "ex1"
     fun indent e = String.concat["\n\t",e]
     fun indentt e= String.concat["\n\t\t",e]
     fun indenttt e= String.concat["\n\t\t\t",e]


     (*J and C decleration helper!*)
     fun decC dim nnodes = "C["^Int.toString(dim)^"]["^Int.toString(nnodes)^"];"

 
     (*now we want to write some symbolic linear algebra that will help us out later on:*)
     (*This creates a vector of (v1Name[i]*v2name[i])_{i}*)
     fun dotProdHelp v1Name v2Name itter = List.map
                                     (fn x => let
                                     val a = String.concat(["[",Int.toString(x),"]"])
                                     in
                                     String.concat(["(",v1Name,a," * ", v2Name,a,")"])
                                     end) itter
     (*This converges the above vector to a dot product and then writes assign oper dotproduct where assign is some variable and oper is an operation like += or =*)
     fun dotProd v1Name v2Name itter assign oper =
     assign^oper^(List.foldr (fn (x,y) => String.concat([x," + ",y])) "0" (dotProdHelp v1Name v2Name itter) )^";\n"
     
 
     val tempGet = fn (x,row, basisJacobian) => List.nth(List.nth(basisJacobian,x),row)
 
     fun makeDot (row, col, colAcc,basisJacobian) =
        List.map (fn x => String.concat(["(C[",Int.toString(x),"][",Int.toString(row),"] * (",tempGet(x,col, basisJacobian),"))" ])) colAcc
 
    fun makeEntry (row, col,colAcc,basisJacobian) =
        indent ((String.concat(["J[",Int.toString(row),"][",Int.toString(col),"] = "]))^(List.foldr (fn (x,y) => String.concat([x," + ",y])) "0" (makeDot(row, col, colAcc,basisJacobian)))^";\n")

 

     (*These faciliate matrix multiplication so that if C is some matrix where each column is a coordinate and P a vector of polynomials, we can compute CP(k)*)
     fun rowToVector (i, rowAcc) = List.map (fn x => String.concat(["C[",Int.toString(x),"][",Int.toString(i),"]"])) rowAcc
     fun almostDot (i,rowAcc, basisFunctions) = (ListPair.zip ((rowToVector(i,rowAcc)), basisFunctions))
     fun component (i, sub, rowAcc, basisFunctions) =
       List.foldr (fn ((x,y),last) => String.concat[last," + (",x,")*(",y,")"]) sub (almostDot (i,rowAcc, basisFunctions))



     fun implaceArrayS dims level sepa sepb = String.concat(List.tabulate(level,fn x => sepa^Int.toString(List.nth(dims,x))^sepb))
     fun implaceArray dims level = String.concat(List.tabulate(level,fn x => "["^Int.toString(List.nth(dims,x))^"]"))
     fun implaceArrayU dim level = implaceArray (List.tabulate(level, fn x => dim)) level
     (*A quick solution stolen from: https://rosettacode.org/wiki/Combinations_with_repetitions#Standard_ML*)
     fun combs_with_rep (m, xs) = let
	 val arr = Array.array (m+1, [])
     in
	 Array.update (arr, 0, [[]]);
	 app (fn x =>
		 Array.modifyi (fn (i, y) =>
				   if i = 0 then y else y @ map (fn xs => x::xs) (Array.sub (arr, i-1))
			       ) arr
	     ) xs;
	 Array.sub (arr, m)
     end
				      (*https://stackoverflow.com/questions/4102605/standard-ml-permutations*)

     fun interleave x [] = [[x]]
       | interleave x (h::t) =
	 (x::h::t)::(List.map(fn l => h::l) (interleave x t))

     fun permute nil = [[]]
       | permute (h::t) = List.concat( List.map (fn l => interleave h l) (permute t))

     fun isolate [] = []
       | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs)

     fun perms_with_rep (m,xs) = isolate(List.concat(List.map permute (combs_with_rep(m,xs))))

					
 
      (*this generates an inline helper for makeTransform that already has C generated
         and has a modoff vector that allows us to subtract something from the result of CP(k)
       Modoff is added to make the inverse function theorem itteration easier to write*)
    fun makeTransformHelper{prefix, dim,rowAcc, basisFunctions} =
          let
              val t1 = "inline "^float^" *"^fn_makeTransformHelper(prefix)^"("^float^"** C, "^float^"* k, const "^float^" *modOff, "^float^" result[2]){\n"
              (*declare static array with result*)
              val t2 = indent(String.concat([""^float^"  result[",Int.toString(dim),"];\n"]))
              (*matrix multplication*)
	      val tmp = List.tabulate(List.length(basisFunctions),fn x => x)
              val t3 = List.map (fn x => indent(String.concat(["result[", Int.toString(x) ,"] = ",component (x, ("0-modOff["^Int.toString(x)^"]"), tmp, basisFunctions) ,";\n" ]))) rowAcc
			
              val t4 = indent("return(result);\n}\n")

          in
               [t1]@t3@[t4]
          end



      (*we can now make makeTransform itself via the maps:nM,pM and cell. Manipulation of these is handled through getPoint, which is handled femptr.hxx*)
    fun makeTransform {prefix, dim} =
        let
              val t1 = ""^float^" * "^fn_makeTransform (prefix)^"("^float^" * k, int cell, MappTy nM, FloatMapTy pM ){\n"
              val t2 = indent ""^float^" *C["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim,fn x => "0")))^"};\n"
              val t3 = indent "getPoints(cell,nM,pM,C);\n"
              val t4 = indent (""^float^" random["^Int.toString(dim)^"] = { 0.0 };\n")
	      val t5 = indent (""^float^" result["^Int.toString(dim)^"] = { 0.0 };\n")
              val t7 = indent "return("^fn_makeTransformHelper(prefix)^"(C,k,random,result));\n}\n"

          in
              [t1,t2,t3,t4,t5,t7] 
          end

    fun helpMakeJacobian{prefix, dim, colAcc, basisJacobian, dimDimTuples} =
      let
	  val jacobType = "["^(Int.toString(dim))^"]"
              val t1 = "inline "^float^" ** "^fn_helpMakeJacobian(prefix)^"("^float^" ** C, "^float^" *k, "^float^" * J"^jacobType^"){\n"
              val t2 = indent " "^float^" * J["^(Int.toString(dim))^"];" (*this is dim by dim*)
              val entries = List.map (fn (r, c)=> (makeEntry (r,c, colAcc,basisJacobian))) dimDimTuples
              val t3 = indent "return(J);\n}\n"
                                                                                    
          in
              [t1]@entries@[t3]
          end

    fun makeJacobian {prefix, dim} =
          let
              val t1 = ""^float^" ** "^fn_makeJacobian(prefix)^"(int cell, MappTy nM, FloatMapTy pM, "^float^" *k, "^float^" * J["^(Int.toString(dim))^"]){\n"
              val t2 = indent ""^float^" *C["^(Int.toString(dim))^"];\n"
              val t3 = indent "getPoints(cell,nM,pM,C);\n"
              val t4 = indent "return("^fn_helpMakeJacobian(prefix)^"(C,k,J));\n}\n"
          in
              [t1,t2,t3,t4]
          end

 
     fun makeTempEntry (row, col, colAcc, basisJacobian) =
        indent((String.concat([""^float^" t",Int.toString(row+1),Int.toString(col+1), " = "]))^(List.foldr (fn (x,y) => String.concat([x," + ",y])) "0" (makeDot(row, col, colAcc,basisJacobian)))^";\n")

    (*See http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html for inverse formulaas*)
    fun helpInverseJacobian{prefix, dim, colAcc, basisJacobian, dimDimTuples} =
         let
             (*computations for entries of the jacobianL*)
             val entries = List.map (fn (r,c)=> (makeTempEntry(r,c, colAcc,basisJacobian) )) dimDimTuples
             (*computation of inverse det:*)
             val det =
             if (dim = 2)
                then ""^float^" det = 1.0/(t11*t22-t12*t21);\n"
                else
                    if (dim = 3)
                    then
                        ""^float^" det = 1.0/(t11*t22*t33 +  t21*t32*t13 + t31*t12*t23 - t11*t32*t23 - t31*t22*t13 - t21*t12*t33);\n"
                    else raise Fail "Only dim = 2 and dim =3 implemented as higher dim spaces not supported"
             (*computation of inverse matrix*)
             val mat =
                if (dim = 2)
                then [["J[0][0]= t22*det;\n","J[0][1] = 0 - t12*det;\n"],
                    ["J[1][0] = 0 - t21*det;\n", "J[1][1] = t11*det;\n"]]
                else
                    if (dim =3)
                    then
                     [["J[0][0] = det*(t22*t33-t23*t32);\n",
                     "J[0][1] = det*(t13*t32-t12*t33);\n", "J[0][2] = det*(t12*t23-t13*t22);\n"],
                     ["J[1][0] = det*(t23*t31-t21*t33);\n",
                     "J[1][1] = det*(t11*t33-t13*t31);\n", "J[1][2] = det*(t13*t21-t11*t23);\n"],
                     ["J[2][0] = det*(t21*t32-t22*t31);\n",
                     "J[2][1] = det*(t12*t31-t11*t32);\n", "J[2][2] = det*(t11*t22-t12*t21);\n"]
                     ]
             else raise Fail "Only dim=2 and dim=3 implemented"
     
         val t1 = "inline "^float^" ** "^fn_helpInverseJacobian(prefix)^"("^float^" ** C, "^float^" *k, "^float^" * J["^(Int.toString(dim))^"]){\n"
         val t2 = indent  ""^float^" *J["^(Int.toString(dim))^"];\n"
         val t3 = entries
         val t4 = indent det
         val t5 = String.concat((List.map indent (List.concat(mat))))
         val t6 = indent "return(0);\n}\n"
         
         in
            [t1]@entries@[t4,t5,t6]
        end
 
      (*wrap up all of the rest into a functon you might call with a cell and the relevant maps*)
    fun jacobianInverse {prefix, dim} =
           let
               val t1 = ""^float^" ** "^fn_makeJacobianInverse(prefix)^"(int cell, MappTy nM, FloatMapTy pM, "^float^" *k,"^float^" * J["^(Int.toString(dim))^"]){\n"
               val t2 = indent ""^float^" *C["^(Int.toString(dim))^"];\n"
               val t3 = indent "getPoints(cell,nM,pM,C);\n"
               val t4 = indent "return("^fn_helpInverseJacobian(prefix)^"(C,k,J));\n}\n"
           in
               [t1,t2,t3,t4]
           end

      (*make a function specalized to the evaluation at the above at a particular point.
      We will call this so much it is not even funny.*)
    fun helpJIs{prefix, dim, rowAcc, avgPos, helpInverseJacobian}  =
          let
          val _ = print ("here"^Int.toString(dim))
          val cvtTy =(case dim
                of 2 =>"["^(Int.toString(2))^"]["^(Int.toString(2))^"]"
                | 3 =>"["^(Int.toString(3))^"]["^(Int.toString(3))^"]"
          (* end case*))
          
          
              val t1 = "inline void *"^fn_helpJIs(prefix)^"("^float^" **C,"^float^" J"^cvtTy^"){\n"
              val t2 = indent ""^float^" k["^Int.toString(dim)^"];\n"
              val t3 = List.map (fn (x,y) => indent (String.concat(["k[",Int.toString(x),"] = ",Real.toString(y),";\n"]))) (ListPair.zip (rowAcc, avgPos))
              val rest = List.tl helpInverseJacobian
          in
              [t1,t2]@t3@rest
          end
          
          
    fun JI_Inverse{prefix, dim, rowAcc, avgPos, colAcc=colAcc, basisJacobian,dimDimTuples}  =
          let
          val _ = print ("here"^Int.toString(dim))
          val cvtTy =(case dim
          of 2 =>"["^(Int.toString(2))^"]["^(Int.toString(2))^"]"
          | 3 =>"["^(Int.toString(3))^"]["^(Int.toString(3))^"]"
          (* end case*))
          
          
          val t1 = "inline void *"^fn_helpJIs(prefix)^"("^float^" **C,"^float^" J"^cvtTy^"){\n"
          val t2 = indent ""^float^" k["^Int.toString(dim)^"];\n"
          val t3 = List.map (fn (x,y) => indent (String.concat(["k[",Int.toString(x),"] = ",Real.toString(y),";\n"]))) (ListPair.zip (rowAcc, avgPos))
          val rest = List.tl helpInverseJacobian
          in
          [t1,t2]@t3@rest
          end
          
          
    fun jIs{prefix,gdim,dim} =
      let
            val _ = print ("here"^Int.toString(dim))
          val cvtTy =(case dim
                of 2 =>"["^(Int.toString(2))^"]["^(Int.toString(2))^"]"
                | 3 =>"["^(Int.toString(3))^"]["^(Int.toString(3))^"]"
                (* end case*))
           val _ = print(cvtTy)
            
	  val t1 = "inline void *"^fn_jIs(prefix)^"("^float^" J"^cvtTy^",MappTy nM, FloatMapTy pM,int cell){\n"
	  val t2 = indent(""^float^" *C["^(Int.toString(gdim))^"] = {"^(String.concatWith "," (List.tabulate(gdim,fn x => "0")))^"};\n")
	  (* val t2 = indent ""^float^" *C["^(Int.toString(gdim))^"];\n" *)
          val t3 = indent "getPoints(cell,nM,pM,C);\n"
	  val t4 = indent (fn_helpJIs(prefix)^"(C,J);") 
	  val t5 = indent "return(0);}\n"
	  
      in
	  [t1,t2,t3,t4,t5]
      end


      (*We now bring everything togeather into the translate coordinates loop. 
       This is where the inverse function theorem itteration lives*)
 fun helpTranslateCoordinates{prefix, dim, rowAcc, avgPos} =
   let
       
	    val jInit = ""^float^" J["^(Int.toString(dim))^"]["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim, fn x => "{"^(String.concatWith "," (List.tabulate(dim,fn x => "0.0f")))^"}")))^"};\n"
              val t1 = "inline "^float^" * "^fn_helpTranslateCoordinates (prefix)^"("^float^" **C, const  "^float^" * x0, int itter, "^float^" newpos["^(Int.toString(dim))^"]){\n"
              
              val t3 = String.concat(List.map (fn (x,y) => String.concat(["\n\t newpos[",Int.toString(x),"] = ",Real.toString(y),";\n"])) (ListPair.zip (rowAcc, avgPos)))
              val t4 = indent "bool notConverged =true;\n"
              val t5 = indent "int i = 0;"
              val t6 = indent ""^float^" change["^(Int.toString(dim))^"] = {0.0 };\n"
              val t7 = indent ""^float^" * temp;\n"^"\t"^float^" random["^Int.toString(dim)^"] = { 0.0 };\n\t"^jInit
              val t8 = indent (fn_helpJIs(prefix)^"(C,J);\n")
              val t9 = indent "while ((i < itter) && notConverged){\n"
              val t10 = indentt ("temp = "^fn_makeTransformHelper(prefix)^"(C,newpos, x0,random);\n") (*a = (F(newpos)-x_{0})*)
              val t11 = (*This perofrms *change = (DF(k))^{-1}a*)
                  String.concat(List.map
                                    (fn x =>
                                        indentt(dotProd
                                             (String.concat(["J[",Int.toString(x),"]"]))
                                             "temp"
                                             rowAcc
                                             (String.concat(["change[",Int.toString(x),"]"]))
                                             "="))
                                    rowAcc )
              val t12 = indentt ""^float^" test = "^(dotProd "change" "change" rowAcc "" "")
              val t13 = indentt"if(test < 1e-12*1e-12){notConverged=false;}\n"
              val t14 = String.concat(List.map (fn x =>
                                                   let
                                                       val a = Int.toString(x)
                                                   in
                                                       String.concat(["\n\t\tnewpos[",a,"] -=","change[",a,"];\n"]) (*this performs newpos = newpos - change = newpos - (DF(k))^{-1}(F(newpos)-x_{0}) *)
                                                   end) rowAcc)
              val t15 = indentt "i+=1;\n}\n"
              val t16 = indent "return(newpos);\n}\n"

          in
              [t1,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16]
          end
              
 (*finally wrap cell, basically uncahnged except for the corrections to the argument to helpTranslatecoordiantes, the movement of the creation of C outside the IFT stuff, and an actual test*)

 
       
      fun wrapCell{prefixM, prefixME, dim,testString,isAffine,gdim} =
        let
	    val opt =  true

              val t0 = fn_makeFindCell prefixME
              val t1 = "fcTy "^t0^" (const "^float^"* vp,  int32_t vL, MappTy nM, FloatMapTy pM, optStruct opt)"
              val t2 = indent("{")
	      val tt = if opt then indent "int32_t * tracker = opt.tracker;\n if(*tracker > vL || *tracker < 0)\n\t{*tracker=0;}\n;\n"
		       else ""
	      val t = indent "printf(\"print abs(%f * %f-\",vp[0],vp[1]);"
              val t3 = indent("")
              val t4 = indent(if isAffine then "int range = 1;" else "int range =16;")
              val t5 = indent(""^float^" *C["^(Int.toString(gdim))^"] = {"^(String.concatWith "," (List.tabulate(gdim,fn x => "0")))^"};\n")
	      (*val t6 = indent ""^float^" newposAllocate["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim,fn x => "0")))^"};\n"*)
	      val t6 = indent ""^float^" * newposAllocate = new "^float^"["^(Int.toString(dim))^"]();"
	      val optCode =
		  let
		      val t1 = indent "int lastCell = *opt.tracker;\n"
		      val t2 = indent "int32_t * nbrs = &opt.Nbrs[vL*lastCell];\n"
		      val t3 = indent("for (int cellIndex = 0;  cellIndex < vL;  cellIndex+=1) {")
		      val t4 = indentt "int32_t cell = nbrs[cellIndex];\n"
		      val t5 = indentt "getPoints(cell,nM,pM,C);\n"
		      val t6 = indentt "newposTy newpos = "^fn_helpTranslateCoordinates(prefixM)^"(C,vp,range,newposAllocate);"
		      val t7 = indentt("bool test = "^testString^";\n")
		      val t8 = indenttt( "if(test){*tracker=cell;return {cell,newpos};}") 
		      val t9 = indentt("}")
		  in
		      if opt then String.concat([t1,t2,t3,t4,t5,t6,t7,t8,t9]) else ""
		  end
              val t7 = indent("for (int cell = 0;  cell < vL;  cell+=1) {")
              val t8 = indentt "getPoints(cell,nM,pM,C);\n"
              val t9 = indentt "newposTy newpos = "^fn_helpTranslateCoordinates(prefixM)^"(C,vp,range,newposAllocate);"
              val t10 = indentt("bool test = "^testString^";\n")
              val t11 = if opt then indenttt( "if(test){*tracker=cell;return {cell,newpos};}")
			else indentt("if(test){return {cell,newpos};}")
              val t12 = indentt("}")
              val t13 = indentt("return {-1,NULL};")
              val t14 = indent("}")
              val es = [t1,t2,tt, t3, t4, t5, t6,optCode, t7,t8, t9,t10,t11,t12,t13,t14]
          in
              es
          end

      fun makeBasisEvaluation{prefixME, sBasisFunctions, sBasisItter} =
              let
                  val t1 = ("inline "^float^" * "^fn_helpEvalBasis(prefixME)^"(const "^float^" *k, "^float^" *result){\n")
                  val tempFunc = (fn x =>
                                     indent(String.concat(
                                                 ["result[",
                                                  Int.toString(x),
                                                  "] = ",
                                                  List.nth(sBasisFunctions,x)
                                                  ,";\n"])))
                  val t2 = String.concat(List.map tempFunc sBasisItter)
                  val t3 = indent "return(result);\n}\n"
                           
                 (*val _  = print(String.concat["inside gen fem and creating make basise eval\n",t1,t2,t3])*)
              in
                  [t1,t2,t3]
              end
      fun foldKeepr f init [] = [init]
	| foldKeepr f init (x::xs) =
	  let
	      val next = foldKeepr f init xs
	      val xp = f (x ,(List.hd next))
	  in
	      xp :: next
	  end 	    


      (*Calculates the index we should a 1d array that is really a |access|d array*)
      fun arrayCalculate dims access =
	let
	    val prods =  List.rev(  foldKeepr (fn (x,y) => x*y) 1 (dims))
	    val acc = List.rev access
	    val sum =  List.foldr (fn (x,y) => x+y) 0 (List.map (fn x=> List.nth(prods,x)*List.nth(acc,x)) (List.tabulate((List.length(access)),fn x=> x)))
	in
	    sum
	end
      fun makeProbePhi{prefixME, nnodes} =
	let
	    val t1 = ("inline "^float^" "^fn_ProbePhi(prefixME)^"(brevTy a, newposTy b) {\n")
	    val t2 = indent ""^float^" result = 0;\n"
	    val t3 = indent ""^float^" *weights = a.data;\n"
	    val t4 = indent "int len = a.col;\n"
	    val t5 = indent "if (len ==0){return(0);}" (*Better error system!*)
	    val t6 = indent ""^float^" res["^(Int.toString(nnodes))^"] = {"^(String.concatWith "," (List.tabulate(nnodes,fn x => "0")))^"};\n"
	    val t7 = indent ""^float^" * basisAtPos = "^fn_helpEvalBasis(prefixME)^"(b,res);\n"
	    val t8 = indent "for(int itter = 0; itter < len; itter++){\n"

	    val t9 = indentt "result += res[itter]*weights[itter];\n\n\t}\n"
	    (*	    val t9 = indent "delete a.data;\n" (*This is stupid*)*)
	    val t10 = indent "return(result);\n}\n"
	in
	    [t1,t2,t3,t4,t6,t7,t8,t9,t10]
	end
      fun helpMakeEvaluateDerv prefix dlevel sdim dim shape namespace =
	let
	    val endArg = if shape then "){\n" else ",int start,int mult ){\n"
	    (*val namespace = "mesh_d2s_single"*)

	    val dervType = implaceArrayU dim dlevel
	    val sdims = List.map Int.toString (List.tabulate(dlevel,fn x => sdim))
	    val dims = List.map Int.toString (List.tabulate(dlevel,fn x => dim))

	    val ty = namespace^"::"^"tensor_ref_"^(String.concatWith "_" dims)
	    val t1 =  ty^" "^(fn_makeEvaluate prefix dlevel [])^"(NodeTy nodes, newposTy b, coordTy c,int cell,MappTy nM, FloatMapTy pM"^endArg
	    val t0 = indent "if(nodes.data == 0){return(0);}" (*Better error system*)
	    val t2 = indent (float ^ " H1" ^ dervType ^";\n ")
	    (* val t3 = fn_makePhiDerv(prefix)^"_"^Int.toString(dlevel)^"(H1, b, c->data);" (*Create D\Phi(F^{-1}(x))*) *)
						  (* val *)
	    val t3 = indent (if shape then ((fn_affineDerv prefix dlevel)^"(cell,nodes,b,c,nM,pM,H1,0,1);")
			     else ((fn_affineDerv prefix dlevel)^"(cell,nodes,b,c,nM,pM,H1,start,mult);"))
	    val t4 = indent "delete(b);"

	    val t5  = indent "return("^ty^"(("^float^"*) H1));\n}"

	in
	    [t1,t0,t2,t3,t5]
	end

      fun helpMakeEvaluate prefixME sdim dim dlevel shape =
	let
	    val endArg = if shape then "){\n" else ",int start,int mult ){\n"
								  
	    val t1 =  "inline "^float^" "^(fn_makeEvaluate prefixME 0 [])^"(NodeTy nodes, newposTy b, coordTy c"^endArg
	    val t2 = indent "if (nodes.data ==0){return(0);}"
	    val tt = indent "printf(\"translated to %f t %f\\n\",b[0],b[1]);"
	    val t3 = indent ""^float^" array["^Int.toString(sdim)^"] = {"^(String.concatWith "," (List.tabulate(sdim,fn x => "0")))^"};\n"
	    val t4 = indent (if shape then "brevTy weights = ProbeNodeC(nodes,c,array,0,1);"
			     else "brevTy weights = ProbeNodeC(nodes,c,array,start,mult);")
	    val t5 = indent ""^float^" result = "^fn_ProbePhi(prefixME)^"(weights,b);"
	    val t = indent "printf(\"%f\\)<=1.0e-10\\n\",result);"
	    val d = indent "delete(b);"
	    val t6 = indent "return(result);\n}\n"
	in
	    [t1,t2,t3,t4,t5,t6]
	end

      (* fun allCombsFromList lists = *)
      (* 	case lists *)
      (* 	 of [] => [] *)
      (* 	  | [a] => a *)
      (* 	  | [a,b] =>  *)
	    
      (*helpMakeEvaluate*)
      fun makeEvaluate({prefixME,sdim,dim, dlevel, shape,namespace}) =
	let
	    val d = dlevel
	    val shapeTest = shape = []
	    val res =
		if d= 0
		then helpMakeEvaluate prefixME sdim dim dlevel  shapeTest
		else helpMakeEvaluateDerv prefixME dlevel sdim dim shapeTest namespace
	    (*Use _data*)
	    val next =
		if shapeTest
		then []
		else
		    let
			(*Manufacture types*)
			val shapeLength = (List.length(shape))
			val completeOrder = shapeLength + d
			val shapeType = implaceArray shape shapeLength
			val dervTabulates = List.tabulate(dim,fn x => x)
			val dervPerms = perms_with_rep(dlevel,dervTabulates)
			val assignDervs = fn (startArray,startVar) =>
					     List.map (fn x =>
							  let
							      val extra = implaceArray x dlevel
							  in
							      indent (String.concat([
									       startArray,
									       extra,
									       " = ",
									       startVar,
									       extra,
									       ";"
										   
									   ]))
							  end ) dervPerms
					    
			val dervType = if d =0 then "" else implaceArrayU dim dlevel
			val completeType = shapeType^dervType
			val dims =  (List.tabulate(dlevel,fn x => dim))
		(* val namespace = "mesh_d2s_single"*)

			val ty = namespace^"::"^"tensor_ref_"^(String.concatWith "_" (List.map Int.toString (List.concat [shape,dims])))

			val args =
			    if d =0
			    then "(NodeTy nodes, newposTy b, coordTy c ){\n"
			    else "(NodeTy nodes, newposTy b, coordTy c,int cell,MappTy nM, FloatMapTy pM ){\n"
			val applyArgs =
			    if d = 0
			    then fn (i,s) => "(nodes, b, c, "^Int.toString(i)^", "^Int.toString(s)^")"
			    else fn (i,s) => "(nodes, b, c , cell, nM, pM,"^Int.toString(i)^", "^Int.toString(s)^")"
			val endArgs = if d = 0 then "" else "._data"

			val mult = List.foldr (fn (x,y) => x*y) 1 shape
			val dlevelMult = List.foldr (fn (x,y) => x*y) mult (List.tabulate(d, fn x => dim))
				     
			val t1 = ty^" "^(fn_makeEvaluate prefixME dlevel shape)^args
			val t0 = indent "if(nodes.data == 0){return(0);}" (*Better error system*)
			val t2 = indent (float ^"* P = new "^float^"["^Int.toString(dlevelMult)^"];")
			val t3 = indent (float ^" (&H)"^completeType^" = *reinterpret_cast<"^float^" (*)"^completeType^">(P);")
			val dimChecks = List.map (fn x => fn y => y < x) shape
			val dimCheck = fn list => (List.foldr (fn (x,y) => x andalso y) true
							      (List.map
								   (fn (x,y) => x(y)) (ListPair.zip(dimChecks,list))))
			val max = List.foldr (fn (x,y) => if x < y then y else x) 0 shape
			val perms =  perms_with_rep(shapeLength,List.tabulate(max,fn x => x))
			val handleVectorEntries = if d =0 then "" else " const"
			val ps = List.map (fn x =>
					      if dimCheck x
					      then
						  let
						      val xstring = String.concat(List.map Int.toString x )
						      val valName = if d = 0 then float^handleVectorEntries^" H"^((xstring))
								    else float^handleVectorEntries^" (&H"^((xstring))^")"^dervType
						      val arrTy = (implaceArray x shapeLength)
						      val start = arrayCalculate shape x
						      val standardEvalAction =
							  if d = 0
							  then String.concat(["H",arrTy," = ","H",xstring,";"])
							  else String.concat((assignDervs ("H"^arrTy,"H"^xstring)))
						      val standardEvalCast = if d = 0
									     then
										 ""
									     else
										 String.concat([ "*reinterpret_cast< const ",
												 float,
												 " (*)",
												 dervType,
												 ">"])
						      
						      
								      

						  in
						      
						      indent (String.concat([valName,
									     " = ",
									   
									     standardEvalCast,"(",
									     (fn_makeEvaluate prefixME d []),
									     (applyArgs (start,mult)),
									     endArgs,");",
									     standardEvalAction
									     
								   
							 ]))
						  end
						  
						  
					      else "") perms
			val ts = String.concat(ps)
			val t4 =indent "return("^ty^"(("^float^"*) H));\n}"
			val test = "printf(\"We have value %f,%f\\n\",H[0],H[1]);"
						  
							 
		    in
			[t1,t0,t2,t3,ts,t4]
		    end

	in
	    List.concat([res,next])
	end





      fun phiDervCode sdim dim dlevel s_derivativeInfoList header sepa sepb =
	let
	    val derivativeInfoList = s_derivativeInfoList
	    val dims = (List.tabulate(dlevel, fn x => dim))
	    val perms = perms_with_rep(dlevel, (List.tabulate(dim, fn x => x)))
	    val addrs = List.map (fn x => arrayCalculate dims x) perms
	    val polys = List.map (fn x => List.nth(derivativeInfoList,x)) addrs
	    val code = List.map (fn (x,y) =>
				    let
					val perm = x (*List.nth(perms,x)*)
					val poly = y(*List.nth(polys,x)*)
				    in
					indent header^(implaceArrayS perm dlevel sepa sepb)^" = "^poly^";\n"
				    end
				) (ListPair.zip (perms,polys))
	    val ts = String.concat(code)
	in
	    ts
	end

      fun makePhiDerv({prefix,sdim,dim,dlevel,s_derivativeInfoList}) =
	
	let
	    
	    val dervType = implaceArrayU dim dlevel
	    val t1 = "inline "^float^" "^fn_makePhiDerv(prefix)^"_"^Int.toString(dlevel)^"("^float^" H"^dervType^", const "^float^"* k, "^float^"* c){\n"
	    val ts = phiDervCode sdim dim dlevel s_derivativeInfoList "H" "[" "]"
	    val t2 = indent "return(0);\n}\n"
																				 

	in
	    [t1,ts,t2]
	end
      fun indexToPartial levelTab index =
	let
	    
	    val counts = List.map (fn x => List.length(List.filter (fn y => y=x) index)) levelTab
	    val str = "("^(String.concatWith "," (List.map Int.toString  counts))^")"
				 
	in
	    str
	end
      fun makeFDervCode gdim dim dlevel ht header sepa sepb =
	let
	    val dimTab = List.tabulate(dim, fn x => x)
	    val gdims = (List.tabulate(gdim, fn x => x))
	    val dims = (List.tabulate(dlevel, fn x => gdim))
	    val perms: int list list = perms_with_rep(dlevel, (List.tabulate(dlevel+1, fn x => x))) (*suspcious as all indicies plus F_{i} index*)
	    val findPartial :int list -> string = indexToPartial dimTab
	    fun entry coordString index =
	      let
		  val partialStrings = HashTable.lookup ht (findPartial index)
		  val vectorPartials = List.map (fn x =>
						    String.concat(["C[",coordString,"]","[",Int.toString(x),"]", (*order?*)
								   "*(",List.nth(partialStrings,x),")"])) gdims
		  val sum = List.foldr (fn (x,y)=> String.concat([x," + ",y])) "0.0" vectorPartials
	      in
		  sum
	      end
	    val code = List.map (fn x =>
				    let
					val partial : int list = List.tl x
					val s = Int.toString((List.hd x))
					val poly = entry s partial
							 
				    in
					indent header^(implaceArrayS x dlevel sepa sepb)^" = "^poly^";\n"
				    end) perms
	    val ts = String.concat(code)
	in
	    ts
	end
	    
      fun makeFDerv({prefix,gdim,dim,dlevel,g_derivativeInfoList}) =
	let
	    val ht : (string, string list) HashTable.hash_table = g_derivativeInfoList
	    val dervType = implaceArrayU gdim dlevel
	    val t1 = "inline "^float^" "^fn_makeFDerv(prefix)^"_"^Int.toString(dlevel)^"("^float^" H"^dervType^", "^float^"* k, "^float^"* c){\n"
	    val code = makeFDervCode gdim dim dlevel ht "H" "[" "]"

				      
	    val tfin = indent "return;}"

	in
	    [t1,code,tfin]
	end

      fun evalPhiTesnorAtIndex jName tenName indexCombinations dlevel dLevelTabulation index = 
	let
	    val J = jName
	    val T = tenName
	    (*
		   Suppose we want to make H[a][b][c] = \sum_{i,j,k} Phi[i][j][k]J[i][a]J[j][b]J[k][c]
		   1 : We do this first by making a function that produces all the needed Js for this map. We use
		      mkJ that given a [a,b,c] produces a map from [i,j,k] to [J[i][a],J[j][b],J[k][c]]
		   2 : We apply mkJ to an index/id 
                   3 : Using entry, we make one of the entries of the sums.
                   4 : we create all the entries by maping on all indicies 
			   
			   
	       
	       *)









	    val mkJ = List.map (fn y => fn x => jName^"["^Int.toString(x)^"]["^Int.toString(y)^"]") index
	    val mkJs : int list -> string list = fn id => List.map (fn x=>
								       let
									   val func = List.nth(mkJ,x)

									   val arg = (List.nth(id,x))

									   val res = func(arg)
								       in
									   res
								       end
								       
								   ) dLevelTabulation 
	    val entry : int list -> string = fn id => tenName^(implaceArray id dlevel)^"*"^(String.concatWith "*" (mkJs(id)))
	    val entires = List.map entry indexCombinations
	    val sum = String.concatWith "+" entires
			       
	in
	    sum
	end
      fun derivativeAffineCase({prefix,sdim,dim,dlevel}) =
	let
	    (*function Name:*)
	    val ty = implaceArrayU dim dlevel (*add name*)
	    val t1 = "void *"^(fn_affineDerv prefix dlevel)^"(int cell, NodeTy nodes, newposTy b, coordTy c, MappTy nM, FloatMapTy pM," ^(float ^ " H " ^ ty )^",int start,int mult){\n"
	    (* 	    (*setup: *)
	     (* t1: contains the array H where we store our results *)
	     (* t2: a place to store jacobian *)
	     (* t3: store the jacobian *)

	     (* t4: contains a place to store the tensor that is the derivative of Phi *)
	     (* t5: place to store the coordinates of a function *)
	     (* t6: store the coordinate of the function *)
	     (* t7: get Phi derivatives *)


	     (* 	     *) *)
             
             val _ = print("thinks dim in derivativeaffine case:"^Int.toString(dim)^"\nthinks sdim in derivativeaffine case:"^Int.toString(sdim))
	    val t2  = indent ""^float^" J["^(Int.toString(dim))^"]["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim, fn x => "{"^(String.concatWith "," (List.tabulate(dim,fn x => "0.0f")))^"}")))^"};\n"
	    val t3 = indent (fn_jIs(prefix)^"(J,nM,pM,cell);")
	    val t4 = indent float^" Phi" ^ ty ^";"
	    val t5 = indent ""^float^" array["^Int.toString(sdim)^"] = {"^(String.concatWith "," (List.tabulate(sdim,fn x => "0")))^"};\n"
	    val t6 = indent "brevTy weights = ProbeNodeC(nodes,c,array,start,mult);"
	    val t7 = indent (fn_makePhiDerv(prefix)^"_"^Int.toString(dlevel)^"(Phi,b,weights.data);\n")
	    (**)
	    val dLevelTabulate = (List.tabulate(dlevel, fn x => x))
	    val dimTabulate = List.tabulate(dim,fn x => x)
	    val perms = perms_with_rep(dlevel, dimTabulate)
	    val evalPoints = perms_with_rep(dlevel, dimTabulate)
					   
	    val _ = print ("["^(String.concatWith "," (List.map (fn y => "["^(String.concatWith "," (List.map Int.toString y))^"]") perms))^"]\n")
					   
	    val makeEntry : int list -> string = evalPhiTesnorAtIndex "J" "Phi" perms dlevel dLevelTabulate

	    val ts = String.concat(List.map (fn id => "\n\tH"^(implaceArray id dlevel)^" = " ^ (makeEntry id)^";\n") evalPoints)
	    val ret = indent "return(0);\n}\n"
	    val debugging = "printf(\"We have Phi: %f,%f.\\n We have J\\n: %f,%f\\n %f, %f\\n\",Phi[0],Phi[1],J[0][0],J[0][1],J[1][0],J[1][1]);\n"
	    
	in
	    [t1,t2,t3,t4,t5,t6,t7,ts,ret]
	end
	    

		      
 end
