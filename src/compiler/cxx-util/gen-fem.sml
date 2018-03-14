(* gen-tys-and-ops.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenFem : sig
    val mkInnerProd_false: {prefix:string,basisFunctionsLength:int} -> string list
    val makeTransformHelper :{prefix:string, dim:int,  basisFunctions:string list} -> string list
    val JI_Inverse : {prefix:string, dim:int,  avgPos:real list, colAcc:int list, basisJacobian:string list list} -> string list
    val jIs : {prefix:string, gdim : int, dim : int} -> string list
    val helpTranslateCoordinates:{prefix:string, dim:int, avgPos:real list,basisFunctions:string list} -> string list
    val wrapCell:{prefixE:string,prefixA:string, dim:int, testString:string, isAffine:bool, gdim: int} -> string list
    val makeBasisEvaluation :{prefix:string,  sBasisFunctions:string list, sBasisItter:int list} -> string list
    val makeProbePhi :{prefixME:string, nnodes:int} -> string list
    val helpMakeEvaluateDerv : {prefix : string,sdim : int, dim: int , dlevel : int, shapeTest : bool, namespace : string} -> string list
    val helpMakeEvaluate : {prefix : string,sdim : int, dim: int ,  shapeTest : bool, namespace : string} -> string list
    val helpMakeEvaluate_shape : {prefixA : string,sdim : int, dim: int ,  dlevel:int, shape : int list, namespace : string} -> string list
    val makePhiDerv : {prefix : string, sdim : int,dim : int, dlevel : int, s_derivativeInfoList : string list} -> string list
 val derivativeAffineCase : {prefixE : string ,prefixA : string, sdim : int , dim : int ,dlevel : int,isSca:bool} -> string list
end =  struct

structure RN = CxxNames
structure TS = TargetSpec
		   
    val float = "double" (*set me!*)
		
    (* function name *)
     val fn_makeTransformHelper = RN.fn_makeTransformHelper
     val fn_helpJIs = RN.fn_helpJIs
     val fn_jIs = RN.fn_jIs
     val fn_helpTranslateCoordinates = RN.fn_helpTranslateCoordinates
     val fn_makeFindCell = RN.fn_makeFindCell
     val fn_helpEvalBasis =  RN.fn_helpEvalBasis
     val fn_ProbePhi = RN.fn_ProbePhi
     val fn_makeEvaluate = RN.fn_makeEval
      val fn_makeEvaluateRange = RN.fn_makeEvalRange
     val fn_makePhiDerv = RN.fn_makePhiDerv
     val fn_makeFDerv = RN.fn_makeFDerv
     val fn_affineDerv = RN.fn_affineDerv

     val debug = false
     fun debMsg s =   if debug then "printf(\""^s^"\\n\");" else ""
     fun pointerStatus arr = if debug then "printf(\"Hi\\n\");printf(\"The array has value %f\\n\","^arr^"[0]);" else ""


     fun indent e = String.concat["\n\t",e]
     fun indentt e= String.concat["\n\t\t",e]
     fun indenttt e= String.concat["\n\t\t\t",e]


     (*J and C decleration helper!*)
     fun decC dim nnodes = "C["^Int.toString(dim)^"]["^Int.toString(nnodes)^"];"

 
     (*now we want to write some symbolic linear algebra that will help us out later on:*)
     (*This creates a vector of (v1Name[i]*v2name[i])_{i}*)
     fun dotProdHelp v1Name v2Name itter = List.map
                                     (fn x => let
                                     val a = concat(["[",Int.toString(x),"]"])
                                     in
                                     concat(["(",v1Name,a," * ", v2Name,a,")"])
                                     end) itter
     (*This converges the above vector to a dot product and then writes assign oper dotproduct where assign is some variable and oper is an operation like += or =*)
     fun dotProd v1Name v2Name itter assign oper =
     assign^oper^(List.foldr (fn (x,y) => concat([x," + ",y])) "0" (dotProdHelp v1Name v2Name itter) )^";\n"
     
 
     val tempGet = fn (x,row, basisJacobian) => List.nth(List.nth(basisJacobian,x),row)
 
     fun makeDot (row, col, colAcc,basisJacobian) =
        List.map (fn x => concat(["(C[",Int.toString(x),"][",Int.toString(row),"] * (",tempGet(x,col, basisJacobian),"))" ])) colAcc
 
    fun makeEntry (row, col,colAcc,basisJacobian) =
        indent ((concat(["J[",Int.toString(row),"][",Int.toString(col),"] = "]))^(List.foldr (fn (x,y) => concat([x," + ",y])) "0" (makeDot(row, col, colAcc,basisJacobian)))^";\n")

 

     (*These faciliate matrix multiplication so that if C is some matrix where each column is a coordinate and P a vector of polynomials, we can compute CP(k)*)
     fun rowToVector (i, rowAcc) = List.map (fn x => concat(["C[",Int.toString(x),"][",Int.toString(i),"]"])) rowAcc
     fun almostDot (i,rowAcc, basisFunctions) = (ListPair.zip ((rowToVector(i,rowAcc)), basisFunctions))
     fun component (i, sub, rowAcc, basisFunctions) =
       List.foldr (fn ((x,y),last) => String.concat[last," + (",x,")*(",y,")"]) sub (almostDot (i,rowAcc, basisFunctions))



     fun implaceArrayS dims level sepa sepb = concat(List.tabulate(level,fn x => sepa^Int.toString(List.nth(dims,x))^sepb))
     fun implaceArray dims level = concat(List.tabulate(level,fn x => "["^Int.toString(List.nth(dims,x))^"]"))
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
            
            
     (*create statement*)
     fun setEq(lhs, rhs) = indent(concat [lhs,"=", rhs,";"])
     fun setEqIndex (var, id, rhs) = indent(concat[var,"[",Int.toString(id), "]=",rhs,";"])
     fun mkCons(var,l,body,2)= setEq(concat["\tvec",(l), " ",var ],concat["__extension__ (vec",(l),"){", body,"}"])
       | mkCons(var,l,body,_)= setEq(concat["vec",(l), " ",var ],concat["__extension__ (vec",(l),"){", body,"}"])
      (*this generates an inline helper for makeTransform that already has C generated
         and has a modoff vector that allows us to subtract something from the result of CP(k)
       Modoff is added to make the inverse function theorem itteration easier to write*)
       
       
       fun name_innerProd(true, snl,prefix)= concat["innerprod_fast_", snl,"_",prefix]
      | name_innerProd(false, snl,prefix)= concat["innerprod_slow_", snl,"_",prefix]
    
    (*inner product between matrix and vector*)
    fun mkInnerProd_false{prefix, basisFunctionsLength}= let
        val snl = Int.toString(basisFunctionsLength)
        val t1= concat["\ninline ", float, " ",name_innerProd(false,snl,prefix),"(double** M, vec",(snl), " V, int i){"]
        val t2 = indent(concat["// inner product between M and V"])
        val tmp = List.tabulate(basisFunctionsLength, fn e=>  concat["(M[",Int.toString(e),"][i]*V[",Int.toString(e),"])"])
        val t3 = indent(concat["return ",String.concatWith"+" tmp,";"])
        val t4 = "\n}\n"
        in [t1,t2,t3,t4]
        end
    


(************************************************************** translate coords ***********************************************************)
    fun makeTransformHelper{prefix, dim, basisFunctions} =
          let
          
              val vnewpos = "k"
              val vshift = "shift"
              val t1 = concat["inline double * ",fn_makeTransformHelper(prefix),"(",float,"** C, ",float,"* ",vnewpos,", const ",float," *",vshift,", ",float," temp[2]){"]
	      val td1 = debMsg "Enetered makeTransformHelper"
             val nl = List.length(basisFunctions)
             (*matrix multplication*)
             val nl = List.length(basisFunctions)
             val snl = Int.toString(nl)
             val tmp = List.tabulate(nl, fn x => x)
             val bl = String.concatWith"," basisFunctions
             val t2 = concat (["\n\t // dim:",  Int.toString(dim)," nl:", snl])
             
             (*creates vector to generalize function*)
             val ta = mkCons("basisfn", snl, bl,1)
             val tb = indent(concat["// inner product between C and  bsf"])
             fun fa (x) = concat["0 - ",vshift,"[",x,"]+",name_innerProd(false,snl,prefix),"(C,basisfn,",x,")"]
             val t3a = List.tabulate(dim, fn x => setEqIndex("temp",x, fa(Int.toString(x))))
             
             (*point wise*)
             fun f (x) = component (x, concat["0-",vshift,"[", Int.toString(x),"]"], tmp, basisFunctions)
             val t3 = List.tabulate(dim, fn x => setEqIndex("//temp",x, f(x)))
	     val td2 = debMsg "Existed makeTransformHelper"
              val t5 = indent("return temp;\n}\n")

          in
               [t1,td1,t2,ta,tb]@t3a@t3@[td2,t5]
          end


    (* gets inverse jacobian- goes with JI_Inverse*)
    (*See http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html for inverse formulas*)
    (*entries t11*t22-t12*t21 are created from JI-Inverse*)
    fun invJacob 2  =
         let
                val name = indent ("//divide components by det")
                val det = setEq (concat["",float," det"],"1.0/(t11*t22-t12*t21)")
                val m1 = setEq("J[0][0]","t22*det")
                val m2 = setEq("J[0][1]","0 - t12*det")
                val m3 = setEq("J[1][0]","0 - t21*det")
                val m4 = setEq("J[1][1]","t11*det")
		val td = debMsg "Exiting inverse Jacobian Call"	
                val t6 = indent "return(0);\n}\n"
            in
                [name, det,m1,m2,m3,m4,td,t6]
            end
        | invJacob 3  =

            let
                val name = indent ("//divide components by det")
                val det =setEq (concat["",float," det"],"1.0/(t11*t22*t33 +  t21*t32*t13 + t31*t12*t23 - t11*t32*t23 - t31*t22*t13 - t21*t12*t33)")
                (*computation of inverse matrix*)
                val m1 = setEq("J[0][0]","det*(t22*t33-t23*t32)")
                val m2 = setEq("J[0][1]","det*(t13*t32-t12*t33)")
                val m3 = setEq("J[0][2]","det*(t12*t23-t13*t22)")
                val m4 = setEq("J[1][0]","det*(t23*t31-t21*t33)")
                val m5 = setEq("J[1][1]","det*(t11*t33-t13*t31)")
                val m6 = setEq("J[1][2]","det*(t13*t21-t11*t23)")
                val m7 = setEq("J[2][0]","det*(t21*t32-t22*t31)")
                val m8 = setEq("J[2][1]","det*(t12*t31-t11*t32)")
                val m9 = setEq("J[2][2]","det*(t11*t22-t12*t21)")
		val td = debMsg "Exiting inverse Jacobian Call"			  
                val t6 = indent"return(0);\n}\n"
            in
               [name, det,m1,m2,m3,m4,m5,m6,m7,m8,m9,td,t6]
            end
        | invJacob dim = raise Fail (concat["Dim",Int.toString(dim)," not implemented"])


    fun JI_Inverse{prefix, dim, avgPos, colAcc, basisJacobian}  =
          let
          val cvtTy =concat["[",(Int.toString(dim)),"][",(Int.toString(dim)),"]"]
          val rowAcc = List.tabulate(dim, fn x => x)
          val dimDimTuples = List.concat (List.tabulate(dim, fn x => (List.map (fn y => (x,y)) rowAcc)))
          val nl = length(basisJacobian)
          val snl = Int.toString(nl)
          val sml = Int.toString(length(List.nth(basisJacobian,0)))
          fun vname (r, c) = concat [""^float^" t",Int.toString(r+1),Int.toString(c+1)]
          (*header and beginning *)
          val t1 = "inline void *"^fn_helpJIs(prefix)^"("^float^" **C,"^float^" J"^cvtTy^"){\n"
	  val td1 = debMsg "Entering helpJIs"
          val t2 = indent ""^float^" k["^Int.toString(dim)^"];"
          val t3 = List.map (fn (x,y) =>  setEqIndex("k",x,Real.toString(y))) (ListPair.zip (rowAcc, avgPos))
          val t4 = indent(concat["//length(col):",Int.toString(length(colAcc))," basisJacob:", snl,"x",sml])
                    
        (*load to vectors*)
          fun reOrganize([],[], rest) = List.rev rest
          | reOrganize(e1::es,[], rest) = reOrganize(es, e1,rest)
          | reOrganize(e1::es,f1::fs, rest) = reOrganize(es, fs, (f1::e1)::rest)
          fun iter(row,[])= []
          | iter(row, e1::es) =
            mkCons("tmp"^Int.toString(row+1),Int.toString(nl),String.concatWith"," e1,1)::iter(row+1,es)
          val entriesB= iter(0, reOrganize(basisJacobian,[],[]))

          fun makeTempEntry (r, c) = concat[name_innerProd(false,snl,prefix),"(C,tmp",Int.toString(c+1),",",Int.toString(r),")"]
          (*computations for entries of the jacobianL*)
          val entriesA = List.map (fn (r,c)=> setEq(vname(r,c),makeTempEntry(r,c))) dimDimTuples
          
          
          (* original *)
          fun body (r, c) = List.foldr (fn (x,y) => concat([x," + ",y])) "0" (makeDot(r, c, colAcc,basisJacobian))
          fun makeTempEntry (r, c) = setEq("//"^vname(r,c), body(r,c))
          (*computations for entries of the jacobianL*)
          val entries = List.map (fn (r,c)=> makeTempEntry(r,c)) dimDimTuples
          
          (*inverse jacobian*)
          val invJ = invJacob(dim)

          in
          [t1,td1,t2,t4]@t3@entriesB@entriesA@entries@invJ
          end
          
          
    fun jIs{prefix,gdim,dim} =
      let
          val cvtTy =concat["[",(Int.toString(dim)),"][",(Int.toString(dim)),"]"]
	  val t1 = "inline void *"^fn_jIs(prefix)^"("^float^" J"^cvtTy^",MappTy nM, FloatMapTy pM,int cell){"
	  val td1 = debMsg "Entering jIs"
	  val t2 = setEq(""^float^" *C["^(Int.toString(gdim))^"]","{"^(String.concatWith "," (List.tabulate(gdim,fn x => "0")))^"}")
          val t3 = indent "getPoints(cell,nM,pM,C);"
	  val t4 = indent (fn_helpJIs(prefix)^"(C,J);")
	  val td2 = debMsg "Exiting jIs"
	  val t5 = indent "return(0);}\n"
	  
      in
	  [t1,td1,t2,t3,t4,td2,t5]
      end


      (*We now bring everything togeather into the translate coordinates loop. 
       This is where the inverse function theorem itteration lives*)
 fun helpTranslateCoordinates{prefix, dim, avgPos,basisFunctions} =
   let
            val rowAcc = List.tabulate(dim, fn x => x)
            val vnewpos = "k"
            val vshift = "shift"


            
	    val jInit = ""^float^" J["^(Int.toString(dim))^"]["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim, fn x => "{"^(String.concatWith "," (List.tabulate(dim,fn x => "0.0f")))^"}")))^"};"
            val t1 = concat["inline ",float," * ",fn_helpTranslateCoordinates (prefix),"(",float," **C, const  ",float," * ",vshift,", int itter, ",float, " ",vnewpos,"[",(Int.toString(dim)),"]){"]
	    val td1 = debMsg "Entering helperTranslateCoordinates"
              val t3 = concat(List.map (fn (x,y) => setEq(concat[vnewpos,"[",Int.toString(x),"]"], Real.toString(y))) (ListPair.zip (rowAcc, avgPos)))
              val v4 = ( "bool notConverged", "true")
              val v5 = ("int i"," 0")
              val v6 = (concat[float," change[",(Int.toString(dim)),"]"],"{0.0}")
              val v7 = (concat[float," random[",Int.toString(dim),"]"],"{ 0.0 }")
              val v8 = (concat[float," temp[",Int.toString(dim),"]"],"{ 0.0 }")
              val t4 = List.map (fn (lhs,rhs)=> setEq(lhs,rhs)) [v4,v5,v6,v7,v8]
              val t7 = indent (concat["//",float," * temp;\n\t"])
              val t8 = indent (fn_helpJIs(prefix)^"(C,J);")
              val t9 = indent "while ((i < itter) && notConverged){"
                  
             (*inside while loop*)
              val t10 = indentt (concat["//temp = ",fn_makeTransformHelper(prefix),"(C,",vnewpos,", ",vshift,",random);"]) (*a = (F(newpos)-x_{0})*)
              (*This preforms *change = (DF(k))^{-1}a*)
              
              (*inner product between C and bassis fn*)
              val nl = List.length(basisFunctions)
              (*matrix multplication*)
              val snl = Int.toString(nl)
              val tmp = List.tabulate(nl, fn x => x)
              val bl = String.concatWith"," basisFunctions
              val tt2 = concat (["\n\t\t // dim:",  Int.toString(dim)," nl:", snl])
              (*creates vector to generalize function*)
              val tta = mkCons("basisfn", snl, bl,2)
              val ttb = indent(concat["\t// inner product between C and  bsf"])
              fun fa (x) = concat["0 - ",vshift,"[",x,"]+",name_innerProd(false,snl,prefix),"(C,basisfn,",x,")"]
              val tt3a = List.tabulate(dim, fn x => setEqIndex("\ttemp",x, fa(Int.toString(x))))
              (*point wise*)
              fun f (x) = component (x, concat["0-",vshift,"[", Int.toString(x),"]"], tmp, basisFunctions)
              val tt3 = List.tabulate(dim, fn x => setEqIndex("\t//temp",x, f(x)))
              val ttransform = [tt2,tta,ttb]@tt3a@tt3


              (*rest of while loop*)
              val snl =Int.toString(dim)
              val t11 = 
                  concat(List.map
                    (fn x =>
                        indentt(dotProd
                             (concat(["J[",Int.toString(x),"]"]))
                             "temp"
                             rowAcc
                             (concat(["change[",Int.toString(x),"]"]))
                             "="))
                    rowAcc )
              val t12 = indentt ""^float^" test = "^(dotProd "change" "change" rowAcc "" "")
              val t13 = indentt"if(test < 1e-12*1e-12){notConverged=false;}"
              val t14 = concat(List.map (fn x =>
                   let
                       val a = Int.toString(x)
                   in
                       concat(["\n\t\t",vnewpos,"[",a,"] -=","change[",a,"];"])
                       (*this performs newpos = newpos - change = newpos - (DF(k))^{-1}(F(newpos)-x_{0}) *)
                   end) rowAcc)
              val t15 = indentt "i+=1;\n\t}"
	      val td2 =   debMsg "Exiting helpTranslateCoordinates"
              val t16 = indent (concat["return ",vnewpos,";\n}\n"])

          in
              [t1,td1,t3]@t4@[t7,jInit,t8,t9,t10]@ttransform@[t11,t12,t13,t14,t15,td2,t16]
          end
          
     (************************************************************** wrap cell ***********************************************************)
      fun wrapCell{prefixE, prefixA, dim,testString,isAffine,gdim} =
        let
	    val opt =  false

              val t0 = fn_makeFindCell prefixA
              val t1 = "fcTy "^t0^" (const "^float^"* vp,  int32_t vL, MappTy nM, FloatMapTy pM, optStruct opt){"
	      val td1 =  debMsg "Entering wrapCell"
	      val tt = if opt then indent "int32_t * tracker = opt.tracker;\n\t if(*tracker > vL || *tracker < 0){*tracker=0;};"
		       else ""
	      val t = indent "printf(\"print abs(%f * %f-\",vp[0],vp[1]);"
              val t3 = indent("")
              val t4 = indent(if isAffine then "int range = 1;" else "int range =16;")
              val t5 = indent(""^float^" *C["^(Int.toString(gdim))^"] = {"^(String.concatWith "," (List.tabulate(gdim,fn x => "0")))^"};")
	      val t6 = indent ""^float^" * newposAllocate = new "^float^"["^(Int.toString(dim))^"]();"
	      val optCode =
		  let
		      val t1 = indent "int lastCell = *opt.tracker;"
		      val t2 = indent "int32_t * nbrs = &opt.Nbrs[vL*lastCell];"
		      val t3 = indent("for (int cellIndex = 0;  cellIndex < vL;  cellIndex+=1) {")
		      val t4 = indentt "int32_t cell = nbrs[cellIndex];"
		      val t5 = indentt "getPoints(cell,nM,pM,C);"
		      val t6 = indentt "newposTy newpos = "^fn_helpTranslateCoordinates(prefixE)^"(C,vp,range,newposAllocate);"
		      val t7 = indentt("bool test = "^testString^";")
		      val t8 = indenttt( "if(test){*tracker=cell;return {cell,newpos};}") 
		      val t9 = indentt("}")
		  in
		      if opt then concat([t1,t2,t3,t4,t5,t6,t7,t8,t9]) else ""
		  end
              val t7 = indent("for (int cell = 0;  cell < vL;  cell+=1) {")
              val t8 = indentt "getPoints(cell,nM,pM,C);"
              val t9 = indentt "newposTy newpos = "^fn_helpTranslateCoordinates(prefixE)^"(C,vp,range,newposAllocate);"
              val t10 = indentt("bool test = "^testString^";")
              val t11 = if opt then indenttt( "if(test){*tracker=cell;return {cell,newpos};}")
			else indentt("if(test){" ^ (debMsg "Exiting wrapCell with found cell") ^ (pointerStatus "newpos") ^ "return {cell,newpos};}")
              val t12 = indentt("}")
              val t13 = indentt("" ^ (debMsg "Exiting wrapCell without found cell") ^ "return {-1,NULL};")
              val t14 = indent("}")
              val es = [t1,td1,tt, t3, t4, t5, t6,optCode, t7,t8, t9,t10,t11,t12,t13,t14]
          in
              es
          end
          
(************************************************************** makeProbePhi  ***********************************************************)
    fun makeProbePhi{prefixME, nnodes} =
        let
            val t1 = ("inline "^float^" "^fn_ProbePhi(prefixME)^"(brevTy a, newposTy pos) {\n")
	    val td1= debMsg "Entering ProbePhi" ^ (pointerStatus "pos")
          val t2 = setEq(concat[float," result "],"0")
          val t3 = setEq(concat[float," *weights"]," a.data")
          val t4 = setEq("int len ", " a.col")
          val t6 = setEq(concat[float," base["^(Int.toString(nnodes))^"]"],"{"^(String.concatWith "," (List.tabulate(nnodes,fn x => "0")))^"}")
          val t7 = indent (concat[fn_helpEvalBasis(prefixME),"(pos,base);"])
          val t8 = indent "for(int i = 0; i < len; i++){"
          val t9 = indentt "result += base[i]*weights[i];\n\t}"
	  val td2= debMsg "Exiting ProbePhi"^(pointerStatus "pos")
          val t10 = indent "return(result);\n}"
        in
          [t1,td1,t2,t3,t4,t6,t7,t8,t9,td2,t10]
        end
        
        
(************************************************************** make basis eval ***********************************************************)
      fun makeBasisEvaluation{prefix,  sBasisFunctions, sBasisItter} =
              let
                  val t1 = concat["inline void ",fn_helpEvalBasis(prefix),"(const ",float," *k, ",float," *result){\n"]
		  val td1 = (debMsg "Entering helpEvalbasis")^(pointerStatus "k")
                  val tempFunc = (fn x => setEq(
                    concat["result[",Int.toString(x),"] "], List.nth(sBasisFunctions,x)
                    ))
                  val t2 = concat(List.map tempFunc sBasisItter)
		  val td2 = (debMsg "Exiting helpEvalbasis")^(pointerStatus "k")
                  val t3 = indent "return ;\n}\n"
              in
                  [t1,td1,t2,td2,t3]
              end
              
    (*makePhiDerv*)
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
          val ts = concat(code)
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
              
(************************************************************** mkeval helper ***********************************************************)
    fun helpMakeEvaluateDerv{prefix, dlevel, sdim, dim, shapeTest, namespace} =
        let
          
          val dervType = implaceArrayU dim dlevel
          val sdims = List.map Int.toString (List.tabulate(dlevel,fn x => sdim))
          val dims = List.map Int.toString (List.tabulate(dlevel,fn x => dim))
          
          val ty = namespace^"::"^"tensor_ref_"^(String.concatWith "_" dims)
          val t0 =  if shapeTest
                    then ty^" "^(fn_makeEvaluate prefix dlevel [])^"(NodeTy nodes, newposTy b, coordTy c,int cell,MappTy nM, FloatMapTy pM"^"){\n"
                    else ty^" "^(fn_makeEvaluateRange prefix dlevel [])^"(NodeTy nodes, newposTy b, coordTy c,int cell,MappTy nM, FloatMapTy pM"^",int start,int mult ){\n"
          val t1 = indent "if(nodes.data == 0){return(0);}" (*Better error system*)
          val t2 = indent (float ^ " H1" ^ dervType ^";\n ")
          val t3 = indent (if shapeTest 
                        then ((fn_affineDerv prefix dlevel shapeTest)^"(cell,nodes,b,c,nM,pM,H1,0,1);")
                        else ((fn_affineDerv prefix dlevel shapeTest)^"(cell,nodes,b,c,nM,pM,H1,start,mult);"))
          val t4 = indent "delete(b);"
          val t5  = indent "return("^ty^"(("^float^"*) H1));\n}"
              
        in
            [t0,t1,t2,t3,t5]
        end
              
    fun helpMakeEvaluate {prefix,sdim,dim, shapeTest,namespace} =
        let

          val t1 = if shapeTest
                    then  "inline "^float^" "^(fn_makeEvaluate prefix 0 [])^"(NodeTy nodes, newposTy b, coordTy c"^"){\n"
                   else  "inline "^float^" "^(fn_makeEvaluateRange prefix 0 [])^"(NodeTy nodes, newposTy b, coordTy c"^",int start,int mult ){\n"
	  val td1 = (debMsg "Entering helpMakeEvaluate")^((pointerStatus "b"))
          (*val stupid = if dim =2 then "const "^float^" arr[2] = {b[0],b[1]};" else "const "^float^" arr[3] = {b[0],b[1],b[2]};"*)
          val t2 = indent "if (nodes.data ==0){return(0);}"
          val tt = indent "printf(\"translated to %f t %f\\n\",b[0],b[1]);"
          val t3 = indent ""^float^" array["^Int.toString(sdim)^"] = {"^(String.concatWith "," (List.tabulate(sdim,fn x => "0")))^"};\n"^((pointerStatus "b"))
          val t4 = indent (if (shapeTest )
                    then "brevTy weights = ProbeNodeC(nodes,c,array,0,1);"
                    else "brevTy weights = ProbeNodeC(nodes,c,array,start,mult);") ^((pointerStatus "b"))
          val t5 = indent ""^float^" result = "^fn_ProbePhi(prefix)^"(weights,b);"
          val t = indent "printf(\"%f\\)<=1.0e-10\\n\",result);"
	  val td2 = (debMsg "Exit helpMakeEvaluate")^((pointerStatus "b"))
          val t6 = indent "return(result);\n}\n"
        in
            [t1,td1,t2,t3,t4,t5,td2,t6]
        end
                  

(************************************************************** mkeval ***********************************************************)
    fun mkps(d,shape,prefixA, dim, dervType) = let
        val handleVectorEntries = if d =0 then "" else " const"
        val dimChecks = List.map (fn x => fn y => y < x) shape
        val dimCheck = fn list => (List.foldr (fn (x,y) => x andalso y) true
            (List.map
            (fn (x,y) => x(y)) (ListPair.zip(dimChecks,list))))
        val shapeLength = (List.length(shape))
        val max = List.foldr (fn (x,y) => if x < y then y else x) 0 shape
        val perms =  perms_with_rep(shapeLength,List.tabulate(max,fn x => x))
        val mult = List.foldr (fn (x,y) => x*y) 1 shape
        val dervTabulates = List.tabulate(dim,fn x => x)
        val dervPerms = perms_with_rep(d,dervTabulates)
        val assignDervs = fn (startArray,startVar) =>
            List.map (fn x =>
                let
                    val extra = implaceArray x d
                in
                    setEq(concat [startArray,extra],concat[startVar,extra])
            end ) dervPerms
         
        fun f x =
            let
                val xstring = concat(List.map Int.toString x )
                val arrTy = (implaceArray x shapeLength)
                val start = arrayCalculate shape x
                (*dependent on diff*)
                val valName = float^handleVectorEntries^" H"^((xstring))
                val standardEvalAction = setEq(concat["H",arrTy],concat["H",xstring])
                val standardEvalCast = ""
                val applyArgs = fn (i,s) => "(nodes, b, c, "^Int.toString(i)^", "^Int.toString(s)^")"
                val endArgs = ""
              in
                  indent (concat([valName," = ",
                    standardEvalCast,"(",(fn_makeEvaluateRange prefixA d []), (applyArgs (start,mult)),endArgs,");",standardEvalAction
                  ]))
              end
        (*derivative case*)
        fun f_deriv x =
            let
                val xstring = concat(List.map Int.toString x )
                val arrTy = (implaceArray x shapeLength)
                val start = arrayCalculate shape x
                (*dependent on diff*)
                val valName = float^handleVectorEntries^" (&H"^((xstring))^")"^dervType
                val standardEvalAction = concat((assignDervs ("H"^arrTy,"H"^xstring)))
                val standardEvalCast = concat([ "*reinterpret_cast< const ",float, " (*)",dervType, ">"])
                val applyArgs = fn (i,s) => "(nodes, b, c , cell, nM, pM,"^Int.toString(i)^", "^Int.toString(s)^")"
                val endArgs = "._data"
            in
                indent (concat([valName," = ",
                    standardEvalCast,"(",(fn_makeEvaluateRange prefixA d []), (applyArgs (start,mult)),endArgs,");",standardEvalAction
                ]))
            end

            
        val g = if(d=0) then f else f_deriv
        in
            List.map (fn x=> if dimCheck(x) then g(x) else "") perms
        end
        
     (* header to eval function *)
      fun getEvalHeader(d,prefixA, shape,dim,namespace) =
        let
            val dims =  (List.tabulate(d,fn x => dim))
            val ty = concat[namespace,"::","tensor_ref_",(String.concatWith "_" (List.map Int.toString (List.concat [shape,dims])))]
            val args =
                if d =0 then "(NodeTy nodes, newposTy b, coordTy c ){\n"
                else "(NodeTy nodes, newposTy b, coordTy c,int cell,MappTy nM, FloatMapTy pM ){\n"
      
            val mult = List.foldr (fn (x,y) => x*y) 1 shape
            val dlevelMult = List.foldr (fn (x,y) => x*y) mult (List.tabulate(d, fn x => dim))
      
        in concat[ty," ",(fn_makeEvaluate  prefixA d shape),args] end
      
      
      (*called when non-scalar shape*)
      fun helpMakeEvaluate_shape{prefixA,sdim,dim, dlevel, shape,namespace} =
	let
	    val d = dlevel
                        (*statements*)
            val t0 = getEvalHeader(dlevel,prefixA, shape,dim,namespace)
	    val td1 = (debMsg "Entering helpMakeEvaluate_shape" )^((pointerStatus "b"))
            val t1 = indent "if(nodes.data == 0){return(0);}"
            (*statement 2*)
            val mult = List.foldr (fn (x,y) => x*y) 1 shape
            val dlevelMult = List.foldr (fn (x,y) => x*y) mult (List.tabulate(d, fn x => dim))
            val t2 = indent (float ^"* P = new "^float^"["^Int.toString(dlevelMult)^"];")
            (*Manufacture types*)
            val shapeLength = (List.length(shape))
            val shapeType = implaceArray shape shapeLength
            val dervType = if d =0 then "" else implaceArrayU dim dlevel
            val completeType = shapeType^dervType
            val t3 = indent (float ^" (&H)"^completeType^" = *reinterpret_cast<"^float^" (*)"^completeType^">(P);")
            val t4 = concat(mkps(d,shape,prefixA, dim, dervType))
            val dims =  (List.tabulate(dlevel,fn x => dim))
            val ty = concat[namespace,"::","tensor_ref_",(String.concatWith "_" (List.map Int.toString (List.concat [shape,dims])))]
	    val td2 = (debMsg "Exit helpMakeEvaluate_shape")^((pointerStatus "b"))
            val t5 =indent (concat["return(",ty,"((",float,"*) H));\n}"])
            
                                      
        in
            [t0,td1,t1,t2,t3,t4,td2,t5]
        end

            
        
 (************************************************************** derivative code ***********************************************************)
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
      fun derivativeAffineCase({prefixA,prefixE, sdim,dim,dlevel,isSca}) =
	let
	    val ty = implaceArrayU dim dlevel (*add name*)
            (* t1: contains the array H where we store our results *)
	    val t1 = "void *"^(fn_affineDerv prefixA dlevel isSca)^"(int cell, NodeTy nodes, newposTy b, coordTy c, MappTy nM, FloatMapTy pM," ^(float ^ " H " ^ ty )^",int start,int mult){\n"
            (* t2: a place to store jacobian *)
	    val t2  = indent ""^float^" J["^(Int.toString(dim))^"]["^(Int.toString(dim))^"] = {"^(String.concatWith "," (List.tabulate(dim, fn x => "{"^(String.concatWith "," (List.tabulate(dim,fn x => "0.0f")))^"}")))^"};"
            (* t3: store the jacobian *)
	    val t3 = indent (fn_jIs(prefixE)^"(J,nM,pM,cell);")
            (* t4: contains a place to store the tensor that is the derivative of Phi *)
	    val t4 = indent float^" Phi" ^ ty ^";"
             (* t5: place to store the coordinates of a function *)
	    val t5 = indent ""^float^" array["^Int.toString(sdim)^"] = {"^(String.concatWith "," (List.tabulate(sdim,fn x => "0")))^"};"
             (* t6: store the coordinate of the function *)
	    val t6 = indent "brevTy weights = ProbeNodeC(nodes,c,array,start,mult);"
            (* t7: get Phi derivatives *)
	    val t7 = indent (fn_makePhiDerv(prefixA)^"_"^Int.toString(dlevel)^"(Phi,b,weights.data);")

	    val dLevelTabulate = (List.tabulate(dlevel, fn x => x))
	    val dimTabulate = List.tabulate(dim,fn x => x)
	    val perms = perms_with_rep(dlevel, dimTabulate)
	    val evalPoints = perms_with_rep(dlevel, dimTabulate)
	    val makeEntry : int list -> string = evalPhiTesnorAtIndex "J" "Phi" perms dlevel dLevelTabulate
	    val ts = concat(List.map (fn id => "\n\tH"^(implaceArray id dlevel)^" = " ^ (makeEntry id)^";\n") evalPoints)
	    val ret = indent "return(0);\n}\n"
	    val debugging = "printf(\"We have Phi: %f,%f.\\n We have J\\n: %f,%f\\n %f, %f\\n\",Phi[0],Phi[1],J[0][0],J[0][1],J[1][0],J[1][1]);\n"
	    
	in
	    [t1,t2,t3,t4,t5,t6,t7,ts,ret]
	end
	    

		      
 end
