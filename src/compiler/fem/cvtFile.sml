(*convert file*)

structure cvtFile =
 
    struct

    structure JS = JSON
    structure JSP = JSONParser
    val basisParse = JSP.parseFile


    fun cvtFile filename =
          let
          (*We start by extracting json*)
              val path = filename

          (*val _  = print(String.concat["path:",path])*)


          val data = basisParse path
          (*This is a JS.JSON type thing and we create some utilites to read it.
           More complex ones will be needed when we come to higher derivatives as this will contain tensors*)
          fun findEntry (JS.OBJECT ls) entry =
            (case (List.find (fn (x,y) => x = entry) ls)
              of SOME((a,b)) => b
               | _ => raise Fail ("findEntry: "^entry)
            (* end case *))
            | findEntry _ entry = raise Fail "findEntry"
          fun checkInt (JS.INT(i)) = i
            | checkInt _ =  raise Fail "checkInt"
          fun checkString (JS.STRING(s)) = s
            | checkString _ = raise Fail "checkString"
          fun checkFloat (JS.FLOAT(r)) = r
            | checkFloat _ = raise Fail "checkFloat"
	  fun checkBool (JS.BOOL(b)) = b
	    | checkBool _ = raise Fail "checkBool"
					   
          fun uniformList f (JS.ARRAY(ls)) = List.map f ls
            | uniformList f _ = raise Fail "checkList"
          
          (*with all of this, we can get some key constants:*)
          val dim = Int.fromLarge (checkInt (findEntry data "dim"))
          val gdim = checkInt (findEntry data "gdim")
	  val sdim = checkInt (findEntry data "sdim")
	  val degree = Int.fromLarge (checkInt (findEntry data "degree")) (*could require more naming info in the future for files*)

						      
          val avgPos = uniformList checkFloat (findEntry data "k")
          val gBasisFunctions = uniformList checkString (findEntry data "g_basis_functions")
	  val isAffine = checkBool (findEntry data "isAffine")

          val gbasisJacobian : string list list =
              uniformList (fn x => uniformList checkString x) (findEntry (findEntry data "g_basis_dervs") "1")
          val testString = checkString (findEntry data "test")

          val sBasisFunctions : string list = uniformList checkString (findEntry data "s_basis_functions")

	  val nDervs = 3 (*Moderate me on the basis of degree to save space later*)
	  val sBasisDervs : string list list =
              List.map (fn y =>
	  	      ((fn x => uniformList checkString x) (findEntry (findEntry data "s_basis_dervs") y )))
	  	       (List.tabulate(nDervs, fn q => Int.toString(q+1)))


	  val ht : (string, string list) HashTable.hash_table =
	      HashTable.mkTable (HashString.hashString,op=) ((nDervs*dim)*50, Fail "Geometric derv not found")
				(*Better size hint!*)

        in
            (dim, (gdim,sdim,degree), avgPos, gBasisFunctions,gbasisJacobian, testString, sBasisFunctions,isAffine,sBasisDervs)
        end


  end
