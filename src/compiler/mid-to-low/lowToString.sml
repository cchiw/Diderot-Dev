(*  *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 *   Just created for debugging purpose
 * 
 *)

structure LowToString =
  struct

    structure IR=LowIR
    structure Op = LowOps
    structure Ty = LowTypes
    structure V = LowIR.Var
    structure StV = LowIR.StateVar

    fun rhsToString e=(case e
        of  IR.VAR x =>  String.concat["var",V.name x]
        | IR.LIT lit =>"lit"
        | IR.OP(rator, args) =>"op"^Op.toString rator 
        | IR.APPLY(f, args) =>"apply"
        | IR.CONS(ty, _) => "cons"
        | IR.EINAPP _=> "einapp"
        (*end case*))

    fun ASSIGNtoString(x,e)=String.concat[Ty.toString(V.ty x)," ",V.name x, "=ASN=",rhsToString e]
    fun SAVEtoString(x,e)=String.concat[StV.name x," =Save= ", V.name e]

    fun printNode x=(case x
        of IR.NULL                  => "Null"
        | IR.JOIN  _                => "Join"
        | IR.COND _                 => "Cond"
        | IR.COM       _            => "Comment"
        | IR.ASSIGN{pred,stm,succ}  => String.concat["ASSIGN ",ASSIGNtoString stm]
        | IR.MASSIGN _              => "MASSIGN"
        | IR.NEW  _                 => "New"
        | IR.SAVE  _                => "Save"
        | IR.EXIT _                 => "Exit"
        | _                         => "nothing"
        (*end x *))

    fun toStringRHS x  = (case V.binding x
        of vb => String.concat[
        "\n Found ", IR.vbToString vb,"\n"]
        (* end case *))
    fun useCount (LowIR.V{useCnt, ...}) = Int.toString(!useCnt)
    fun stringArgs(args) =
String.concat(List.map (fn e1 => (String.concat["\n\t",LowTypes.toString(V.ty(e1)),V.name(e1),"{", useCount(e1),"}"])) args)

    fun toStringAssgn(IR.ASSGN (x,  A))=(case A
        of  IR.OP(opss,args)=> String.concat [stringArgs[x],"==",Op.toString opss,
        " : ",(stringArgs args)]
        | IR.LIT(Literal.Int d)=> String.concat[V.toString  x,"= Literal_Int",IntInf.toString( d)]
        | IR.LIT lit=> String.concat[V.toString  x,"==...Literal", (Literal.toString lit)]
        | IR.CONS  _ =>  String.concat[(V.toString  x),"== Type:","--"]
        |  _ => String.concat[V.toString  x,"==","Etc",toStringRHS x]
        (*end case*))
    | toStringAssgn _ = raise Fail "Non-assignment operator"

    fun toStringAll(ty,rator)=""
(*        String.concat(["\n",Ty.toString ty,"--", toStringAssgn rator] )*)

  end