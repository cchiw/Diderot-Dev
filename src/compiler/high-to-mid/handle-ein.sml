(* handle-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure HandleEin : sig

    val expand : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assignment list

  end = struct
   
    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR

    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt

    fun expand (lhs, ein, args) = let
        (*DEBUG*)
         fun ll ([],cnt) = ""
           | ll (a1::args,cnt) = String.concat["\n\t", Int.toString(cnt),"_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1),",", ll(args,cnt+1)]
    val _ =  "strting to expand"

         val _ = (String.concat["\n\n  **************  **************  **************  expand ***********\n:",MidIR.Var.name(lhs),"="])

            (*"-",ll(args,0),"\n\n"])*)
        (* ************** distribute and push Summation*********** *)
          val ein' = EinSums.transform ein
         val newbies = [(lhs, DstIR.EINAPP(ein', args))]

            fun prntNewbies(newbies, id) = let
                val _ = (id)
                val _=  List.map (fn (lhs,DstIR.EINAPP(e,a))=> (String.concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs)," ",DstIR.Var.name(lhs),"=",EinPP.toString(e) ,"-",ll(a,0)])  | _ =>  "") newbies
               in  "done ing"
                end

          fun iter([], ys) = ys
            | iter(e1::es, ys) = let
            val (lhs, DstIR.EINAPP(e, a)) = e1
            val y1 = FloatEin.transform (lhs, e, a)
            in
                iter(es, ys@y1)
            end
        val _ = "\nbefore split"
        val _ = prntNewbies(newbies, "\n\n\n pre splitting ")
        (* **************** split phase ************* *)

        val newbies =iter(newbies, [])
        val _ = prntNewbies(newbies, "\n\n\npost floatx1")
        val newbies =iter(newbies, [])
        val newbies =iter(newbies, [])
        val _ = prntNewbies(newbies, "\n\n\npost floatx2")
        (* **************** expand-fem ************* *)
        (*val _ =  "\n\n\nbefore translating fields"*)
        val _ = "about to translate field"
        val newbies  = List.foldr (fn (e,acc)=>  translateField.transform(e)@acc ) []  newbies
        (*val _ = prntNewbies(newbies, "\n\n\npost transform fields")*)
        val _ = "\n\n--------------\n"
        (* ************** ProbeEIN *********** *)
        val avail = AvailRHS.new()
          val _ = List.app (ProbeEin.expand avail) (newbies);
          val stmts = List.rev (AvailRHS.getAssignments avail)
          val asgn = List.map DstIR.ASSGN stmts
          in
            asgn
          end

  end (* HandleEin *)
