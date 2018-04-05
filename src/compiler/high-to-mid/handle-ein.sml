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
    structure ScanE = ScanEin




    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt
    fun ll ([],cnt) = ""
    | ll (a1::args,cnt) = String.concat["\n\t", Int.toString(cnt),"_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1),",", ll(args,cnt+1)]

    fun prntNewbies(newbies, id) = let
        val _ = (id)
        in  List.map (fn (lhs,DstIR.EINAPP(e,a))=> (concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs)," ",DstIR.Var.name(lhs),"=",EinPP.toString(e) ,"-",ll(a,0),"---->"])  | _ => "") newbies
        end
    val flagmatch = true

    fun expand (lhs, ein, args) = let
        val _ = ("\n\n   ***********\n")
         val _ = (String.concat["\n\n   expand ***********\n:",MidIR.Var.name(lhs),"=", EinPP.toString(ein),"-",ll(args,0)])
        val sliceFlag = Controls.get Ctl.sliceFlag
        val fullSplitFlag = Controls.get Ctl.fullSplitFlag
        val replaceFlag =  Controls.get Ctl.replaceFlag

        (* ************** distribute and push Summation*********** *)

          val ein' = EinSums.transform ein
        val newbie = (lhs, DstIR.EINAPP(ein', args))

        (* ************** Scan and rewrite *********** *)
        val readIR = Controls.get Ctl.readEin
        val _ = if(readIR) then ScanE.readIR_Single(newbie,"tmp-High") else 1

        (* **************** split phase ************* *)
          fun iter([], ys) = ys
            | iter(e1::es, ys) = let
            val (lhs, DstIR.EINAPP(e, a)) = e1
            val y1 = FloatEin.transform (lhs, e, a)
            in
                iter(es, ys@y1)
            end
        val _ =  "about to split"
        val newbies =iter([newbie], [])
        (* val _ = (concat["\n -----> Pieces: ",Int.toString(length(newbies)),"\n"])*)
        (*val _ = prntNewbies(newbies, "\n\n\npost floatx1")*)
        val newbies =iter(newbies, [])
        val _ = prntNewbies(newbies, "\n\n\npost floatx2")
        (*val _ = ScanE.readIR_Split(newbies)*)
        val _ = (String.concat["Number of pieces:",Int.toString(length(newbies))])

        (* **************** expand-fem ************* *)

        val _ =   "\n\n\nbefore translating fields"
        val newbies  = List.foldr (fn (e,acc)=>  translateField.transform(e)@acc ) []  newbies
        (*val _ = prntNewbies(newbies, "\n\n\npost transform fields")*)
        val _ = ScanE.readIR_Split(newbies)
        (* ************** ProbeEIN *********** *)
      val _ = ("****about to call probe ein")
        (*   val _ = "about to call probe ein"*)
        val avail = AvailRHS.new()
        val _ = List.app  (ProbeEin.expand avail) newbies;
        val stmts = List.rev (AvailRHS.getAssignments avail)
        val _ = prntNewbies(stmts, "\n\n\npost probe ein ")
        val asgn = List.map DstIR.ASSGN stmts
        val _  = "exit"
        val _ = ("\n\n   ***********\n")
          in
            asgn
          end

  end (* HandleEin *)
