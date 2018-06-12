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
    structure H = Helper

    val flagmatch = true

  fun single_pass newbies = 
     let

        (* **************** split phase ************* *)
        fun iter([], ys) = ys
         | iter((lhs, DstIR.EINAPP(e, a))::es, ys) = let
                val y1 = FloatEin.transform (lhs, e, a)
                in
                    iter(es, ys@y1)
             end
          | iter(e1::es, ys) =  iter(es, ys@[e1])

        val newbies = iter(newbies, [])

        (*val _ = ScanE.readIR_Split(newbies)*)
        val _ = (String.concat["Number of pieces:",Int.toString(length(newbies))])

        (* **************** expand-fem ************* *)

        val newbies = List.foldr (fn (e,acc)=>  translateField.transform(e)@acc ) []  newbies
        val _ = H. multipleRhsToString("post translating fields", newbies)
        val _ = ScanE.readIR_Split(newbies)                
        in newbies end 
                
    fun expand (lhs, ein, args) = let
            val _ = ("\n\n   ***********\n")
            val _ = Helper.einToString("start expand", lhs, ein, args)
            val sliceFlag = Controls.get Ctl.sliceFlag
            val fullSplitFlag = Controls.get Ctl.fullSplitFlag
            val replaceFlag = Controls.get Ctl.replaceFlag
            (* ************** distribute and push Summation*********** *)
            val ein' = EinSums.transform ein
            val newbie = (lhs, DstIR.EINAPP(ein', args))
            (* ************** Scan and rewrite *********** *)
            val readIR = Controls.get Ctl.readEin
            val _ = if(readIR) then ScanE.readIR_Single(newbie,"tmp-High") else 1

            (*split and translate fields*)
            val newbies = single_pass([newbie])
            (*has to run multiple times in case a probe is embedded inside a cfexp*)
            val newbies = single_pass newbies
            val newbies = single_pass newbies
             val newbies = single_pass newbies
            (* ************** ProbeEIN *********** *)
            (*   val _ = "about to call probe ein"*)
            val avail = AvailRHS.new()
            val _ = List.app  (ProbeEin.expand avail) newbies;
            val stmts = List.rev (AvailRHS.getAssignments avail)
            (*val _ = H.prntNewbies(stmts, "\n\n\npost probe ein ")*)
            val asgn = List.map DstIR.ASSGN stmts
            (*val _ = H.prntNewbies(stmts, "\n\n\n marker: post probe")*)
            val _ = ("\n\n   ***********\n")
            in
                asgn
            end

  end (* HandleEin *)
